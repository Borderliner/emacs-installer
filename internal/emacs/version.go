// Package emacs models an Emacs build: which version to fetch, which configure
// flags to turn on, the dependencies those imply, and the compile/install
// pipeline that carries it out.
package emacs

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"regexp"
	"sort"
	"strconv"
	"time"
)

// Channel distinguishes a finished release from a development pretest.
type Channel string

const (
	Stable  Channel = "stable"
	Pretest Channel = "pretest"
)

// Version is one selectable Emacs release with everything needed to fetch it.
type Version struct {
	Number  string  // "30.1" or, for a pretest, "30.0.93"
	Channel Channel // Stable or Pretest
	URL     string  // full .tar.xz download URL
	Note    string  // short human note, e.g. "latest stable"
}

// Title / Desc feed the TUI list widget.
func (v Version) Title() string { return "Emacs " + v.Number }
func (v Version) Desc() string  { return v.Note }

// Dir is the top-level directory inside the tarball.
func (v Version) Dir() string { return "emacs-" + v.Number }

// Tarball is the archive filename.
func (v Version) Tarball() string { return "emacs-" + v.Number + ".tar.xz" }

const (
	stableIndex  = "https://ftp.gnu.org/gnu/emacs/"
	pretestIndex = "https://alpha.gnu.org/gnu/emacs/pretest/"
)

var (
	stableRe  = regexp.MustCompile(`emacs-(\d+)\.(\d+)\.tar\.xz`)
	pretestRe = regexp.MustCompile(`emacs-(\d+)\.(\d+)\.(\d+)\.tar\.xz`)
)

// Discover fetches the live version list: the newest pretest (alpha/beta) plus
// the last three major releases, each at its latest minor. If the network is
// unavailable it returns a sensible built-in fallback together with the error,
// so the wizard can still proceed offline.
func Discover(ctx context.Context) ([]Version, error) {
	var out []Version
	var firstErr error

	if v, err := latestPretest(ctx); err == nil && v.Number != "" {
		out = append(out, v)
	} else if err != nil {
		firstErr = err
	}

	if stables, err := latestStables(ctx, 3); err == nil {
		out = append(out, stables...)
	} else if firstErr == nil {
		firstErr = err
	}

	if len(out) == 0 {
		return Fallback(), firstErr
	}
	return out, firstErr
}

// latestStables returns the newest n major releases at their highest minor.
func latestStables(ctx context.Context, n int) ([]Version, error) {
	body, err := fetch(ctx, stableIndex)
	if err != nil {
		return nil, err
	}
	best := map[int]int{} // major -> highest minor seen
	for _, m := range stableRe.FindAllStringSubmatch(body, -1) {
		maj, min := atoi(m[1]), atoi(m[2])
		if cur, ok := best[maj]; !ok || min > cur {
			best[maj] = min
		}
	}
	majors := make([]int, 0, len(best))
	for maj := range best {
		majors = append(majors, maj)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(majors)))
	if len(majors) > n {
		majors = majors[:n]
	}

	var out []Version
	for i, maj := range majors {
		num := fmt.Sprintf("%d.%d", maj, best[maj])
		note := "previous stable release"
		if i == 0 {
			note = "latest stable release"
		}
		out = append(out, Version{
			Number:  num,
			Channel: Stable,
			URL:     stableIndex + "emacs-" + num + ".tar.xz",
			Note:    note,
		})
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("no stable releases found in index")
	}
	return out, nil
}

// latestPretest returns the highest three-component pretest release.
func latestPretest(ctx context.Context) (Version, error) {
	body, err := fetch(ctx, pretestIndex)
	if err != nil {
		return Version{}, err
	}
	var bMaj, bMin, bPatch int
	found := false
	for _, m := range pretestRe.FindAllStringSubmatch(body, -1) {
		maj, min, patch := atoi(m[1]), atoi(m[2]), atoi(m[3])
		if !found || greater(maj, min, patch, bMaj, bMin, bPatch) {
			bMaj, bMin, bPatch, found = maj, min, patch, true
		}
	}
	if !found {
		return Version{}, fmt.Errorf("no pretest releases found in index")
	}
	num := fmt.Sprintf("%d.%d.%d", bMaj, bMin, bPatch)
	return Version{
		Number:  num,
		Channel: Pretest,
		URL:     pretestIndex + "emacs-" + num + ".tar.xz",
		Note:    "latest pretest · alpha/beta",
	}, nil
}

// Fallback is the built-in list used when the mirrors cannot be reached.
func Fallback() []Version {
	return []Version{
		{Number: "30.0.93", Channel: Pretest, URL: pretestIndex + "emacs-30.0.93.tar.xz", Note: "pretest · alpha/beta (offline fallback)"},
		{Number: "30.1", Channel: Stable, URL: stableIndex + "emacs-30.1.tar.xz", Note: "latest stable release"},
		{Number: "29.4", Channel: Stable, URL: stableIndex + "emacs-29.4.tar.xz", Note: "previous stable release"},
		{Number: "28.2", Channel: Stable, URL: stableIndex + "emacs-28.2.tar.xz", Note: "previous stable release"},
	}
}

// greater reports whether version (a1,a2,a3) sorts above (b1,b2,b3).
func greater(a1, a2, a3, b1, b2, b3 int) bool {
	switch {
	case a1 != b1:
		return a1 > b1
	case a2 != b2:
		return a2 > b2
	default:
		return a3 > b3
	}
}

func atoi(s string) int { n, _ := strconv.Atoi(s); return n }

// fetch GETs a URL with a bounded timeout and returns the body as a string.
func fetch(ctx context.Context, url string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, 20*time.Second)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("User-Agent", "emacs-installer")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("GET %s: %s", url, resp.Status)
	}
	b, err := io.ReadAll(io.LimitReader(resp.Body, 8<<20))
	return string(b), err
}
