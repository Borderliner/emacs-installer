package emacs

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// Progress is one update from a running phase: a log line and/or a fractional
// completion for the current phase (Fraction < 0 means "indeterminate").
type Progress struct {
	Line     string
	Fraction float64
}

// Phase is a single unit of the install pipeline. Exactly one of Script (a root
// shell program, run by the TUI via a suspended-terminal exec so sudo/doas can
// prompt) or Run (an in-process action that streams Progress) is set.
type Phase struct {
	Title      string
	Privileged bool
	Script     string
	Run        func(ctx context.Context, ch chan<- Progress) error
}

// streamExec runs a command and streams its merged stdout+stderr line by line.
func streamExec(ctx context.Context, ch chan<- Progress, dir string, extraEnv []string, name string, args ...string) error {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Dir = dir
	if len(extraEnv) > 0 {
		cmd.Env = append(os.Environ(), extraEnv...)
	}

	pr, pw := io.Pipe()
	cmd.Stdout = pw
	cmd.Stderr = pw

	done := make(chan struct{})
	go func() {
		sc := bufio.NewScanner(pr)
		sc.Buffer(make([]byte, 0, 64*1024), 4*1024*1024)
		for sc.Scan() {
			ch <- Progress{Line: sc.Text(), Fraction: -1}
		}
		close(done)
	}()

	if err := cmd.Start(); err != nil {
		pw.Close()
		<-done
		return err
	}
	err := cmd.Wait()
	pw.Close()
	<-done
	return err
}

// downloadFile fetches url into dest, emitting real byte-percentage progress.
// It downloads to a .part file and renames on success so partial files are
// never mistaken for complete ones.
func downloadFile(ctx context.Context, ch chan<- Progress, url, dest string) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return err
	}
	req.Header.Set("User-Agent", "emacs-installer")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("GET %s: %s", url, resp.Status)
	}

	if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
		return err
	}
	part := dest + ".part"
	f, err := os.Create(part)
	if err != nil {
		return err
	}

	total := resp.ContentLength
	var read int64
	buf := make([]byte, 128*1024)
	lastEmit := time.Now()
	ch <- Progress{Line: "downloading " + filepath.Base(dest), Fraction: 0}
	for {
		n, rerr := resp.Body.Read(buf)
		if n > 0 {
			if _, werr := f.Write(buf[:n]); werr != nil {
				f.Close()
				return werr
			}
			read += int64(n)
			if total > 0 && time.Since(lastEmit) > 60*time.Millisecond {
				ch <- Progress{Fraction: float64(read) / float64(total)}
				lastEmit = time.Now()
			}
		}
		if rerr == io.EOF {
			break
		}
		if rerr != nil {
			f.Close()
			return rerr
		}
		if err := ctx.Err(); err != nil {
			f.Close()
			return err
		}
	}
	if err := f.Close(); err != nil {
		return err
	}
	ch <- Progress{Fraction: 1, Line: fmt.Sprintf("downloaded %.1f MiB", float64(read)/(1<<20))}
	return os.Rename(part, dest)
}
