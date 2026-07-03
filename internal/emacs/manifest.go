package emacs

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/Borderliner/emacs-installer/internal/action"
	"github.com/Borderliner/emacs-installer/internal/desktop"
	"github.com/Borderliner/emacs-installer/internal/service"
	"github.com/Borderliner/emacs-installer/internal/sys"
)

const manifestSchema = 1

// Manifest is the receipt written after a successful install. It is compact on
// purpose: everything else (exact file paths, symlink names) is regenerated
// deterministically from these fields by the same generators the install used,
// so the uninstaller removes precisely what was created.
type Manifest struct {
	SchemaVersion int    `json:"schema_version"`
	CreatedAt     string `json:"created_at"`
	OS            string `json:"os"`
	Prefix        string `json:"prefix"`
	Version       string `json:"version"`
	User          string `json:"user"`
	Home          string `json:"home"`
	Symlink       bool   `json:"symlink"`
	Desktop       bool   `json:"desktop"`
	Daemon        bool   `json:"daemon"`
	ServiceInit   string `json:"service_init"`
}

// ManifestPath is where the receipt lives (per-user state).
func ManifestPath(home string) string {
	if home == "" {
		home = os.Getenv("HOME")
	}
	return filepath.Join(home, ".local/state/emacs-installer/manifest.json")
}

// manifest builds the receipt for a completed install.
func (c Config) manifest() Manifest {
	return Manifest{
		SchemaVersion: manifestSchema,
		CreatedAt:     time.Now().Format(time.RFC3339),
		OS:            c.Info.OS,
		Prefix:        c.Prefix,
		Version:       c.Version.Number,
		User:          c.Info.User,
		Home:          c.Info.Home,
		Symlink:       c.Symlink,
		Desktop:       c.Desktop && c.Info.OS == "linux",
		Daemon:        c.EnableDaemon,
		ServiceInit:   string(c.Init),
	}
}

// SaveManifest writes the receipt as JSON.
func SaveManifest(m Manifest) error {
	path := ManifestPath(m.Home)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	data, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, append(data, '\n'), 0o644)
}

// LoadManifest reads the receipt, reporting ok=false when none exists.
func LoadManifest(home string) (Manifest, bool) {
	data, err := os.ReadFile(ManifestPath(home))
	if err != nil {
		return Manifest{}, false
	}
	var m Manifest
	if json.Unmarshal(data, &m) != nil {
		return Manifest{}, false
	}
	return m, true
}

// DeriveManifest reconstructs a best-effort receipt when none was recorded
// (older installs, or a manifest that was deleted). Teardown is tolerant of
// files that turn out not to exist, so over-guessing is safe.
func DeriveManifest(info sys.Info, prefix string) Manifest {
	if prefix == "" {
		prefix = "/opt/emacs"
	}
	return Manifest{
		SchemaVersion: manifestSchema,
		OS:            info.OS,
		Prefix:        prefix,
		User:          info.User,
		Home:          info.Home,
		Symlink:       true,
		Desktop:       info.OS == "linux",
		Daemon:        true,
		ServiceInit:   string(info.Init),
	}
}

// InstallPresent reports whether there is something to uninstall.
func InstallPresent(m Manifest) bool {
	if m.Prefix == "" {
		return false
	}
	_, err := os.Stat(m.Prefix)
	return err == nil
}

// UninstallConfig drives an uninstall run.
type UninstallConfig struct {
	Info         sys.Info
	Manifest     Manifest
	DeleteSource bool
	DryRun       bool
}

// unsafePrefixes must never be handed to `rm -rf`, as a guard against a corrupt
// or malicious manifest.
var unsafePrefixes = map[string]bool{
	"": true, "/": true, "/usr": true, "/usr/local": true, "/bin": true,
	"/etc": true, "/opt": true, "/home": true, "/var": true, "/root": true,
	"/Applications": true, "/System": true,
}

func (u UninstallConfig) prefix() string {
	p := strings.TrimRight(u.Manifest.Prefix, "/")
	if p == "" || unsafePrefixes[p] || !strings.HasPrefix(p, "/") {
		return "/opt/emacs"
	}
	return p
}

func (u UninstallConfig) home() string {
	if u.Manifest.Home != "" {
		return u.Manifest.Home
	}
	return u.Info.Home
}

func (u UninstallConfig) osName() string {
	if u.Manifest.OS != "" {
		return u.Manifest.OS
	}
	return u.Info.OS
}

// teardownItems gathers the service + desktop files/commands to undo.
func (u UninstallConfig) teardownItems() (remove []action.FileSpec, cmds []action.Command) {
	m := u.Manifest
	if m.Daemon {
		r, c := service.Teardown(service.Params{
			Init:     sys.InitSystem(m.ServiceInit),
			User:     m.User,
			Home:     u.home(),
			EmacsBin: filepath.Join(u.prefix(), "bin/emacs"),
		})
		remove = append(remove, r...)
		cmds = append(cmds, c...)
	}
	if m.Desktop && u.osName() == "linux" {
		remove = append(remove, desktop.Generate(u.prefix(), true, u.home())...)
	}
	return remove, cmds
}

// BuildPhases assembles the uninstall pipeline: a user-scoped cleanup followed
// by a privileged one that removes the prefix and system files.
func (u UninstallConfig) BuildPhases() []Phase {
	remove, cmds := u.teardownItems()
	dummy := Config{DryRun: u.DryRun}
	return []Phase{
		dummy.shellPhase("Remove user files & source", u.userScript(remove, cmds)),
		dummy.privilegedPhase("Remove "+u.prefix()+" & system files", u.systemScript(remove, cmds)),
	}
}

// userScript is a tolerant program (run unprivileged) that stops any user
// daemon, deletes user-scoped files, drops the manifest, and — if requested —
// the downloaded source.
func (u UninstallConfig) userScript(remove []action.FileSpec, cmds []action.Command) string {
	var b strings.Builder
	for _, c := range cmds {
		if !c.System && len(c.Argv) > 0 {
			b.WriteString(action.ShellJoin(c.Argv) + " || true\n")
		}
	}
	for _, s := range remove {
		if !s.System {
			fmt.Fprintf(&b, "rm -rf %s\n", action.ShellQuote(s.Path))
		}
	}
	fmt.Fprintf(&b, "rm -f %s\n", action.ShellQuote(ManifestPath(u.home())))
	if u.DeleteSource {
		src := filepath.Join(u.home(), ".cache", "emacs-installer")
		fmt.Fprintf(&b, "echo removing source cache %s\n", action.ShellQuote(src))
		fmt.Fprintf(&b, "rm -rf %s\n", action.ShellQuote(src))
	} else {
		b.WriteString("echo keeping downloaded source cache\n")
	}
	return b.String()
}

// systemScript is a tolerant root program that stops any system daemon, removes
// the symlinks, system files, the install prefix, and refreshes desktop caches.
func (u UninstallConfig) systemScript(remove []action.FileSpec, cmds []action.Command) string {
	prefix := u.prefix()
	var b strings.Builder
	for _, c := range cmds {
		if c.System && len(c.Argv) > 0 {
			b.WriteString(action.ShellJoin(c.Argv) + " || true\n")
		}
	}
	for _, s := range remove {
		if s.System {
			fmt.Fprintf(&b, "rm -rf %s\n", action.ShellQuote(s.Path))
		}
	}
	if u.Manifest.Symlink {
		// Only remove links we actually own (symlinks pointing back into prefix).
		fmt.Fprintf(&b, "for t in emacs emacsclient etags ebrowse ctags; do l=/usr/local/bin/$t; "+
			"if [ -L \"$l\" ] && [ \"$(readlink \"$l\")\" = %s/bin/$t ]; then rm -f \"$l\"; fi; done\n", prefix)
	}
	fmt.Fprintf(&b, "echo removing %s\n", action.ShellQuote(prefix))
	fmt.Fprintf(&b, "rm -rf %s\n", action.ShellQuote(prefix))
	if u.osName() == "darwin" {
		b.WriteString("rm -rf /Applications/Emacs.app\n")
	}
	if u.osName() == "linux" {
		b.WriteString("command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database /usr/share/applications 2>/dev/null || true\n")
		b.WriteString("command -v gtk-update-icon-cache >/dev/null 2>&1 && gtk-update-icon-cache -f /usr/share/icons/hicolor 2>/dev/null || true\n")
	}
	return b.String()
}

// manifestPhase records the receipt at the end of a successful install.
func (c Config) manifestPhase() Phase {
	title := "Record installation"
	if c.DryRun {
		return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
			ch <- Progress{Line: "[dry-run] write " + ManifestPath(c.Info.Home), Fraction: 1}
			return nil
		}}
	}
	m := c.manifest()
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		if err := SaveManifest(m); err != nil {
			return err
		}
		ch <- Progress{Line: "recorded manifest at " + ManifestPath(c.Info.Home), Fraction: 1}
		return nil
	}}
}
