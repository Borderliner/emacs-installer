// Package pkg abstracts the various Linux/macOS package managers behind one
// small interface, and maps Emacs' build dependencies onto the concrete package
// names each distribution actually ships. Because those names drift between
// releases (libgccjit-13-dev vs libgccjit-14-dev, webkit2gtk-4.0 vs 4.1, ...),
// the resolver *searches the repositories* and picks the first candidate that
// really exists before anything is installed.
package pkg

import (
	"io"
	"os/exec"
	"strings"

	"github.com/Borderliner/emacs-installer/internal/sys"
)

// Req is one dependency requirement, satisfied by the first of its alternative
// package names that the package manager reports as available.
type Req []string

// Manager talks to a single distribution's package tool.
type Manager struct {
	Name     string // "apt", "dnf", "pacman", ...
	Escalate bool   // whether `install` must be run as root (false for brew)

	install   func(pkgs []string) string // shell snippet to install packages
	refresh   func() string              // shell snippet to refresh repo metadata ("" if none)
	available func(pkg string) bool      // repo availability probe (nil = assume available)

	cache map[string]bool
}

// ForFamily returns the package manager for a distribution family.
func ForFamily(f sys.Family) (*Manager, bool) {
	m, ok := managers[f]
	return m, ok
}

// Available reports whether a package name resolves in the configured repos.
// Results are memoised so repeated resolution passes stay cheap.
func (m *Manager) Available(pkg string) bool {
	if m.available == nil {
		return true // manager cannot cheaply search (e.g. portage): trust the catalog
	}
	if m.cache == nil {
		m.cache = map[string]bool{}
	}
	if v, ok := m.cache[pkg]; ok {
		return v
	}
	v := m.available(pkg)
	m.cache[pkg] = v
	return v
}

// RefreshScript is the (possibly empty) command that updates repo metadata.
func (m *Manager) RefreshScript() string {
	if m.refresh == nil {
		return ""
	}
	return m.refresh()
}

// InstallScript is a complete shell program that refreshes metadata (if needed)
// and installs pkgs. It is meant to be handed to sys.Escalation.WrapShell.
func (m *Manager) InstallScript(pkgs []string) string {
	var b strings.Builder
	b.WriteString("set -e\n")
	if r := m.RefreshScript(); r != "" {
		b.WriteString(r)
		b.WriteByte('\n')
	}
	b.WriteString(m.install(pkgs))
	b.WriteByte('\n')
	return b.String()
}

// pickAvailable returns the first available candidate in a Req. If none can be
// confirmed it falls back to the first candidate and reports verified=false so
// the UI can warn that the name could not be validated.
func (m *Manager) pickAvailable(req Req) (pkg string, verified bool) {
	if len(req) == 0 {
		return "", false
	}
	for _, c := range req {
		if m.Available(c) {
			return c, true
		}
	}
	return req[0], false
}

// exitOK runs a command discarding output and reports a zero exit status.
func exitOK(name string, args ...string) bool {
	cmd := exec.Command(name, args...)
	cmd.Stdout, cmd.Stderr = io.Discard, io.Discard
	return cmd.Run() == nil
}

// outNonEmpty runs a command and reports whether it printed anything.
func outNonEmpty(name string, args ...string) bool {
	out, err := exec.Command(name, args...).Output()
	return err == nil && len(strings.TrimSpace(string(out))) > 0
}

func join(pkgs []string) string { return strings.Join(pkgs, " ") }

// managers wires each family to its concrete package tool. Availability probes
// are unprivileged and side-effect free; install/refresh scripts run as root
// (except brew, which refuses to run as root).
var managers = map[sys.Family]*Manager{
	sys.FamilyDebian: {
		Name: "apt", Escalate: true,
		refresh: func() string { return "apt-get update -qq" },
		install: func(p []string) string {
			return "DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends " + join(p)
		},
		available: func(pkg string) bool { return exitOK("apt-cache", "show", pkg) },
	},
	sys.FamilyFedora: {
		Name: "dnf", Escalate: true,
		install:   func(p []string) string { return "dnf install -y " + join(p) },
		available: func(pkg string) bool { return exitOK("sh", "-c", "dnf -q info "+shellQuote(pkg)+" >/dev/null 2>&1") },
	},
	sys.FamilyArch: {
		Name: "pacman", Escalate: true,
		install:   func(p []string) string { return "pacman -S --needed --noconfirm " + join(p) },
		available: func(pkg string) bool { return exitOK("pacman", "-Si", pkg) },
	},
	sys.FamilySUSE: {
		Name: "zypper", Escalate: true,
		refresh: func() string { return "zypper --non-interactive refresh" },
		install: func(p []string) string { return "zypper --non-interactive install --no-recommends " + join(p) },
		available: func(pkg string) bool {
			return exitOK("zypper", "--non-interactive", "search", "--match-exact", "--type", "package", pkg)
		},
	},
	sys.FamilyAlpine: {
		Name: "apk", Escalate: true,
		refresh:   func() string { return "apk update" },
		install:   func(p []string) string { return "apk add " + join(p) },
		available: func(pkg string) bool { return outNonEmpty("apk", "search", "-x", pkg) },
	},
	sys.FamilyVoid: {
		Name: "xbps", Escalate: true,
		install:   func(p []string) string { return "xbps-install -Sy " + join(p) },
		available: func(pkg string) bool { return exitOK("xbps-query", "-R", pkg) },
	},
	sys.FamilyGentoo: {
		Name: "emerge", Escalate: true,
		install: func(p []string) string { return "emerge --quiet --noreplace " + join(p) },
		// portage atom resolution is expensive to probe cheaply; trust the catalog.
	},
	sys.FamilyMacOS: {
		Name: "brew", Escalate: false,
		install:   func(p []string) string { return "brew install " + join(p) },
		available: func(pkg string) bool { return exitOK("brew", "info", "--formula", pkg) },
	},
}

// shellQuote single-quotes a string for safe embedding in an sh -c program.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}
