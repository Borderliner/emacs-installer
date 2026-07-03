package tui

import (
	"fmt"
	"regexp"
	"testing"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/Borderliner/emacs-installer/internal/emacs"
	"github.com/Borderliner/emacs-installer/internal/pkg"
)

var ansiRe = regexp.MustCompile(`\x1b\[[0-9;]*[A-Za-z]`)

func strip(s string) string { return ansiRe.ReplaceAllString(s, "") }

// TestRenderScreens prints each screen so the layout can be eyeballed with
// `go test ./internal/tui -run RenderScreens -v`.
func TestRenderScreens(t *testing.T) {
	m := New(Options{DryRun: true})
	m, _ = up(m, tea.WindowSizeMsg{Width: 92, Height: 34})
	m, _ = up(m, versionsLoadedMsg{versions: emacs.Fallback()})

	show := func(name string) { fmt.Printf("\n===== %s =====\n%s\n", name, strip(m.View())) }

	show("WELCOME")
	m, _ = up(m, key("enter"))
	show("VERSION")
	m, _ = up(m, key("enter"))
	show("FEATURES")
	m, _ = up(m, key("enter")) // -> deps
	m, _ = up(m, depsResolvedMsg{mgr: m.pkgManager, res: pkg.Resolution{
		Packages:   []string{"build-essential", "libgtk-3-dev", "libgccjit-14-dev", "libtree-sitter-dev", "libgnutls28-dev", "texinfo"},
		Unverified: []string{"libgccjit-14-dev"},
	}})
	show("DEPENDENCIES")
	m, _ = up(m, key("enter"))
	show("SERVICE")
	m, _ = up(m, key("enter"))
	show("REVIEW")

	// Into the live install; pump a few events, snapshot mid-run, then finish.
	var cmd tea.Cmd
	m, cmd = up(m, key("enter"))
	for i := 0; i < 6 && cmd != nil && m.step == stepInstall; i++ {
		m, cmd = up(m, cmd())
	}
	show("INSTALL (mid-run)")
	for i := 0; cmd != nil && m.step == stepInstall && i < 20000; i++ {
		m, cmd = up(m, cmd())
	}
	show("DONE")
}

func up(m Model, msg tea.Msg) (Model, tea.Cmd) {
	n, c := m.Update(msg)
	return n.(Model), c
}
