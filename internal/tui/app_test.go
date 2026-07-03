package tui

import (
	"testing"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/hajianpour/emacs-installer/internal/emacs"
	"github.com/hajianpour/emacs-installer/internal/pkg"
)

func key(s string) tea.KeyMsg {
	switch s {
	case "enter":
		return tea.KeyMsg{Type: tea.KeyEnter}
	case "down":
		return tea.KeyMsg{Type: tea.KeyDown}
	case "up":
		return tea.KeyMsg{Type: tea.KeyUp}
	default:
		return tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(s)}
	}
}

// send applies a message and returns the updated model, asserting View() never
// panics and always renders something.
func send(t *testing.T, m Model, msg tea.Msg) (Model, tea.Cmd) {
	t.Helper()
	next, cmd := m.Update(msg)
	nm := next.(Model)
	if nm.View() == "" && !nm.quitting {
		t.Fatalf("empty view at step %d", nm.step)
	}
	return nm, cmd
}

// TestDryRunWalkthrough drives the entire wizard headlessly in dry-run mode and
// asserts it reaches the Done screen without executing anything real.
func TestDryRunWalkthrough(t *testing.T) {
	m := New(Options{DryRun: true})

	m, _ = send(t, m, tea.WindowSizeMsg{Width: 100, Height: 34})
	m, _ = send(t, m, versionsLoadedMsg{versions: emacs.Fallback()})

	// Welcome -> Version
	m, _ = send(t, m, key("enter"))
	if m.step != stepVersion {
		t.Fatalf("expected version step, got %d", m.step)
	}

	// Version -> Features (select the top entry)
	m, _ = send(t, m, key("enter"))
	if m.step != stepFlags {
		t.Fatalf("expected flags step, got %d", m.step)
	}
	if len(m.toolkits) == 0 || len(m.toggles) == 0 {
		t.Fatalf("features not populated: %d toolkits, %d toggles", len(m.toolkits), len(m.toggles))
	}

	// Toggle a couple of features, then continue.
	m, _ = send(t, m, key("down"))
	m, _ = send(t, m, key("x"))
	m, _ = send(t, m, key("enter")) // -> Deps (fires resolveDeps cmd, which we skip)
	if m.step != stepDeps {
		t.Fatalf("expected deps step, got %d", m.step)
	}
	if len(m.configureArgs) == 0 {
		t.Fatalf("configure args not assembled")
	}

	// Inject a resolution instead of hitting real repositories.
	m, _ = send(t, m, depsResolvedMsg{mgr: m.pkgManager, res: pkg.Resolution{Packages: []string{"gcc", "make"}}})
	m, _ = send(t, m, key("enter")) // -> Service
	if m.step != stepService {
		t.Fatalf("expected service step, got %d", m.step)
	}

	// Service -> Review
	m, _ = send(t, m, key("enter"))
	if m.step != stepReview {
		t.Fatalf("expected review step, got %d", m.step)
	}

	// Review -> Install (starts the dry-run pipeline)
	var cmd tea.Cmd
	m, cmd = send(t, m, key("enter"))
	if m.step != stepInstall {
		t.Fatalf("expected install step, got %d", m.step)
	}
	if len(m.phases) == 0 {
		t.Fatalf("no phases built")
	}

	// Drive the pipeline to completion.
	for i := 0; cmd != nil && m.step == stepInstall; i++ {
		if i > 20000 {
			t.Fatalf("pipeline did not terminate")
		}
		msg := cmd()
		m, cmd = send(t, m, msg)
	}

	if m.step != stepDone {
		t.Fatalf("expected done step, got %d (err: %v)", m.step, m.installErr)
	}
}
