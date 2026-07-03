package tui

import (
	"fmt"
	"testing"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/Borderliner/emacs-installer/internal/emacs"
)

// TestUninstallDryRun injects a manifest pointing at a real (temp) prefix so the
// uninstall flow has something to remove, then drives it to completion in
// dry-run and prints the confirm + done screens.
func TestUninstallDryRun(t *testing.T) {
	tmp := t.TempDir()
	m := New(Options{DryRun: true, Uninstall: true})
	m.manifest = emacs.Manifest{
		SchemaVersion: 1, OS: "linux", Prefix: tmp, Version: "30.1",
		User: "tester", Home: tmp, Symlink: true, Desktop: true,
		Daemon: true, ServiceInit: "systemd",
	}
	m.manifestOK = true
	m, _ = up(m, tea.WindowSizeMsg{Width: 92, Height: 34})

	if m.step != stepUninstall {
		t.Fatalf("expected uninstall step, got %d", m.step)
	}
	fmt.Printf("\n===== UNINSTALL CONFIRM =====\n%s\n", strip(m.View()))

	m, _ = up(m, key("x")) // toggle "also delete source"
	if !m.deleteSource {
		t.Fatal("delete-source toggle did not flip")
	}

	var cmd tea.Cmd
	m, cmd = up(m, key("enter"))
	if m.step != stepInstall {
		t.Fatalf("expected running step, got %d", m.step)
	}
	for i := 0; cmd != nil && m.step == stepInstall && i < 20000; i++ {
		m, cmd = up(m, cmd())
	}
	if m.step != stepDone {
		t.Fatalf("expected done, got %d (err %v)", m.step, m.installErr)
	}
	fmt.Printf("\n===== UNINSTALL DONE =====\n%s\n", strip(m.View()))
}
