package emacs

import (
	"strings"
	"testing"

	"github.com/Borderliner/emacs-installer/internal/sys"
)

func sampleManifest() Manifest {
	return Manifest{
		SchemaVersion: 1, OS: "linux", Prefix: "/opt/emacs", Version: "30.1",
		User: "u", Home: "/home/u", Symlink: true, Desktop: true,
		Daemon: true, ServiceInit: string(sys.InitSystemd),
	}
}

func TestUninstallScripts(t *testing.T) {
	u := UninstallConfig{Info: sys.Info{OS: "linux"}, Manifest: sampleManifest()}
	remove, cmds := u.teardownItems()
	system := u.systemScript(remove, cmds)
	user := u.userScript(remove, cmds)
	t.Logf("SYSTEM SCRIPT:\n%s\nUSER SCRIPT:\n%s", system, user)

	if !strings.Contains(system, "rm -rf '/opt/emacs'") {
		t.Error("system script does not remove the prefix")
	}
	if !strings.Contains(system, "/usr/local/bin/$t") {
		t.Error("system script does not clean up symlinks")
	}
	// systemd unit is user-scoped, so its disable lands in the user script.
	// (ShellJoin quotes each token, so match tokens individually.)
	if !strings.Contains(user, "systemctl") || !strings.Contains(user, "disable") {
		t.Error("user script does not disable the user daemon")
	}
	if !strings.Contains(user, "manifest.json") {
		t.Error("user script does not remove the manifest receipt")
	}
	if strings.Contains(user, ".cache/emacs-installer") {
		t.Error("source cache removed even though DeleteSource is false")
	}
}

func TestUninstallDeleteSource(t *testing.T) {
	u := UninstallConfig{Info: sys.Info{OS: "linux"}, Manifest: sampleManifest(), DeleteSource: true}
	remove, cmds := u.teardownItems()
	if !strings.Contains(u.userScript(remove, cmds), ".cache/emacs-installer") {
		t.Error("source cache not removed when DeleteSource is true")
	}
}

func TestUnsafePrefixGuard(t *testing.T) {
	for _, bad := range []string{"", "/", "/usr", "/home", "relative/path"} {
		u := UninstallConfig{Manifest: Manifest{Prefix: bad}}
		if got := u.prefix(); got != "/opt/emacs" {
			t.Errorf("prefix %q not guarded, got %q", bad, got)
		}
	}
	u := UninstallConfig{Manifest: Manifest{Prefix: "/opt/emacs-31"}}
	if got := u.prefix(); got != "/opt/emacs-31" {
		t.Errorf("valid prefix rejected: %q", got)
	}
}
