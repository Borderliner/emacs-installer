package emacs

import (
	"strings"
	"testing"

	"github.com/Borderliner/emacs-installer/internal/sys"
)

func cfgFor(osName string, init sys.InitSystem) Config {
	return Config{
		Info:         sys.Info{OS: osName, User: "u", Home: "/home/u", Cores: 4},
		Version:      Version{Number: "30.1", URL: "https://ftp.gnu.org/gnu/emacs/emacs-30.1.tar.xz"},
		Prefix:       "/opt/emacs",
		Symlink:      true,
		Desktop:      true,
		EnableDaemon: true,
		Init:         init,
	}
}

func TestFreeBSDPlan(t *testing.T) {
	c := cfgFor("freebsd", sys.InitFreeBSD)
	if c.makeProgram() != "gmake" {
		t.Errorf("FreeBSD makeProgram = %q, want gmake", c.makeProgram())
	}
	if c.archiveName() != "emacs-30.1.tar.xz" {
		t.Errorf("FreeBSD archive = %q", c.archiveName())
	}
	s := c.installScript("/src")
	for _, want := range []string{"gmake -C", "/usr/local/etc/rc.d/emacs", "/usr/local/share/applications", "sysrc"} {
		if !strings.Contains(s, want) {
			t.Errorf("FreeBSD install script missing %q", want)
		}
	}
	t.Logf("FreeBSD install script:\n%s", s)
}

func TestOpenBSDUsesGzip(t *testing.T) {
	c := cfgFor("openbsd", sys.InitOpenBSD)
	if !c.useGzip() {
		t.Error("OpenBSD should use the .gz release")
	}
	if c.archiveName() != "emacs-30.1.tar.gz" {
		t.Errorf("OpenBSD archive = %q, want emacs-30.1.tar.gz", c.archiveName())
	}
	if got := c.archiveURL(); got != "https://ftp.gnu.org/gnu/emacs/emacs-30.1.tar.gz" {
		t.Errorf("OpenBSD url = %q", got)
	}
	if c.makeProgram() != "gmake" {
		t.Error("OpenBSD should use gmake")
	}
}

func TestBSDDefaultsNativeCompOff(t *testing.T) {
	for _, os := range []string{"freebsd", "openbsd", "netbsd"} {
		sel := DefaultSelection(Env{OS: os, EmacsMajor: 30})
		if sel.Enabled[keyNativeComp] {
			t.Errorf("%s: native compilation should default off", os)
		}
	}
	if !DefaultSelection(Env{OS: "linux", EmacsMajor: 30}).Enabled[keyNativeComp] {
		t.Error("linux: native compilation should default on")
	}
}
