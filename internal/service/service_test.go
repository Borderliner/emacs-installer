package service

import (
	"testing"

	"github.com/Borderliner/emacs-installer/internal/sys"
)

func TestGenerateAndTeardownAllInits(t *testing.T) {
	inits := []sys.InitSystem{
		sys.InitSystemd, sys.InitOpenRC, sys.InitRunit, sys.InitSysV, sys.InitDinit,
		sys.InitSlackware, sys.InitFreeBSD, sys.InitOpenBSD, sys.InitNetBSD, sys.InitLaunchd,
	}
	for _, in := range inits {
		p := Params{Init: in, User: "u", Home: "/home/u", EmacsBin: "/opt/emacs/bin/emacs"}

		specs, cmds, err := Generate(p)
		if err != nil {
			t.Errorf("%s: Generate error: %v", in, err)
			continue
		}
		if len(specs) == 0 && len(cmds) == 0 {
			t.Errorf("%s: Generate produced nothing", in)
		}
		if s := Summary(p); s == "" || s == string(in) {
			t.Errorf("%s: missing Summary", in)
		}

		rem, tcmds := Teardown(p)
		if len(rem) == 0 && len(tcmds) == 0 {
			t.Errorf("%s: Teardown produced nothing", in)
		}
	}
}
