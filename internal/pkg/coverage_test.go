package pkg

import (
	"testing"

	"github.com/Borderliner/emacs-installer/internal/sys"
)

// allFamilies is every family the installer claims to support.
var allFamilies = []sys.Family{
	sys.FamilyDebian, sys.FamilyFedora, sys.FamilyArch, sys.FamilySUSE,
	sys.FamilyAlpine, sys.FamilyVoid, sys.FamilyGentoo, sys.FamilySolus,
	sys.FamilyMandriva, sys.FamilySlackware, sys.FamilyFreeBSD, sys.FamilyOpenBSD,
	sys.FamilyNetBSD, sys.FamilyMacOS,
}

// TestEveryFamilyHasManager makes sure each supported family maps to a package
// manager and resolves at least one concrete package for the base build deps.
func TestEveryFamilyHasManager(t *testing.T) {
	for _, f := range allFamilies {
		mgr, ok := ForFamily(f)
		if !ok {
			t.Errorf("%s: no package manager", f)
			continue
		}
		if mgr.Name == "" || mgr.install == nil {
			t.Errorf("%s: incomplete manager", f)
		}
		res := Resolve(mgr, f, []string{DepToolchain, DepGnuTLS, DepGtk3, DepTreesit})
		if len(res.Packages) == 0 {
			t.Errorf("%s: base deps resolved to nothing", f)
		}
	}
}
