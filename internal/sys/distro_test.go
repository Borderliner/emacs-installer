package sys

import "testing"

func TestClassify(t *testing.T) {
	cases := map[string]Family{
		// existing
		"ubuntu": FamilyDebian, "debian": FamilyDebian, "almalinux": FamilyFedora,
		"rocky": FamilyFedora, "rhel": FamilyFedora, "opensuse-tumbleweed": FamilySUSE,
		"opensuse-slowroll": FamilySUSE, "opensuse-leap": FamilySUSE, "arch": FamilyArch,
		"artix": FamilyArch, "void": FamilyVoid, "gentoo": FamilyGentoo,
		// newly added
		"solus": FamilySolus, "kaos": FamilyArch, "openmandriva": FamilyMandriva,
		"slackware": FamilySlackware, "freebsd": FamilyFreeBSD, "openbsd": FamilyOpenBSD,
		"netbsd": FamilyNetBSD,
	}
	for id, want := range cases {
		if got := classify(Distro{ID: id}); got != want {
			t.Errorf("classify(%q) = %q, want %q", id, got, want)
		}
	}
}

func TestClassifyIDLike(t *testing.T) {
	// A derivative not in the table falls back to its ID_LIKE chain.
	d := Distro{ID: "someubuntuderivative", IDLike: []string{"ubuntu", "debian"}}
	if got := classify(d); got != FamilyDebian {
		t.Errorf("ID_LIKE classify = %q, want debian", got)
	}
}
