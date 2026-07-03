package pkg

import "github.com/Borderliner/emacs-installer/internal/sys"

// Resolution is the outcome of turning a set of logical dependencies into
// concrete package names for a given distribution.
type Resolution struct {
	Packages   []string // concrete packages to install (deduped, in request order)
	Unverified []string // packages whose repo availability could not be confirmed
}

// Resolve maps logical dependency keys (see the Dep* constants) to the concrete
// package names for fam, searching the repositories to choose valid candidates.
func Resolve(m *Manager, fam sys.Family, logical []string) Resolution {
	var res Resolution
	seen := map[string]bool{}
	for _, key := range logical {
		byFam, ok := catalog[key]
		if !ok {
			continue
		}
		// An absent or empty entry means this family needs nothing for the
		// dep (e.g. sound/dbus on macOS, or a system-provided library) — that
		// is normal, so we simply skip it.
		for _, req := range byFam[fam] {
			p, verified := m.pickAvailable(req)
			if p == "" || seen[p] {
				continue
			}
			seen[p] = true
			res.Packages = append(res.Packages, p)
			if !verified {
				res.Unverified = append(res.Unverified, p)
			}
		}
	}
	return res
}
