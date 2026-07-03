package sys

import (
	"bufio"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

// Family groups distributions that share a package manager and package naming.
// Everything downstream (dependency resolution, service files) keys off this
// rather than the exact distro ID, so a new derivative usually works for free.
type Family string

const (
	FamilyDebian    Family = "debian"    // apt: Debian, Ubuntu, Mint, Pop!_OS, ...
	FamilyFedora    Family = "fedora"    // dnf: Fedora, RHEL, Rocky, Alma, CentOS
	FamilyArch      Family = "arch"      // pacman: Arch, Manjaro, EndeavourOS, Artix, KaOS
	FamilySUSE      Family = "suse"      // zypper: openSUSE, SLES
	FamilyAlpine    Family = "alpine"    // apk
	FamilyVoid      Family = "void"      // xbps
	FamilyGentoo    Family = "gentoo"    // portage/emerge
	FamilySolus     Family = "solus"     // eopkg
	FamilyMandriva  Family = "mandriva"  // dnf/rpm (OpenMandriva, lib64 naming)
	FamilySlackware Family = "slackware" // slackpkg
	FamilyFreeBSD   Family = "freebsd"   // pkg
	FamilyOpenBSD   Family = "openbsd"   // pkg_add
	FamilyNetBSD    Family = "netbsd"    // pkgin
	FamilyMacOS     Family = "macos"     // homebrew
	FamilyUnknown   Family = "unknown"
)

// Distro is the parsed contents of /etc/os-release, plus the Family we mapped
// it into.
type Distro struct {
	ID      string   // os-release ID, e.g. "ubuntu"
	IDLike  []string // os-release ID_LIKE, e.g. ["debian"]
	Version string   // VERSION_ID, e.g. "24.04"
	Name    string   // PRETTY_NAME, e.g. "Ubuntu 24.04.1 LTS"
	Family  Family
}

// idToFamily maps a concrete os-release ID to its Family. Derivatives that are
// not listed here fall back to their ID_LIKE chain.
var idToFamily = map[string]Family{
	// Debian / apt
	"debian": FamilyDebian, "ubuntu": FamilyDebian, "linuxmint": FamilyDebian,
	"pop": FamilyDebian, "elementary": FamilyDebian, "zorin": FamilyDebian,
	"kali": FamilyDebian, "raspbian": FamilyDebian, "devuan": FamilyDebian,
	"mx": FamilyDebian, "deepin": FamilyDebian, "linuxmint-debian": FamilyDebian,
	// Fedora / RHEL / dnf
	"fedora": FamilyFedora, "rhel": FamilyFedora, "centos": FamilyFedora,
	"rocky": FamilyFedora, "almalinux": FamilyFedora, "ol": FamilyFedora,
	"amzn": FamilyFedora, "nobara": FamilyFedora, "scientific": FamilyFedora,
	// Arch / pacman (KaOS is independent but pacman-based)
	"arch": FamilyArch, "manjaro": FamilyArch, "endeavouros": FamilyArch,
	"artix": FamilyArch, "cachyos": FamilyArch, "garuda": FamilyArch,
	"arcolinux": FamilyArch, "kaos": FamilyArch,
	// SUSE / zypper
	"opensuse": FamilySUSE, "opensuse-leap": FamilySUSE,
	"opensuse-tumbleweed": FamilySUSE, "opensuse-slowroll": FamilySUSE,
	"sles": FamilySUSE, "sled": FamilySUSE, "suse": FamilySUSE,
	// OpenMandriva (rpm + dnf, but lib64-style package names)
	"openmandriva": FamilyMandriva, "openmandriva-rome": FamilyMandriva,
	"openmandriva-rock": FamilyMandriva,
	// Others
	"solus":     FamilySolus,
	"slackware": FamilySlackware,
	"alpine":    FamilyAlpine, "postmarketos": FamilyAlpine,
	"void":   FamilyVoid,
	"gentoo": FamilyGentoo, "funtoo": FamilyGentoo,
	// BSDs (usually detected by GOOS, but map their os-release IDs too)
	"freebsd": FamilyFreeBSD, "openbsd": FamilyOpenBSD, "netbsd": FamilyNetBSD,
}

// DetectDistro reads /etc/os-release (or reports macOS/BSD) and classifies the
// host.
func DetectDistro() Distro {
	switch runtime.GOOS {
	case "darwin":
		return Distro{ID: "macos", Name: macOSName(), Family: FamilyMacOS}
	case "freebsd", "openbsd", "netbsd":
		return detectBSD()
	}
	d := parseOSRelease("/etc/os-release")
	if d.ID == "" {
		d = parseOSRelease("/usr/lib/os-release")
	}
	d.Family = classify(d)
	return d
}

// detectBSD classifies a BSD by GOOS. FreeBSD and NetBSD ship /etc/os-release
// (used for a nicer name); OpenBSD does not, so we fall back to uname.
func detectBSD() Distro {
	d := parseOSRelease("/etc/os-release")
	switch runtime.GOOS {
	case "freebsd":
		d.Family = FamilyFreeBSD
	case "openbsd":
		d.Family = FamilyOpenBSD
	case "netbsd":
		d.Family = FamilyNetBSD
	}
	if d.ID == "" {
		d.ID = runtime.GOOS
	}
	if d.Name == "" {
		d.Name = unameName(runtime.GOOS)
	}
	return d
}

// unameName returns "OpenBSD 7.5"-style label, falling back to the GOOS.
func unameName(fallback string) string {
	if out, err := exec.Command("uname", "-sr").Output(); err == nil {
		if s := strings.TrimSpace(string(out)); s != "" {
			return s
		}
	}
	return fallback
}

// classify resolves a Distro to a Family, first by exact ID then by walking the
// ID_LIKE chain (so "linuxmint" with ID_LIKE=ubuntu still lands on Debian).
func classify(d Distro) Family {
	if f, ok := idToFamily[strings.ToLower(d.ID)]; ok {
		return f
	}
	for _, like := range d.IDLike {
		if f, ok := idToFamily[strings.ToLower(like)]; ok {
			return f
		}
		// ID_LIKE may itself be a family keyword.
		switch strings.ToLower(like) {
		case "debian":
			return FamilyDebian
		case "fedora", "rhel", "centos":
			return FamilyFedora
		case "arch":
			return FamilyArch
		case "suse", "opensuse":
			return FamilySUSE
		}
	}
	return FamilyUnknown
}

// parseOSRelease reads a KEY=value os-release file, stripping shell quoting.
func parseOSRelease(path string) Distro {
	f, err := os.Open(path)
	if err != nil {
		return Distro{}
	}
	defer f.Close()

	var d Distro
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		key, val, ok := strings.Cut(line, "=")
		if !ok {
			continue
		}
		val = strings.Trim(val, `"'`)
		switch key {
		case "ID":
			d.ID = val
		case "ID_LIKE":
			d.IDLike = strings.Fields(val)
		case "VERSION_ID":
			d.Version = val
		case "PRETTY_NAME":
			d.Name = val
		}
	}
	if d.Name == "" {
		d.Name = d.ID
	}
	return d
}

// macOSName returns a friendly label for the running macOS release.
func macOSName() string {
	if v := os.Getenv("MACOSX_DEPLOYMENT_TARGET"); v != "" {
		return "macOS " + v
	}
	return "macOS"
}
