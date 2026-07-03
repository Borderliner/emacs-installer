// Package sys inspects the host: which distribution and init system are in
// use, whether we can escalate privileges, how many CPU threads we have, and
// where to stage the build. Everything here is read-only detection with no
// side effects, so callers can probe the system freely.
package sys

import (
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
)

// Info is a snapshot of everything the installer needs to know about the host.
type Info struct {
	OS         string // runtime.GOOS: "linux" or "darwin"
	Distro     Distro
	Init       InitSystem
	Escalation Escalation
	Cores      int
	Root       bool
	User       string // login name of the invoking (non-root, if via sudo) user
	Home       string // that user's home directory
}

// Detect gathers a full snapshot of the host. prefer optionally forces a
// privilege-escalation tool ("sudo" or "doas"); pass "" to auto-detect.
func Detect(prefer string) Info {
	return Info{
		OS:         runtime.GOOS,
		Distro:     DetectDistro(),
		Init:       DetectInit(),
		Escalation: DetectEscalation(prefer),
		Cores:      Cores(),
		Root:       IsRoot(),
		User:       invokingUser(),
		Home:       invokingHome(),
	}
}

// Cores is the number of CPU threads to hand to `make -j`.
func Cores() int {
	if n := runtime.NumCPU(); n > 0 {
		return n
	}
	return 1
}

// IsRoot reports whether we run with effective uid 0.
func IsRoot() bool { return os.Geteuid() == 0 }

// CacheDir is where source tarballs are downloaded and unpacked. It always
// resolves to the invoking user's cache, even when the installer is launched
// through sudo, so build artifacts stay user-owned.
func CacheDir() string {
	if home := invokingHome(); home != "" {
		return filepath.Join(home, ".cache", "emacs-installer")
	}
	if d, err := os.UserCacheDir(); err == nil {
		return filepath.Join(d, "emacs-installer")
	}
	return filepath.Join(os.TempDir(), "emacs-installer")
}

// invokingUser returns the human running the installer. Under sudo/doas we
// prefer the original user recorded in the environment over "root".
func invokingUser() string {
	for _, env := range []string{"SUDO_USER", "DOAS_USER"} {
		if v := os.Getenv(env); v != "" && v != "root" {
			return v
		}
	}
	if u, err := user.Current(); err == nil {
		return u.Username
	}
	return os.Getenv("USER")
}

// invokingHome returns the invoking user's home directory, again preferring the
// pre-sudo identity so we do not scatter files under /root.
func invokingHome() string {
	if name := os.Getenv("SUDO_USER"); name != "" && name != "root" {
		if u, err := user.Lookup(name); err == nil {
			return u.HomeDir
		}
	}
	if u, err := user.Current(); err == nil && u.HomeDir != "" {
		return u.HomeDir
	}
	return os.Getenv("HOME")
}

// have reports whether an executable is on PATH.
func have(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}

// exists reports whether a filesystem path is present.
func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
