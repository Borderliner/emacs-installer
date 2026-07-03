// Command emacs-installer is a terminal UI that compiles GNU Emacs from source
// and installs it on mainstream Linux distributions (and, best-effort, macOS).
package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/Borderliner/emacs-installer/internal/tui"
)

// version is overridden at build time via -ldflags (see the Makefile).
var version = "dev"

func main() {
	var (
		dryRun    bool
		su        string
		prefix    string
		uninstall bool
		showVer   bool
	)
	flag.BoolVar(&dryRun, "dry-run", false, "walk the wizard and print the commands instead of running them")
	flag.StringVar(&su, "su", "", "privilege escalation tool: sudo or doas (default: auto-detect)")
	flag.StringVar(&prefix, "prefix", "/opt/emacs", "installation prefix")
	flag.BoolVar(&uninstall, "uninstall", false, "remove a previous install (binaries, symlinks, launchers, daemon) and clean up")
	flag.BoolVar(&showVer, "version", false, "print version and exit")
	flag.Parse()

	if showVer {
		fmt.Println("emacs-installer", version)
		return
	}

	if err := tui.Run(tui.Options{DryRun: dryRun, Su: su, Prefix: prefix, Uninstall: uninstall}); err != nil {
		fmt.Fprintln(os.Stderr, "error:", err)
		os.Exit(1)
	}
}
