package emacs

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/Borderliner/emacs-installer/internal/action"
	"github.com/Borderliner/emacs-installer/internal/desktop"
	"github.com/Borderliner/emacs-installer/internal/pkg"
	"github.com/Borderliner/emacs-installer/internal/service"
	"github.com/Borderliner/emacs-installer/internal/sys"
)

// Config is the full set of installation decisions gathered by the wizard. It
// is the single input to BuildPhases, which turns it into an ordered pipeline.
type Config struct {
	Info      sys.Info
	Version   Version
	Env       Env
	Selection Selection

	Prefix        string       // install prefix, e.g. /opt/emacs
	Manager       *pkg.Manager // resolved package manager (nil if none)
	Packages      []string     // concrete package names to install
	ConfigureArgs []string     // configure args from Resolve (excludes --prefix)

	Symlink      bool           // link binaries into /usr/local/bin
	Desktop      bool           // generate .desktop launchers (Linux)
	EnableDaemon bool           // set up + enable the emacs daemon service
	Init         sys.InitSystem // init target for the daemon

	DryRun bool
}

// SrcDir is the extracted source tree.
func (c Config) SrcDir() string { return filepath.Join(sys.CacheDir(), c.Version.Dir()) }

// TarballPath is the downloaded archive.
func (c Config) TarballPath() string { return filepath.Join(sys.CacheDir(), c.Version.Tarball()) }

// EmacsBin is the installed emacs executable.
func (c Config) EmacsBin() string { return filepath.Join(c.Prefix, "bin", "emacs") }

// FullConfigureArgs is the complete ./configure argument list including prefix.
func (c Config) FullConfigureArgs() []string {
	return append([]string{"--prefix=" + c.Prefix}, c.ConfigureArgs...)
}

// BuildPhases turns the config into the ordered install pipeline.
func (c Config) BuildPhases() []Phase {
	var phases []Phase
	src := c.SrcDir()
	tarball := c.TarballPath()

	if len(c.Packages) > 0 && c.Manager != nil {
		title := "Install dependencies (" + c.Manager.Name + ")"
		script := c.Manager.InstallScript(c.Packages)
		if c.Manager.Escalate {
			phases = append(phases, c.privilegedPhase(title, script))
		} else {
			phases = append(phases, c.shellPhase(title, script))
		}
	}

	phases = append(phases,
		c.downloadPhase(tarball),
		c.extractPhase(tarball, src),
		c.execPhase("Configure build", src, "./configure", c.FullConfigureArgs()...),
		c.execPhase(fmt.Sprintf("Compile (make -j%d)", c.Info.Cores), src, "make", fmt.Sprintf("-j%d", c.Info.Cores)),
		c.privilegedPhase("Install to "+c.Prefix, c.installScript(src)),
	)

	if up, ok := c.userPhase(); ok {
		phases = append(phases, up)
	}
	phases = append(phases, c.manifestPhase())
	return phases
}

// integration collects the service + desktop files/commands. Each item is
// already tagged System or user by its generator.
func (c Config) integration() ([]action.FileSpec, []action.Command) {
	var specs []action.FileSpec
	var cmds []action.Command

	if c.Desktop && c.Info.OS == "linux" {
		specs = append(specs, desktop.Generate(c.Prefix, true, c.Info.Home)...)
	}
	if c.EnableDaemon {
		s, cm, err := service.Generate(service.Params{
			Init:     c.Init,
			User:     c.Info.User,
			Home:     c.Info.Home,
			EmacsBin: c.EmacsBin(),
			Notify:   c.Selection.Enabled["libsystemd"],
		})
		if err == nil {
			specs = append(specs, s...)
			cmds = append(cmds, cm...)
		}
	}
	return specs, cmds
}

// installScript is the root program: make install, optional /usr/local/bin
// symlinks, a macOS Emacs.app copy, then every System-scoped integration file
// and command.
func (c Config) installScript(src string) string {
	specs, cmds := c.integration()

	var b strings.Builder
	b.WriteString("set -e\n")
	fmt.Fprintf(&b, "make -C %s install\n", action.ShellQuote(src))

	if c.Info.OS == "darwin" && c.Selection.Toolkit == "ns" {
		fmt.Fprintf(&b, "if [ -d %s/nextstep/Emacs.app ]; then rm -rf /Applications/Emacs.app && cp -R %s/nextstep/Emacs.app /Applications/; fi\n",
			action.ShellQuote(src), action.ShellQuote(src))
	}
	if c.Symlink {
		b.WriteString("mkdir -p /usr/local/bin\n")
		fmt.Fprintf(&b, "for t in emacs emacsclient etags ebrowse ctags; do if [ -e %s/bin/$t ]; then ln -sf %s/bin/$t /usr/local/bin/$t; fi; done\n", c.Prefix, c.Prefix)
	}

	b.WriteString(action.ShellScript(specs, cmds))

	if c.Desktop && c.Info.OS == "linux" {
		b.WriteString("command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database /usr/share/applications 2>/dev/null || true\n")
		fmt.Fprintf(&b, "command -v gtk-update-icon-cache >/dev/null 2>&1 && gtk-update-icon-cache -f %s/share/icons/hicolor 2>/dev/null || true\n", c.Prefix)
	}
	return b.String()
}

// userPhase applies the user-scoped integration (systemd --user unit, launchd
// agent). Returns ok=false when there is nothing user-scoped to do.
func (c Config) userPhase() (Phase, bool) {
	specs, cmds := c.integration()
	if !action.HasUser(specs, cmds) {
		return Phase{}, false
	}
	const title = "User integration"
	if c.DryRun {
		return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
			for _, s := range specs {
				if !s.System {
					ch <- Progress{Line: "[dry-run] write " + s.Path}
				}
			}
			for _, cm := range cmds {
				if !cm.System {
					ch <- Progress{Line: "[dry-run] $ " + strings.Join(cm.Argv, " ")}
				}
			}
			ch <- Progress{Fraction: 1}
			return nil
		}}, true
	}
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		return action.ApplyUser(specs, cmds, func(l string) { ch <- Progress{Line: l} })
	}}, true
}

// --- phase constructors (each honours DryRun) -------------------------------

func (c Config) execPhase(title, dir, name string, args ...string) Phase {
	if c.DryRun {
		line := "[dry-run] $ " + name + " " + strings.Join(args, " ")
		return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
			ch <- Progress{Line: line, Fraction: 1}
			return nil
		}}
	}
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		return streamExec(ctx, ch, dir, nil, name, args...)
	}}
}

func (c Config) shellPhase(title, script string) Phase {
	if c.DryRun {
		return Phase{Title: title, Run: dryScript(script)}
	}
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		return streamExec(ctx, ch, "", nil, "sh", "-c", script)
	}}
}

func (c Config) privilegedPhase(title, script string) Phase {
	if c.DryRun {
		return Phase{Title: title, Run: dryScript(script)}
	}
	return Phase{Title: title, Privileged: true, Script: script}
}

func (c Config) downloadPhase(tarball string) Phase {
	title := "Download " + c.Version.Tarball()
	if c.DryRun {
		return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
			ch <- Progress{Line: "[dry-run] GET " + c.Version.URL, Fraction: 1}
			return nil
		}}
	}
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		if fi, err := os.Stat(tarball); err == nil && fi.Size() > 0 {
			ch <- Progress{Line: "already downloaded", Fraction: 1}
			return nil
		}
		return downloadFile(ctx, ch, c.Version.URL, tarball)
	}}
}

func (c Config) extractPhase(tarball, src string) Phase {
	title := "Extract source"
	if c.DryRun {
		return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
			ch <- Progress{Line: "[dry-run] tar xf " + tarball, Fraction: 1}
			return nil
		}}
	}
	return Phase{Title: title, Run: func(ctx context.Context, ch chan<- Progress) error {
		_ = os.RemoveAll(src)
		return extractTar(ctx, ch, tarball, sys.CacheDir())
	}}
}

// dryScript returns a Run that prints a shell program without executing it.
func dryScript(script string) func(context.Context, chan<- Progress) error {
	return func(ctx context.Context, ch chan<- Progress) error {
		ch <- Progress{Line: "[dry-run] would run:"}
		for _, ln := range strings.Split(strings.TrimRight(script, "\n"), "\n") {
			ch <- Progress{Line: "    " + ln}
		}
		ch <- Progress{Fraction: 1}
		return nil
	}
}
