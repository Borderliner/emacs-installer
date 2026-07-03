# emacs-installer

A good-looking terminal UI that **compiles GNU Emacs from source and installs
it** for you, across the mainstream Linux distributions (and macOS, best-effort).

Built with [Bubble Tea](https://github.com/charmbracelet/bubbletea) — truecolor,
animated progress, and a single self-contained binary with no runtime
dependencies.

```
  Emacs Installer
  Linux Mint 22.3  ·  debian  ·  init: systemd  ·  12 threads  ·  root: sudo
  ✓ Version  ›  ✓ Features  ›  ✓ Packages  ›  ● Daemon  ›  ○ Review  ›  ○ Install
 ╭──────────────────────────────────────────────────────────────────────╮
 │   Installing Emacs   step 5 of 7                                       │
 │                                                                        │
 │   ███████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░   58%        │
 │                                                                        │
 │   ∙∙● Compile (make -j12)                                              │
 │   ┌──────────────────────────────────────────────────────────────┐    │
 │   │  CC       lisp.o                                              │    │
 │   │  CC       eval.o                                              │    │
 │   └──────────────────────────────────────────────────────────────┘    │
 ╰──────────────────────────────────────────────────────────────────────╯
```

## What it does

The wizard walks you through six steps and then runs the pipeline:

1. **Pick a version** — the latest pretest (alpha/beta) plus the last three
   major releases at their newest minor, discovered live from the GNU mirrors
   (with an offline fallback).
2. **Pick your build features** — friendly checkboxes, each explaining exactly
   what the underlying `./configure` flag turns on, with sane defaults. Choose a
   GUI toolkit (GTK 3, pure-GTK for Wayland, terminal-only, or NextStep on
   macOS).
3. **Dependencies** — your distro is auto-detected and the *correct package
   names are searched in its repositories* before anything is installed (so
   `libgccjit-14-dev` vs `libgccjit-13-dev`, `webkit2gtk-4.1` vs `4.0`, etc. are
   resolved to whatever your release actually ships).
4. **Compile** — with `make -j<cores>` using every CPU thread, shown with a live
   progress view and scrollable build log.
5. **Install** — into `/opt/emacs`, using `sudo` or `doas` (whichever you have),
   and optionally symlinked into `/usr/local/bin`.
6. **Integrate** — generates `.desktop` launchers and, if you want, enables an
   Emacs daemon on startup so `emacsclient` just works — on `systemd`, `OpenRC`,
   `runit`, `SysVinit`, `dinit`, or macOS `launchd`.

## Install

Grab a release binary, or build it yourself (needs Go 1.24+):

```sh
make build          # produces ./emacs-installer
./emacs-installer
```

### Try it safely first

```sh
./emacs-installer --dry-run     # walk the whole wizard; print every command
                                # instead of running it — nothing is changed
```

### Flags

| Flag         | Default      | Meaning                                              |
|--------------|--------------|------------------------------------------------------|
| `--dry-run`  | off          | Print commands instead of executing them             |
| `--su`       | auto         | Force `sudo` or `doas` for privileged steps          |
| `--prefix`   | `/opt/emacs` | Installation prefix                                  |
| `--version`  | —            | Print version and exit                               |

Run it as your **normal user** — it escalates only the steps that need root
(installing packages, `make install`, system service files) and prompts for a
password at most once per phase, which is what keeps `doas` (no credential cache
by default) usable from inside the TUI.

## Supported systems

**Package managers / families:** Debian·Ubuntu·Mint·Pop·… (`apt`),
Fedora·RHEL·Rocky·Alma (`dnf`), Arch·Manjaro·EndeavourOS (`pacman`),
openSUSE·SLES (`zypper`), Alpine (`apk`), Void (`xbps`), Gentoo (`emerge`,
best-effort), macOS (`brew`).

**Init systems for the daemon:** systemd, OpenRC, runit, SysVinit, dinit, and
macOS launchd. systemd and launchd get clean per-user units; the others install
a system service that runs the daemon as your user.

## Build features explained

Native compilation (libgccjit) with an optional ahead-of-time mode, tree-sitter,
fast JSON (libjansson, for Emacs < 30), dynamic modules, GnuTLS, libxml2,
HarfBuzz, the core image formats plus SVG/WebP/ImageMagick, XWidgets (WebKit),
ALSA sound, D-Bus, console GPM mouse, and systemd readiness notification. Each
checkbox describes precisely what it enables and only appears when it is
relevant to your platform and chosen Emacs version.

## After installing

```sh
emacs                 # if symlinked into /usr/local/bin
emacsclient -c        # attach a GUI frame to the daemon
/opt/emacs/bin/emacs  # always works, symlink or not
```

To remove a build: `sudo rm -rf /opt/emacs`, delete the `/usr/local/bin/emacs*`
symlinks, the `.desktop` files, and disable the service the same way your init
system enables it.

## Project layout

The code is split into small, single-purpose packages so it stays easy to
maintain:

```
main.go                 CLI entry point
internal/
  sys/        host detection: distro/family, init system, privilege, cpu
  pkg/        package-manager abstraction, cross-distro catalog, repo resolver
  emacs/      version discovery, compile-flag catalog, build pipeline, plan
  service/    per-init-system daemon unit generation
  desktop/    .desktop launcher generation
  action/     shared "create file / run command" vocabulary (root vs user)
  tui/        the Bubble Tea wizard (theme, model, screens)
```

Adding a distribution is usually just a few lines in `pkg/catalog.go`; adding an
init system is one method in `service/service.go`.

## Caveats

- Native compilation needs `libgccjit`, which isn't packaged everywhere
  (notably some musl distros) — the resolver flags anything it can't confirm.
- macOS support builds a NextStep app and copies it to `/Applications`, but is
  less exercised than Linux.
- Source tarballs are fetched over HTTPS from the GNU mirrors; signature
  verification is not yet performed.

## License

MIT © Mohammadreza Hajianpour
