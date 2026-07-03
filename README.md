# emacs-installer

A good-looking terminal UI that **compiles Emacs from source and installs it**
for you, across the mainstream Linux distributions (and macOS, best-effort).

It walks you through:

1. **Pick a version** — the latest pretest (alpha/beta) plus the last few major
   releases, discovered live from the GNU mirrors.
2. **Pick your build features** — friendly checkboxes, each explaining exactly
   what the underlying `./configure` flag turns on, with sane defaults.
3. **Install dependencies** — your distro is auto-detected and the correct
   package names are searched in its repositories before anything is installed.
4. **Compile** — with `make -j<cores>` using every CPU thread you have, shown
   with a live progress view.
5. **Install** — into `/opt/emacs`, using `sudo` or `doas` (whichever you have).
6. **Integrate** — generates `.desktop` launchers and, if you want, enables an
   Emacs daemon on startup so `emacsclient` just works — on `systemd`, `OpenRC`,
   `runit`, `SysVinit` or `dinit`.

## Status

Under active construction — see the commit history for progress.

## Building

```sh
make build      # produces ./emacs-installer
./emacs-installer
```

Requires Go 1.24+ to build. The resulting binary is self-contained.

### Try it safely

```sh
./emacs-installer --dry-run    # walk the whole wizard; print commands instead of running them
```

## License

MIT © Mohammadreza Hajianpour
