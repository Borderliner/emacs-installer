package emacs

import "github.com/Borderliner/emacs-installer/internal/pkg"

// Env describes the host facts that decide which build features make sense and
// what their defaults should be. It is assembled once the target version is
// known (EmacsMajor) so version-conditional options resolve correctly.
type Env struct {
	OS         string // "linux" or "darwin"
	Systemd    bool   // init system is systemd
	Wayland    bool   // running a Wayland session (nudges the default toolkit)
	EmacsMajor int    // major version being built, e.g. 30
}

// Toolkit is the graphical backend — a single-choice among mutually exclusive
// options.
type Toolkit struct {
	Key       string
	Title     string
	Help      string
	GUI       bool     // false for the terminal-only build
	Configure []string // configure args this toolkit contributes
	Deps      []string // logical dependency keys
}

// Toggle is one on/off build feature presented as a checkbox.
type Toggle struct {
	Key     string
	Title   string
	Help    string // explains exactly what the underlying configure flag enables
	Default bool

	on, off []string // configure args when enabled / disabled
	deps    []string // logical deps required when enabled

	linuxOnly    bool // ALSA, GPM, D-Bus, libsystemd
	needsSystemd bool // only meaningful under systemd
	needsGUI     bool // only meaningful with a graphical toolkit
	maxMajorLT   int  // if >0, only applies when EmacsMajor < this (e.g. JSON < 30)
}

// Toggle keys that need special assembly (native compilation has an AOT sub-mode).
const (
	keyNativeComp = "native-comp"
	keyNativeAOT  = "native-aot"
)

// baseDeps are always installed regardless of feature selection: a compiler,
// the manual toolchain, and terminfo.
var baseDeps = []string{pkg.DepToolchain, pkg.DepTexinfo, pkg.DepNcurses}

// allToggles is the master feature list. Toggles(env) filters it for the host.
var allToggles = []Toggle{
	{
		Key: keyNativeComp, Title: "Native compilation", Default: true,
		Help: "Compile your Elisp to native machine code with libgccjit. Large, across-the-board speed-up; the recommended modern default.",
		deps: []string{pkg.DepGccJIT}, // configure arg assembled specially (see Resolve)
	},
	{
		Key: keyNativeAOT, Title: "  ↳ Ahead-of-time (compile everything now)", Default: false,
		Help: "Natively compile ALL bundled Elisp during the build instead of lazily on first use. Noticeably longer build, but nothing to compile at runtime afterwards.",
	},
	{
		Key: "treesit", Title: "Tree-sitter", Default: true,
		Help: "Enable the built-in tree-sitter *-ts-mode major modes for fast, accurate, structural syntax highlighting.",
		on:   []string{"--with-tree-sitter"}, off: []string{"--without-tree-sitter"},
		deps: []string{pkg.DepTreesit},
	},
	{
		Key: "json", Title: "Fast JSON (libjansson)", Default: true, maxMajorLT: 30,
		Help: "Speed up JSON parsing (used heavily by LSP clients such as Eglot) via libjansson. Built in from Emacs 30, so this only appears on older versions.",
		on:   []string{"--with-json"}, off: []string{"--without-json"},
		deps: []string{pkg.DepJansson},
	},
	{
		Key: "modules", Title: "Dynamic modules", Default: true,
		Help: "Allow loading compiled dynamic modules (.so/.dylib) — required by packages like vterm and tree-sitter grammars.",
		on:   []string{"--with-modules"}, off: []string{"--without-modules"},
	},
	{
		Key: "tls", Title: "GnuTLS (encrypted networking)", Default: true,
		Help: "Secure network connections: HTTPS, IMAPS, and signed package archives. Strongly recommended — without it, package.el cannot reach the archives.",
		on:   []string{"--with-gnutls"}, off: []string{"--without-gnutls"},
		deps: []string{pkg.DepGnuTLS},
	},
	{
		Key: "xml", Title: "XML / HTML parsing (libxml2)", Default: true,
		Help: "Parse HTML and XML — used by the eww web browser and various feed/readers — via libxml2.",
		on:   []string{"--with-xml2"}, off: []string{"--without-xml2"},
		deps: []string{pkg.DepXML},
	},
	{
		Key: "harfbuzz", Title: "HarfBuzz text shaping", Default: true, needsGUI: true,
		Help: "Correctly shape complex scripts (Arabic, Indic) and font ligatures via HarfBuzz.",
		on:   []string{"--with-harfbuzz"}, off: []string{"--without-harfbuzz"},
		deps: []string{pkg.DepHarfBuzz},
	},
	{
		Key: "images", Title: "Core image formats (PNG/JPEG/GIF/TIFF/XPM)", Default: true, needsGUI: true,
		Help: "Display the standard raster image formats. Auto-detected at build time; this just makes sure the libraries are present.",
		deps: []string{pkg.DepPNG, pkg.DepJPEG, pkg.DepGIF, pkg.DepTIFF, pkg.DepXPM},
	},
	{
		Key: "rsvg", Title: "SVG images (librsvg)", Default: true, needsGUI: true,
		Help: "Render scalable SVG images — icons, some themes, and inline graphics — via librsvg.",
		on:   []string{"--with-rsvg"}, off: []string{"--without-rsvg"},
		deps: []string{pkg.DepRSVG},
	},
	{
		Key: "webp", Title: "WebP images", Default: true, needsGUI: true,
		Help: "Display WebP images.",
		on:   []string{"--with-webp"}, off: []string{"--without-webp"},
		deps: []string{pkg.DepWebP},
	},
	{
		Key: "imagemagick", Title: "ImageMagick (extra image formats)", Default: false, needsGUI: true,
		Help: "Load many additional image formats through ImageMagick. Heavier dependency; upstream disables it by default on security grounds.",
		on:   []string{"--with-imagemagick"}, off: []string{"--without-imagemagick"},
		deps: []string{pkg.DepMagick},
	},
	{
		Key: "xwidgets", Title: "XWidgets (embedded WebKit)", Default: false, needsGUI: true,
		Help: "Embed live GTK/WebKit widgets inside buffers — e.g. the xwidget-webkit in-Emacs browser. Pulls in WebKitGTK.",
		on:   []string{"--with-xwidgets"},
		deps: []string{pkg.DepWebkit},
	},
	{
		Key: "sound", Title: "Sound (ALSA)", Default: true, linuxOnly: true,
		Help: "Play audio with play-sound-file and the audible bell via ALSA.",
		on:   []string{"--with-sound=alsa"}, off: []string{"--with-sound=no"},
		deps: []string{pkg.DepSound},
	},
	{
		Key: "dbus", Title: "D-Bus", Default: true, linuxOnly: true,
		Help: "Talk to desktop services over D-Bus: notifications, the secrets/keyring service, MPRIS media control, and more.",
		on:   []string{"--with-dbus"}, off: []string{"--without-dbus"},
		deps: []string{pkg.DepDbus},
	},
	{
		Key: "gpm", Title: "Console mouse (GPM)", Default: true, linuxOnly: true,
		Help: "Mouse support inside the raw Linux text console (not a terminal emulator) via GPM.",
		on:   []string{"--with-gpm"}, off: []string{"--without-gpm"},
		deps: []string{pkg.DepGpm},
	},
	{
		Key: "libsystemd", Title: "systemd readiness notification", Default: true,
		linuxOnly: true, needsSystemd: true,
		Help: "Let an Emacs daemon report readiness to systemd (Type=notify units), so `systemctl` knows when the server is actually up.",
		on:   []string{"--with-libsystemd"}, off: []string{"--without-libsystemd"},
		deps: []string{pkg.DepSystemd},
	},
}

// applies reports whether a toggle is relevant on this host/version.
func (t Toggle) applies(env Env) bool {
	if t.linuxOnly && env.OS != "linux" {
		return false
	}
	if t.needsSystemd && !env.Systemd {
		return false
	}
	if t.maxMajorLT > 0 && env.EmacsMajor >= t.maxMajorLT {
		return false
	}
	return true
}

// Toggles returns the feature checkboxes applicable to env, in display order.
func Toggles(env Env) []Toggle {
	var out []Toggle
	for _, t := range allToggles {
		if t.applies(env) {
			out = append(out, t)
		}
	}
	return out
}

// Toolkits returns the graphical-backend choices for env; the first is the
// recommended default.
func Toolkits(env Env) []Toolkit {
	if env.OS == "darwin" {
		return []Toolkit{
			{Key: "ns", Title: "NextStep (native macOS.app)", GUI: true,
				Help:      "Native Cocoa GUI — builds a proper macOS application.",
				Configure: []string{"--with-ns"}},
			{Key: "nox", Title: "No GUI — terminal only", GUI: false,
				Help:      "Terminal (-nw) build with no windowing system.",
				Configure: []string{"--without-ns", "--without-x"}},
		}
	}
	gtk := Toolkit{Key: "gtk3", Title: "GTK 3", GUI: true,
		Help:      "Modern GTK 3 toolkit: best HiDPI, GTK themes and input methods. The safe default on X11.",
		Configure: []string{"--with-x-toolkit=gtk3", "--with-cairo"},
		Deps:      []string{pkg.DepGtk3, pkg.DepCairo}}
	pgtk := Toolkit{Key: "pgtk", Title: "Pure GTK (Wayland-native)", GUI: true,
		Help:      "Renders entirely through GTK, so it runs natively on Wayland without XWayland. Recommended on a Wayland session.",
		Configure: []string{"--with-pgtk", "--with-cairo"},
		Deps:      []string{pkg.DepGtk3, pkg.DepCairo}}
	nox := Toolkit{Key: "nox", Title: "No GUI — terminal only", GUI: false,
		Help:      "Terminal (-nw) build with no X/Wayland dependencies. Smallest footprint.",
		Configure: []string{"--without-x"}}

	if env.Wayland {
		return []Toolkit{pgtk, gtk, nox}
	}
	return []Toolkit{gtk, pgtk, nox}
}

// ToolkitByKey looks up a toolkit for env (falls back to the first/default).
func ToolkitByKey(env Env, key string) Toolkit {
	tks := Toolkits(env)
	for _, t := range tks {
		if t.Key == key {
			return t
		}
	}
	return tks[0]
}

// Selection is the user's feature choices.
type Selection struct {
	Toolkit string          // toolkit key
	Enabled map[string]bool // toggle key -> enabled
}

// DefaultSelection returns the recommended defaults for env.
func DefaultSelection(env Env) Selection {
	sel := Selection{Toolkit: Toolkits(env)[0].Key, Enabled: map[string]bool{}}
	for _, t := range Toggles(env) {
		sel.Enabled[t.Key] = t.Default
	}
	return sel
}

// Result is the assembled build configuration.
type Result struct {
	Args []string // ./configure arguments (excluding --prefix)
	Deps []string // logical dependency keys to resolve into packages
}

// Resolve turns a Selection into concrete configure arguments and the logical
// dependencies they require, applying all the cross-feature rules (native-comp
// AOT mode, GUI-only features dropped on a terminal build, etc.).
func Resolve(sel Selection, env Env) Result {
	tk := ToolkitByKey(env, sel.Toolkit)
	res := Result{}
	res.Args = append(res.Args, tk.Configure...)
	res.Deps = append(res.Deps, baseDeps...)
	res.Deps = append(res.Deps, tk.Deps...)

	for _, t := range allToggles {
		if !t.applies(env) {
			continue
		}
		if t.Key == keyNativeComp || t.Key == keyNativeAOT {
			continue // assembled below
		}
		if t.needsGUI && !tk.GUI {
			continue // no point building image/shaping support into a -nw binary
		}
		if sel.Enabled[t.Key] {
			res.Args = append(res.Args, t.on...)
			res.Deps = append(res.Deps, t.deps...)
		} else {
			res.Args = append(res.Args, t.off...)
		}
	}

	// Native compilation, with its ahead-of-time sub-mode.
	if sel.Enabled[keyNativeComp] {
		if sel.Enabled[keyNativeAOT] {
			res.Args = append(res.Args, "--with-native-compilation=aot")
		} else {
			res.Args = append(res.Args, "--with-native-compilation=yes")
		}
		res.Deps = append(res.Deps, pkg.DepGccJIT)
	} else {
		res.Args = append(res.Args, "--with-native-compilation=no")
	}

	res.Deps = dedup(res.Deps)
	return res
}

func dedup(in []string) []string {
	seen := map[string]bool{}
	out := in[:0:0]
	for _, s := range in {
		if !seen[s] {
			seen[s] = true
			out = append(out, s)
		}
	}
	return out
}
