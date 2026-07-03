package pkg

import "github.com/hajianpour/emacs-installer/internal/sys"

// Logical dependency keys. Compile flags (internal/emacs) reference these; the
// catalog below maps each to concrete package names per distribution family.
const (
	DepToolchain = "toolchain" // C compiler + make
	DepTexinfo   = "texinfo"   // makeinfo, needed to build the manuals
	DepNcurses   = "ncurses"   // terminal (-nw) frontend
	DepGnuTLS    = "gnutls"    // TLS for network security
	DepXML       = "libxml2"   // libxml2 / libxslt parsing
	DepGtk3      = "gtk3"      // GTK 3 toolkit (also used by pure-GTK)
	DepGccJIT    = "gccjit"    // libgccjit for native compilation
	DepTreesit   = "treesitter"
	DepJansson   = "jansson" // fast JSON (Emacs < 30)
	DepPNG       = "png"
	DepJPEG      = "jpeg"
	DepGIF       = "gif"
	DepTIFF      = "tiff"
	DepXPM       = "xpm"
	DepWebP      = "webp"
	DepRSVG      = "rsvg" // SVG rendering
	DepMagick    = "imagemagick"
	DepWebkit    = "webkit" // xwidgets (embedded WebKit)
	DepSound     = "sound"  // ALSA
	DepDbus      = "dbus"
	DepGpm       = "gpm" // console mouse
	DepHarfBuzz  = "harfbuzz"
	DepCairo     = "cairo"
	DepSystemd   = "libsystemd" // sd_notify daemon integration
)

// r is a tiny constructor for a requirement (a list of alternative names).
func r(names ...string) Req { return names }

// catalog maps every logical dependency to the concrete package requirements of
// each family. A family absent from an inner map needs nothing for that dep
// (system-provided, or not applicable — e.g. GTK on a macOS NextStep build).
var catalog = map[string]map[sys.Family][]Req{
	DepToolchain: {
		sys.FamilyDebian: {r("build-essential")},
		sys.FamilyFedora: {r("gcc"), r("make")},
		sys.FamilyArch:   {r("base-devel")},
		sys.FamilySUSE:   {r("gcc"), r("make")},
		sys.FamilyAlpine: {r("build-base")},
		sys.FamilyVoid:   {r("base-devel")},
		sys.FamilyGentoo: {r("sys-devel/gcc"), r("sys-devel/make")},
		// macOS: the compiler comes from the Xcode Command Line Tools, which
		// are not a brew formula; the installer checks for them separately.
	},
	DepTexinfo: {
		sys.FamilyDebian: {r("texinfo")},
		sys.FamilyFedora: {r("texinfo")},
		sys.FamilyArch:   {r("texinfo")},
		sys.FamilySUSE:   {r("texinfo")},
		sys.FamilyAlpine: {r("texinfo")},
		sys.FamilyVoid:   {r("texinfo")},
		sys.FamilyGentoo: {r("sys-apps/texinfo")},
		sys.FamilyMacOS:  {r("texinfo")},
	},
	DepNcurses: {
		sys.FamilyDebian: {r("libncurses-dev", "libncurses5-dev")},
		sys.FamilyFedora: {r("ncurses-devel")},
		sys.FamilyArch:   {r("ncurses")},
		sys.FamilySUSE:   {r("ncurses-devel")},
		sys.FamilyAlpine: {r("ncurses-dev")},
		sys.FamilyVoid:   {r("ncurses-devel")},
		sys.FamilyGentoo: {r("sys-libs/ncurses")},
	},
	DepGnuTLS: {
		sys.FamilyDebian: {r("libgnutls28-dev")},
		sys.FamilyFedora: {r("gnutls-devel")},
		sys.FamilyArch:   {r("gnutls")},
		sys.FamilySUSE:   {r("libgnutls-devel")},
		sys.FamilyAlpine: {r("gnutls-dev")},
		sys.FamilyVoid:   {r("gnutls-devel")},
		sys.FamilyGentoo: {r("net-libs/gnutls")},
		sys.FamilyMacOS:  {r("gnutls")},
	},
	DepXML: {
		sys.FamilyDebian: {r("libxml2-dev")},
		sys.FamilyFedora: {r("libxml2-devel")},
		sys.FamilyArch:   {r("libxml2")},
		sys.FamilySUSE:   {r("libxml2-devel")},
		sys.FamilyAlpine: {r("libxml2-dev")},
		sys.FamilyVoid:   {r("libxml2-devel")},
		sys.FamilyGentoo: {r("dev-libs/libxml2")},
		sys.FamilyMacOS:  {r("libxml2")},
	},
	DepGtk3: {
		sys.FamilyDebian: {r("libgtk-3-dev")},
		sys.FamilyFedora: {r("gtk3-devel")},
		sys.FamilyArch:   {r("gtk3")},
		sys.FamilySUSE:   {r("gtk3-devel")},
		sys.FamilyAlpine: {r("gtk+3.0-dev")},
		sys.FamilyVoid:   {r("gtk+3-devel")},
		sys.FamilyGentoo: {r("x11-libs/gtk+:3")},
	},
	DepGccJIT: {
		// Debian/Ubuntu suffix the package with the GCC major version; search
		// picks whichever is present.
		sys.FamilyDebian: {r("libgccjit-14-dev", "libgccjit-13-dev", "libgccjit-12-dev", "libgccjit-11-dev", "libgccjit-10-dev", "libgccjit-dev")},
		sys.FamilyFedora: {r("libgccjit-devel")},
		sys.FamilyArch:   {r("libgccjit")},
		sys.FamilySUSE:   {r("libgccjit-devel")},
		sys.FamilyAlpine: {r("libgccjit")},
		sys.FamilyVoid:   {r("libgccjit-devel", "gccjit-devel")},
		sys.FamilyMacOS:  {r("libgccjit")},
		// Gentoo: provided by sys-devel/gcc built with USE=jit; cannot be added
		// as a separate package here.
	},
	DepTreesit: {
		sys.FamilyDebian: {r("libtree-sitter-dev")},
		sys.FamilyFedora: {r("libtree-sitter-devel")},
		sys.FamilyArch:   {r("tree-sitter")},
		sys.FamilySUSE:   {r("libtree-sitter-devel", "tree-sitter-devel")},
		sys.FamilyAlpine: {r("tree-sitter-dev")},
		sys.FamilyVoid:   {r("tree-sitter-devel")},
		sys.FamilyGentoo: {r("dev-libs/tree-sitter")},
		sys.FamilyMacOS:  {r("tree-sitter")},
	},
	DepJansson: {
		sys.FamilyDebian: {r("libjansson-dev")},
		sys.FamilyFedora: {r("jansson-devel")},
		sys.FamilyArch:   {r("jansson")},
		sys.FamilySUSE:   {r("libjansson-devel")},
		sys.FamilyAlpine: {r("jansson-dev")},
		sys.FamilyVoid:   {r("jansson-devel")},
		sys.FamilyGentoo: {r("dev-libs/jansson")},
		sys.FamilyMacOS:  {r("jansson")},
	},
	DepPNG: {
		sys.FamilyDebian: {r("libpng-dev")},
		sys.FamilyFedora: {r("libpng-devel")},
		sys.FamilyArch:   {r("libpng")},
		sys.FamilySUSE:   {r("libpng16-devel", "libpng-devel")},
		sys.FamilyAlpine: {r("libpng-dev")},
		sys.FamilyVoid:   {r("libpng-devel")},
		sys.FamilyGentoo: {r("media-libs/libpng")},
		sys.FamilyMacOS:  {r("libpng")},
	},
	DepJPEG: {
		sys.FamilyDebian: {r("libjpeg-dev", "libjpeg-turbo8-dev")},
		sys.FamilyFedora: {r("libjpeg-turbo-devel")},
		sys.FamilyArch:   {r("libjpeg-turbo")},
		sys.FamilySUSE:   {r("libjpeg8-devel", "libjpeg-devel")},
		sys.FamilyAlpine: {r("libjpeg-turbo-dev")},
		sys.FamilyVoid:   {r("libjpeg-turbo-devel")},
		sys.FamilyGentoo: {r("media-libs/libjpeg-turbo")},
		sys.FamilyMacOS:  {r("jpeg-turbo")},
	},
	DepGIF: {
		sys.FamilyDebian: {r("libgif-dev")},
		sys.FamilyFedora: {r("giflib-devel")},
		sys.FamilyArch:   {r("giflib")},
		sys.FamilySUSE:   {r("giflib-devel")},
		sys.FamilyAlpine: {r("giflib-dev")},
		sys.FamilyVoid:   {r("giflib-devel")},
		sys.FamilyGentoo: {r("media-libs/giflib")},
		sys.FamilyMacOS:  {r("giflib")},
	},
	DepTIFF: {
		sys.FamilyDebian: {r("libtiff-dev", "libtiff5-dev")},
		sys.FamilyFedora: {r("libtiff-devel")},
		sys.FamilyArch:   {r("libtiff")},
		sys.FamilySUSE:   {r("libtiff-devel")},
		sys.FamilyAlpine: {r("tiff-dev")},
		sys.FamilyVoid:   {r("tiff-devel")},
		sys.FamilyGentoo: {r("media-libs/tiff")},
		sys.FamilyMacOS:  {r("libtiff")},
	},
	DepXPM: {
		sys.FamilyDebian: {r("libxpm-dev")},
		sys.FamilyFedora: {r("libXpm-devel")},
		sys.FamilyArch:   {r("libxpm")},
		sys.FamilySUSE:   {r("libXpm-devel")},
		sys.FamilyAlpine: {r("libxpm-dev")},
		sys.FamilyVoid:   {r("libXpm-devel")},
		sys.FamilyGentoo: {r("x11-libs/libXpm")},
	},
	DepWebP: {
		sys.FamilyDebian: {r("libwebp-dev")},
		sys.FamilyFedora: {r("libwebp-devel")},
		sys.FamilyArch:   {r("libwebp")},
		sys.FamilySUSE:   {r("libwebp-devel")},
		sys.FamilyAlpine: {r("libwebp-dev")},
		sys.FamilyVoid:   {r("libwebp-devel")},
		sys.FamilyGentoo: {r("media-libs/libwebp")},
		sys.FamilyMacOS:  {r("webp")},
	},
	DepRSVG: {
		sys.FamilyDebian: {r("librsvg2-dev")},
		sys.FamilyFedora: {r("librsvg2-devel")},
		sys.FamilyArch:   {r("librsvg")},
		sys.FamilySUSE:   {r("librsvg-devel")},
		sys.FamilyAlpine: {r("librsvg-dev")},
		sys.FamilyVoid:   {r("librsvg-devel")},
		sys.FamilyGentoo: {r("gnome-base/librsvg")},
		sys.FamilyMacOS:  {r("librsvg")},
	},
	DepMagick: {
		sys.FamilyDebian: {r("libmagickwand-dev", "libmagick++-dev")},
		sys.FamilyFedora: {r("ImageMagick-devel")},
		sys.FamilyArch:   {r("imagemagick")},
		sys.FamilySUSE:   {r("ImageMagick-devel", "libMagickWand-devel")},
		sys.FamilyAlpine: {r("imagemagick-dev")},
		sys.FamilyVoid:   {r("libmagick-devel", "ImageMagick-devel")},
		sys.FamilyGentoo: {r("media-gfx/imagemagick")},
		sys.FamilyMacOS:  {r("imagemagick")},
	},
	DepWebkit: {
		sys.FamilyDebian: {r("libwebkit2gtk-4.1-dev", "libwebkit2gtk-4.0-dev")},
		sys.FamilyFedora: {r("webkit2gtk4.1-devel", "webkit2gtk3-devel")},
		sys.FamilyArch:   {r("webkit2gtk-4.1", "webkit2gtk")},
		sys.FamilySUSE:   {r("webkit2gtk3-devel", "libwebkit2gtk-4_1-devel")},
		sys.FamilyAlpine: {r("webkit2gtk-4.1-dev", "webkit2gtk-dev")},
		sys.FamilyVoid:   {r("webkit2gtk-devel")},
		sys.FamilyGentoo: {r("net-libs/webkit-gtk")},
	},
	DepSound: {
		sys.FamilyDebian: {r("libasound2-dev")},
		sys.FamilyFedora: {r("alsa-lib-devel")},
		sys.FamilyArch:   {r("alsa-lib")},
		sys.FamilySUSE:   {r("alsa-devel")},
		sys.FamilyAlpine: {r("alsa-lib-dev")},
		sys.FamilyVoid:   {r("alsa-lib-devel")},
		sys.FamilyGentoo: {r("media-libs/alsa-lib")},
	},
	DepDbus: {
		sys.FamilyDebian: {r("libdbus-1-dev")},
		sys.FamilyFedora: {r("dbus-devel")},
		sys.FamilyArch:   {r("dbus")},
		sys.FamilySUSE:   {r("dbus-1-devel")},
		sys.FamilyAlpine: {r("dbus-dev")},
		sys.FamilyVoid:   {r("dbus-devel")},
		sys.FamilyGentoo: {r("sys-apps/dbus")},
	},
	DepGpm: {
		sys.FamilyDebian: {r("libgpm-dev")},
		sys.FamilyFedora: {r("gpm-devel")},
		sys.FamilyArch:   {r("gpm")},
		sys.FamilySUSE:   {r("gpm-devel")},
		sys.FamilyVoid:   {r("gpm-devel")},
		sys.FamilyGentoo: {r("sys-libs/gpm")},
	},
	DepHarfBuzz: {
		sys.FamilyDebian: {r("libharfbuzz-dev")},
		sys.FamilyFedora: {r("harfbuzz-devel")},
		sys.FamilyArch:   {r("harfbuzz")},
		sys.FamilySUSE:   {r("harfbuzz-devel")},
		sys.FamilyAlpine: {r("harfbuzz-dev")},
		sys.FamilyVoid:   {r("harfbuzz-devel")},
		sys.FamilyGentoo: {r("media-libs/harfbuzz")},
		sys.FamilyMacOS:  {r("harfbuzz")},
	},
	DepCairo: {
		sys.FamilyDebian: {r("libcairo2-dev")},
		sys.FamilyFedora: {r("cairo-devel")},
		sys.FamilyArch:   {r("cairo")},
		sys.FamilySUSE:   {r("cairo-devel")},
		sys.FamilyAlpine: {r("cairo-dev")},
		sys.FamilyVoid:   {r("cairo-devel")},
		sys.FamilyGentoo: {r("x11-libs/cairo")},
		sys.FamilyMacOS:  {r("cairo")},
	},
	DepSystemd: {
		sys.FamilyDebian: {r("libsystemd-dev")},
		sys.FamilyFedora: {r("systemd-devel")},
		sys.FamilyArch:   {r("systemd-libs", "systemd")},
		sys.FamilySUSE:   {r("systemd-devel")},
		sys.FamilyGentoo: {r("sys-apps/systemd")},
	},
}
