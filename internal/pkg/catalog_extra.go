package pkg

import "github.com/Borderliner/emacs-installer/internal/sys"

// catalogExtra holds the package mappings for the platforms added later: Solus
// (eopkg), OpenMandriva (dnf, lib64 naming), Slackware (slackpkg), and the BSDs
// (pkg / pkg_add / pkgin). Names here are best-effort — for managers that can
// search (all but slackpkg), the resolver validates them against the live repos
// at runtime and flags anything it cannot confirm.
//
// It is kept separate from the main catalog for readability and merged in below.
var catalogExtra = map[string]map[sys.Family][]Req{
	DepToolchain: {
		sys.FamilySolus:     {r("gcc"), r("make"), r("binutils"), r("glibc-devel"), r("pkg-config")},
		sys.FamilyMandriva:  {r("gcc"), r("make"), r("glibc-devel"), r("pkgconf")},
		sys.FamilySlackware: {r("gcc"), r("make"), r("binutils"), r("glibc")},
		sys.FamilyFreeBSD:   {r("gmake"), r("pkgconf")},
		sys.FamilyOpenBSD:   {r("gmake")},
		sys.FamilyNetBSD:    {r("gmake"), r("pkgconf")},
	},
	DepTexinfo: {
		sys.FamilySolus:     {r("texinfo")},
		sys.FamilyMandriva:  {r("texinfo")},
		sys.FamilySlackware: {r("texinfo")},
		sys.FamilyFreeBSD:   {r("texinfo")},
		sys.FamilyOpenBSD:   {r("texinfo")},
		sys.FamilyNetBSD:    {r("gtexinfo", "texinfo")},
	},
	DepNcurses: {
		sys.FamilySolus:     {r("ncurses-devel")},
		sys.FamilyMandriva:  {r("lib64ncurses-devel", "ncurses-devel")},
		sys.FamilySlackware: {r("ncurses")},
		sys.FamilyFreeBSD:   {r("ncurses")},
		sys.FamilyNetBSD:    {r("ncurses", "ncursesw")},
		// OpenBSD ships ncurses in the base system.
	},
	DepGnuTLS: {
		sys.FamilySolus:     {r("gnutls-devel")},
		sys.FamilyMandriva:  {r("lib64gnutls-devel", "gnutls-devel")},
		sys.FamilySlackware: {r("gnutls")},
		sys.FamilyFreeBSD:   {r("gnutls")},
		sys.FamilyOpenBSD:   {r("gnutls")},
		sys.FamilyNetBSD:    {r("gnutls")},
	},
	DepXML: {
		sys.FamilySolus:     {r("libxml2-devel")},
		sys.FamilyMandriva:  {r("lib64xml2-devel", "libxml2-devel")},
		sys.FamilySlackware: {r("libxml2")},
		sys.FamilyFreeBSD:   {r("libxml2")},
		sys.FamilyOpenBSD:   {r("libxml")},
		sys.FamilyNetBSD:    {r("libxml2")},
	},
	DepGtk3: {
		sys.FamilySolus:     {r("libgtk-3-devel")},
		sys.FamilyMandriva:  {r("lib64gtk+3.0-devel", "lib64gtk3.0-devel")},
		sys.FamilySlackware: {r("gtk+3")},
		sys.FamilyFreeBSD:   {r("gtk3")},
		sys.FamilyOpenBSD:   {r("gtk+3")},
		sys.FamilyNetBSD:    {r("gtk3")},
	},
	DepGccJIT: {
		sys.FamilyMandriva: {r("lib64gccjit-devel", "libgccjit-devel")},
		// Solus/Slackware/BSD do not reliably package libgccjit; native
		// compilation defaults off there (see DefaultSelection).
	},
	DepTreesit: {
		sys.FamilySolus:    {r("tree-sitter-devel", "libtree-sitter-devel")},
		sys.FamilyMandriva: {r("lib64tree-sitter-devel", "tree-sitter-devel")},
		sys.FamilyFreeBSD:  {r("tree-sitter", "libtree-sitter")},
		sys.FamilyOpenBSD:  {r("tree-sitter")},
		sys.FamilyNetBSD:   {r("tree-sitter")},
	},
	DepJansson: {
		sys.FamilySolus:    {r("jansson-devel")},
		sys.FamilyMandriva: {r("lib64jansson-devel", "jansson-devel")},
		sys.FamilyFreeBSD:  {r("jansson")},
		sys.FamilyOpenBSD:  {r("jansson")},
		sys.FamilyNetBSD:   {r("jansson")},
	},
	DepPNG: {
		sys.FamilySolus:     {r("libpng-devel")},
		sys.FamilyMandriva:  {r("lib64png-devel", "libpng-devel")},
		sys.FamilySlackware: {r("libpng")},
		sys.FamilyFreeBSD:   {r("png")},
		sys.FamilyOpenBSD:   {r("png")},
		sys.FamilyNetBSD:    {r("png")},
	},
	DepJPEG: {
		sys.FamilySolus:     {r("libjpeg-turbo-devel")},
		sys.FamilyMandriva:  {r("lib64jpeg-devel", "libjpeg-turbo-devel")},
		sys.FamilySlackware: {r("libjpeg-turbo")},
		sys.FamilyFreeBSD:   {r("jpeg-turbo")},
		sys.FamilyOpenBSD:   {r("jpeg")},
		sys.FamilyNetBSD:    {r("jpeg")},
	},
	DepGIF: {
		sys.FamilySolus:     {r("giflib-devel")},
		sys.FamilyMandriva:  {r("lib64giflib-devel", "giflib-devel")},
		sys.FamilySlackware: {r("giflib")},
		sys.FamilyFreeBSD:   {r("giflib")},
		sys.FamilyOpenBSD:   {r("giflib")},
		sys.FamilyNetBSD:    {r("giflib")},
	},
	DepTIFF: {
		sys.FamilySolus:     {r("libtiff-devel")},
		sys.FamilyMandriva:  {r("lib64tiff-devel", "libtiff-devel")},
		sys.FamilySlackware: {r("libtiff")},
		sys.FamilyFreeBSD:   {r("tiff")},
		sys.FamilyOpenBSD:   {r("tiff")},
		sys.FamilyNetBSD:    {r("tiff")},
	},
	DepXPM: {
		sys.FamilySolus:     {r("libxpm-devel")},
		sys.FamilyMandriva:  {r("lib64xpm-devel", "libXpm-devel")},
		sys.FamilySlackware: {r("libXpm")},
		sys.FamilyFreeBSD:   {r("libXpm")},
		sys.FamilyOpenBSD:   {r("xpm")},
		sys.FamilyNetBSD:    {r("libXpm")},
	},
	DepWebP: {
		sys.FamilySolus:     {r("libwebp-devel")},
		sys.FamilyMandriva:  {r("lib64webp-devel", "libwebp-devel")},
		sys.FamilySlackware: {r("libwebp")},
		sys.FamilyFreeBSD:   {r("webp")},
		sys.FamilyOpenBSD:   {r("libwebp")},
		sys.FamilyNetBSD:    {r("libwebp")},
	},
	DepRSVG: {
		sys.FamilySolus:     {r("librsvg-devel")},
		sys.FamilyMandriva:  {r("lib64rsvg2.0-devel", "librsvg2-devel")},
		sys.FamilySlackware: {r("librsvg")},
		sys.FamilyFreeBSD:   {r("librsvg2")},
		sys.FamilyOpenBSD:   {r("librsvg")},
		sys.FamilyNetBSD:    {r("librsvg")},
	},
	DepMagick: {
		sys.FamilySolus:     {r("imagemagick-devel")},
		sys.FamilyMandriva:  {r("imagemagick-devel", "lib64magick-devel")},
		sys.FamilySlackware: {r("ImageMagick")},
		sys.FamilyFreeBSD:   {r("ImageMagick7")},
		sys.FamilyOpenBSD:   {r("ImageMagick")},
		sys.FamilyNetBSD:    {r("ImageMagick")},
	},
	DepWebkit: {
		sys.FamilySolus:    {r("libwebkit-gtk41-devel", "webkit2gtk-devel")},
		sys.FamilyMandriva: {r("lib64webkit2-devel")},
		sys.FamilyFreeBSD:  {r("webkit2-gtk3")},
		sys.FamilyOpenBSD:  {r("webkitgtk4")},
		sys.FamilyNetBSD:   {r("webkit-gtk")},
	},
	DepSound: { // ALSA — Linux only; the sound toggle is dropped on the BSDs.
		sys.FamilySolus:     {r("alsa-lib-devel")},
		sys.FamilyMandriva:  {r("lib64asound2-devel", "alsa-lib-devel")},
		sys.FamilySlackware: {r("alsa-lib")},
	},
	DepDbus: {
		sys.FamilySolus:     {r("dbus-devel")},
		sys.FamilyMandriva:  {r("lib64dbus-devel", "dbus-devel")},
		sys.FamilySlackware: {r("dbus")},
	},
	DepGpm: {
		sys.FamilySolus:     {r("gpm-devel")},
		sys.FamilyMandriva:  {r("lib64gpm-devel", "gpm-devel")},
		sys.FamilySlackware: {r("gpm")},
	},
	DepHarfBuzz: {
		sys.FamilySolus:     {r("harfbuzz-devel")},
		sys.FamilyMandriva:  {r("lib64harfbuzz-devel", "harfbuzz-devel")},
		sys.FamilySlackware: {r("harfbuzz")},
		sys.FamilyFreeBSD:   {r("harfbuzz")},
		sys.FamilyOpenBSD:   {r("harfbuzz")},
		sys.FamilyNetBSD:    {r("harfbuzz")},
	},
	DepCairo: {
		sys.FamilySolus:     {r("libcairo-devel")},
		sys.FamilyMandriva:  {r("lib64cairo-devel", "cairo-devel")},
		sys.FamilySlackware: {r("cairo")},
		sys.FamilyFreeBSD:   {r("cairo")},
		sys.FamilyOpenBSD:   {r("cairo")},
		sys.FamilyNetBSD:    {r("cairo")},
	},
	DepSystemd: { // only requested when the init system is systemd (Solus, Mandriva)
		sys.FamilySolus:    {r("systemd-devel")},
		sys.FamilyMandriva: {r("lib64systemd-devel", "systemd-devel")},
	},
}

// init merges catalogExtra into the primary catalog. The two never define the
// same (dep, family) pair, so this is a straight union.
func init() {
	for key, byFam := range catalogExtra {
		if catalog[key] == nil {
			catalog[key] = map[sys.Family][]Req{}
		}
		for fam, reqs := range byFam {
			catalog[key][fam] = reqs
		}
	}
}
