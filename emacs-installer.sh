#!/usr/bin/env bash
set -euo pipefail

### ====== configurable knobs ======
: "${EMACS_VER:=30.2}"            # emacs release tarball version
: "${GCC_VER:=13}"                # must match installed libgccjit major
: "${GUI:=pgtk}"                  # pgtk | tty
: "${PREFIX:=$HOME/.local}"       # install prefix
: "${MAKE_JOBS:=$(nproc)}"        # parallel jobs for make
### ================================

info() { printf '\033[1;34m[INFO]\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m %s\n' "$*"; }
die()  { printf '\033[1;31m[ERR ]\033[0m %s\n' "$*" ; exit 1; }

### 0) sanity & OS check (best effort)
if ! command -v apt >/dev/null 2>&1; then
  die "This script expects Debian/Ubuntu (apt available)."
fi

### 1) install build deps
info "Installing build dependencies (requires sudo)..."
export DEBIAN_FRONTEND=noninteractive
sudo apt update

# core toolchain + docs
sudo apt install -y \
  build-essential pkg-config autoconf automake texinfo \
  curl ca-certificates git

# native-comp toolchain (match GCC & JIT major)
sudo apt install -y "gcc-${GCC_VER}" "libgccjit-${GCC_VER}-dev"

# libraries we explicitly enable for a lean-but-capable Emacs
sudo apt install -y \
  zlib1g-dev libgnutls28-dev libxml2-dev libjansson-dev libtree-sitter-dev

# GUI-specific deps (Wayland/GTK via PGTK) or none for TTY
if [[ "${GUI}" == "pgtk" ]]; then
  sudo apt install -y \
    libgtk-3-dev libcairo2-dev libharfbuzz-dev libpango1.0-dev
fi

### 2) fetch source tarball
WORKDIR="$(mktemp -d)"
trap 'rm -rf "${WORKDIR}"' EXIT
cd "${WORKDIR}"

TARBALL="emacs-${EMACS_VER}.tar.xz"
URL="https://ftp.gnu.org/gnu/emacs/${TARBALL}"

info "Downloading Emacs ${EMACS_VER}..."
curl -fsSL "${URL}" -o "${TARBALL}" || die "Failed to download ${URL}"
tar xf "${TARBALL}"
cd "emacs-${EMACS_VER}"

### 3) configure flags
COMMON_FLAGS=(
  "--prefix=${PREFIX}"
  "--without-all"                # start ultra-minimal
  "--with-native-compilation"
  "--with-threads"
  "--with-modules"
  "--with-gnutls"
  "--with-xml2"
  "--with-tree-sitter"
  "--with-zlib"
  # hard-disable image & heavy extras (keeps binary small)
  "--with-gif=no" "--with-jpeg=no" "--with-png=no" "--with-tiff=no"
  "--with-rsvg=no" "--with-xpm=no"
  # keep bloat off
  "--without-x"
  "--without-sound"
  "--without-dbus"
  "--without-gsettings"
  "--without-xwidgets"
)

if [[ "${GUI}" == "pgtk" ]]; then
  CFG_FLAGS=(
    "${COMMON_FLAGS[@]}"
    "--with-pgtk"
    "--with-toolkit-scroll-bars"    # required when starting from --without-all
  )
elif [[ "${GUI}" == "tty" ]]; then
  CFG_FLAGS=(
    "${COMMON_FLAGS[@]}"
    # no GUI flags
  )
else
  die "Unknown GUI profile '${GUI}'. Use 'pgtk' or 'tty'."
fi

### 4) clean stale state (if re-running in same tree)
make distclean >/dev/null 2>&1 || true
rm -f config.cache

### 5) configure with the chosen GCC (to match libgccjit)
info "Configuring (GUI=${GUI}, GCC=${GCC_VER})..."
CC="gcc-${GCC_VER}" ./configure "${CFG_FLAGS[@]}"

### 6) build (AOT everything) & install stripped
info "Building Emacs (NATIVE_FULL_AOT=1, -j${MAKE_JOBS})..."
make -j"${MAKE_JOBS}" NATIVE_FULL_AOT=1

info "Installing to ${PREFIX}..."
make install-strip

### 7) post-install: basic verification
EMACS_BIN="${PREFIX}/bin/emacs"
if [[ ! -x "${EMACS_BIN}" ]]; then
  die "Install finished but ${EMACS_BIN} not found."
fi

info "Verifying native-comp & features..."
"${EMACS_BIN}" --batch \
  --eval '(princ (format "native-comp: %S\n" (native-comp-available-p)))' \
  --eval '(princ (format "features: %s\n" system-configuration-features))'

cat <<'EOF'

Success!

Tips:
- Add ~/.local/bin to your PATH if not already:
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc

- If you see "libgccjit.so not found" at runtime, add (adjust for GCC major):
    echo 'export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/'"${GCC_VER}"':$LD_LIBRARY_PATH' >> ~/.bashrc

- Minimal startup tweaks (optional) – create ~/.emacs.d/early-init.el with:
    (setq package-enable-at-startup nil
          native-comp-async-report-warnings-errors 'silent
          inhibit-startup-message t)

EOF
