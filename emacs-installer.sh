#!/usr/bin/env bash
set -euo pipefail

# =============== styling helpers ===============
bold()  { printf "\033[1m%s\033[0m\n" "$*"; }
info()  { printf "\033[1;34m[INFO]\033[0m %s\n" "$*"; }
warn()  { printf "\033[1;33m[WARN]\033[0m %s\n" "$*"; }
err()   { printf "\033[1;31m[ERR ]\033[0m %s\n" "$*"; }

require_cmd() { command -v "$1" >/dev/null 2>&1 || { err "Need '$1' on PATH"; exit 1; }; }

# =============== OS detection ===============
detect_os() {
    if command -v apt >/dev/null 2>&1; then
        echo "debian"
    elif command -v pacman >/dev/null 2>&1; then
        echo "arch"
    else
        err "This script targets Debian/Ubuntu or Arch Linux."
        exit 1
    fi
}

OS_TYPE=$(detect_os)

# Script directory (for bundled .emacs.d detection)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HAVE_BUNDLED_EMACSD=0
[[ -d "${SCRIPT_DIR}/.emacs.d" ]] && HAVE_BUNDLED_EMACSD=1

# =============== tiny prompts ===============
prompt_default() {
  local prompt="$1" default="$2" var
  read -rp "$(printf '\033[1;36m[?]\033[0m %s [%s]: ' "$prompt" "$default")" var
  printf '%s' "${var:-$default}"
}

yesno() {
  local prompt="$1" default="${2:-Y}" ans
  while :; do
    read -rp "$(printf '\033[1;36m[?]\033[0m %s [Y/n]: ' "$prompt")" ans
    ans="${ans:-$default}"
    case "${ans}" in
      Y|y|yes) return 0 ;;
      N|n|no)  return 1 ;;
      *) echo "Please answer y or n." ;;
    esac
  done
}

# =============== GCC version detection ===============
detect_gccjit_majors_debian() {
    dpkg -l | awk '/libgccjit-[0-9]+-dev/ {print $2}' | sed -E 's/.*libgccjit-([0-9]+)-dev.*/\1/' | sort -u
}

detect_gccjit_majors_arch() {
    pacman -Q | awk '/gcc/ && !/gnu/ {print $1}' | sed -E 's/^gcc([0-9]+).*/\1/' | grep -E '^[0-9]+$' | sort -u
}

detect_gcc_major_from_gcc() {
    gcc -dumpfullversion 2>/dev/null | cut -d. -f1
}

pick_gcc_version() {
    local detected installed choices default choice
    
    if [[ "$OS_TYPE" == "debian" ]]; then
        installed="$(detect_gccjit_majors_debian || true)"
    else
        installed="$(detect_gccjit_majors_arch || true)"
    fi
    
    detected="$(detect_gcc_major_from_gcc || true)"
    choices=()
    [[ -n "${installed}" ]] && choices+=(${installed})
    [[ -n "${detected}" ]] && choices+=(${detected})
    # dedupe
    choices=($(printf "%s\n" "${choices[@]}" | sort -u))
    
    if ((${#choices[@]}==0)); then
        if [[ "$OS_TYPE" == "debian" ]]; then
            default="13"
        else
            default="$(detect_gcc_major_from_gcc || echo "13")"
        fi
    else
        default="${choices[-1]}"
    fi
    
    choice="$(prompt_default "GCC/libgccjit MAJOR version to use" "${default}")"
    echo "${choice}"
}

# =============== dependency installation ===============
install_dependencies_debian() {
    info "Installing dependencies (sudo apt)…"
    export DEBIAN_FRONTEND=noninteractive
    sudo apt update
    sudo apt install -y \
        build-essential pkg-config autoconf automake texinfo \
        curl ca-certificates git rsync

    sudo apt install -y "gcc-${GCC_VER}" "libgccjit-${GCC_VER}-dev"

    # common libs
    sudo apt install -y \
        zlib1g-dev libgnutls28-dev libxml2-dev libjansson-dev libtree-sitter-dev

    # GUI/image deps
    sudo apt install -y \
        libgtk-3-dev libcairo2-dev libharfbuzz-dev libpango1.0-dev \
        libgif-dev libjpeg-dev libpng-dev libtiff-dev librsvg2-dev libxpm-dev
}

install_dependencies_arch() {
    info "Installing dependencies (sudo pacman)…"
    sudo pacman -Sy --needed --noconfirm \
        base-devel pkg-config autoconf automake texinfo \
        curl ca-certificates git rsync

    # GCC and libgccjit (Arch typically uses unversioned gcc for the latest)
    sudo pacman -S --needed --noconfirm gcc libgccjit

    # common libs
    sudo pacman -S --needed --noconfirm \
        zlib-ng-compat gnutls libxml2 jansson tree-sitter

    # GUI/image deps
    sudo pacman -S --needed --noconfirm \
        gtk3 cairo harfbuzz pango \
        giflib libjpeg libpng libtiff librsvg libxpm
}

install_dependencies() {
    case "$OS_TYPE" in
        debian) install_dependencies_debian ;;
        arch) install_dependencies_arch ;;
    esac
}

# =============== interactive config ===============
bold "Interactive Emacs builder (native-comp, lean)"
echo
EMACS_VER="$(prompt_default "Emacs version (tarball from ftp.gnu.org)" "30.2")"
MAKE_JOBS="$(prompt_default "Parallel jobs for make" "$(nproc)")"

# GCC/libgccjit major
GCC_VER="$(pick_gcc_version)"

# Install base prefix
PREFIX_BASE="$(prompt_default "Base install prefix" "$HOME/.local")"
BIN_DIR="${PREFIX_BASE}/bin"

# Work directory
DEFAULT_WORKDIR="$(mktemp -d)"
WORKDIR="$(prompt_default "Working directory (temp build dir)" "${DEFAULT_WORKDIR}")"
mkdir -p "${WORKDIR}"

# Ask about copying bundled .emacs.d during setup (if present)
COPY_EMACSD="N"
if (( HAVE_BUNDLED_EMACSD )); then
  if yesno "Found bundled .emacs.d next to this script. Copy it to \$HOME/.emacs.d?" "Y"; then
    COPY_EMACSD="Y"
  else
    COPY_EMACSD="N"
  fi
fi

bold ""
bold "Summary"
echo "  OS            : ${OS_TYPE}"
echo "  Emacs version : ${EMACS_VER}"
echo "  Mode          : pgtk"
echo "  GCC major     : ${GCC_VER}"
echo "  Prefix base   : ${PREFIX_BASE}"
echo "  Bin dir       : ${BIN_DIR}"
echo "  Work dir      : ${WORKDIR}"
if (( HAVE_BUNDLED_EMACSD )); then
  echo "  Copy .emacs.d : ${COPY_EMACSD}"
else
  echo "  Copy .emacs.d : (no bundled .emacs.d detected)"
fi
bold ""

yesno "Proceed with these settings?" "Y" || { info "Aborted."; exit 0; }

trap '[[ -d "${DEFAULT_WORKDIR}" ]] && [[ "${WORKDIR}" == "${DEFAULT_WORKDIR}" ]] && rm -rf "${WORKDIR}" || true' EXIT

PREFIX_PGTK="${PREFIX_BASE}/emacs-${EMACS_VER}-pgtk"

# =============== dependencies ===============
require_cmd curl
require_cmd tar
install_dependencies

# =============== fetch source ===============
cd "${WORKDIR}"
TARBALL="emacs-${EMACS_VER}.tar.xz"
URL="http://ftpmirror.gnu.org/emacs/${TARBALL}"

info "Downloading Emacs ${EMACS_VER}…"
if command -v wget >/dev/null 2>&1; then
  wget -O "${TARBALL}" "${URL}"
else
  curl -fL --progress-bar "${URL}" -o "${TARBALL}"
fi
tar xf "${TARBALL}"
SRCDIR="${WORKDIR}/emacs-${EMACS_VER}"

# =============== configure flag sets ===============
COMMON_FLAGS=(
  "--with-native-compilation"
  "--with-threads"
  "--with-modules"
  "--with-gnutls"
  "--with-xml2"
  "--with-tree-sitter"
  "--with-zlib"
  "--without-x"
  "--without-sound"
  "--without-dbus"
  "--without-gsettings"
  "--without-xwidgets"
)

configure_build() {
  local builddir="$1" prefix="$2"
  mkdir -p "${builddir}"
  pushd "${builddir}" >/dev/null
  make distclean >/dev/null 2>&1 || true
  rm -f config.cache

  local flags=("${COMMON_FLAGS[@]}")
  flags+=(
    "--with-pgtk"
    "--with-toolkit-scroll-bars"
    "--with-cairo"
    "--with-gif" "--with-jpeg" "--with-png" "--with-tiff"
    "--with-rsvg" "--with-xpm"
  )

  info "Configuring (pgtk)…"
  
  # On Arch, use regular gcc; on Debian, use versioned gcc
  if [[ "$OS_TYPE" == "debian" ]]; then
    CC="gcc-${GCC_VER}" "${SRCDIR}/configure" \
      --prefix="${prefix}" \
      "${flags[@]}"
  else
    CC="gcc" "${SRCDIR}/configure" \
      --prefix="${prefix}" \
      "${flags[@]}"
  fi

  popd >/dev/null
}

build_install() {
  local builddir="$1" prefix="$2"
  pushd "${builddir}" >/dev/null

  info "Building pgtk (-j${MAKE_JOBS})…"
  make -j"${MAKE_JOBS}" NATIVE_FULL_AOT=1

  info "Installing pgtk → ${prefix}…"
  make install-strip
  popd >/dev/null
}

verify_bin() {
  local exe="$1" label="$2"
  if [[ -x "${exe}" ]]; then
    info "Verifying ${label}…"
    "${exe}" --batch \
      --eval '(princ (format "native-comp: %S\n" (native-comp-available-p)))' \
      --eval '(princ (format "features: %s\n" system-configuration-features))'
  else
    warn "Expected executable not found: ${exe}"
  fi
}

# =============== build ===============
mkdir -p "${BIN_DIR}"

configure_build "${WORKDIR}/build-pgtk" "${PREFIX_PGTK}"
build_install    "${WORKDIR}/build-pgtk" "${PREFIX_PGTK}"
ln -sf "${PREFIX_PGTK}/bin/emacs" "${BIN_DIR}/emacs"
ln -sf "${PREFIX_PGTK}/bin/emacsclient" "${BIN_DIR}/emacsclient"
ln -sf "${PREFIX_PGTK}/bin/ebrowse" "${BIN_DIR}/ebrowse"
ln -sf "${PREFIX_PGTK}/bin/etags" "${BIN_DIR}/etags"
ln -sf "${PREFIX_PGTK}/bin/ctags" "${BIN_DIR}/ctags"

# =============== verify & post hints ===============
verify_bin "${BIN_DIR}/emacs" "PGTK build"

# =============== optional config copy (from earlier choice) ===============
if (( HAVE_BUNDLED_EMACSD )) && [[ "${COPY_EMACSD}" == "Y" ]]; then
  info "Copying .emacs.d → \$HOME/.emacs.d"
  rsync -a --delete "${SCRIPT_DIR}/.emacs.d/" "$HOME/.emacs.d/"
else
  info "Skipping .emacs.d copy."
fi

bold "Copying emacs.desktop file"
mkdir -p $HOME/.local/share/applications
cp ./emacs.desktop $HOME/.local/share/applications
cp ./emacsclient.desktop $HOME/.local/share/applications
cp ./emacs-tty.desktop $HOME/.local/share/applications

# =============== Arch-specific library path hint ===============
LIBGCCJIT_PATH=""
if [[ "$OS_TYPE" == "arch" ]]; then
    # Try to find libgccjit.so on Arch
    LIBGCCJIT_PATH=$(find /usr/lib -name "libgccjit.so" 2>/dev/null | head -1)
    if [[ -z "$LIBGCCJIT_PATH" ]]; then
        LIBGCCJIT_PATH="/usr/lib/gcc/$(uname -m)-pc-linux-gnu/$(gcc -dumpversion)"
    else
        LIBGCCJIT_PATH=$(dirname "$LIBGCCJIT_PATH")
    fi
else
    LIBGCCJIT_PATH="/usr/lib/gcc/x86_64-linux-gnu/${GCC_VER}"
fi

bold ""
bold "============================================================"
bold "Success!"
echo "Install root:"
echo "  PGTK: ${PREFIX_PGTK}  (symlink: ${BIN_DIR}/emacs)"
echo ""
echo "Add to PATH (bash/zsh):"
echo "  echo 'export PATH=\"${BIN_DIR}:\$PATH\"' >> ~/.bashrc"
echo ""
echo "If you see 'libgccjit.so not found' at runtime, add:"
echo "  echo 'export LD_LIBRARY_PATH=${LIBGCCJIT_PATH}:\$LD_LIBRARY_PATH' >> ~/.bashrc"
echo ""
bold "--- Fish shell users ---"
echo "Add to PATH:"
echo "  set -U fish_user_paths ${BIN_DIR} \$fish_user_paths"
echo "If libgccjit is not found:"
echo "  set -Ux LD_LIBRARY_PATH ${LIBGCCJIT_PATH}:\$LD_LIBRARY_PATH"
echo ""
bold "Done 🚀"
bold "============================================================"