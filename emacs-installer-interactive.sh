#!/usr/bin/env bash
set -euo pipefail

# =============== styling helpers ===============
bold()  { printf "\033[1m%s\033[0m\n" "$*"; }
info()  { printf "\033[1;34m[INFO]\033[0m %s\n" "$*"; }
warn()  { printf "\033[1;33m[WARN]\033[0m %s\n" "$*"; }
err()   { printf "\033[1;31m[ERR ]\033[0m %s\n" "$*"; }

require_cmd() { command -v "$1" >/dev/null 2>&1 || { err "Need '$1' on PATH"; exit 1; }; }

# =============== sanity ===============
require_cmd curl
require_cmd tar
if ! command -v apt >/dev/null 2>&1; then
  err "This script targets Debian/Ubuntu (needs apt)."
  exit 1
fi

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

choose_mode() {
  local choice
  bold "Select build mode:" >&2
  {
    echo "  1) Wayland GUI (pgtk)"
    echo "  2) TTY-only"
    echo "  3) Both (pgtk + tty)"
  } >&2
  read -rp $'\033[1;36m[?]\033[0m Enter 1/2/3 [3]: ' choice
  case "${choice:-3}" in
    1) printf '%s\n' pgtk ;;
    2) printf '%s\n' tty  ;;
    3|'') printf '%s\n' both ;;
    *) warn "Invalid choice; defaulting to 'both'" >&2; printf '%s\n' both ;;
  esac
}

detect_gccjit_majors() {
  dpkg -l | awk '/libgccjit-[0-9]+-dev/ {print $2}' | sed -E 's/.*libgccjit-([0-9]+)-dev.*/\1/' | sort -u
}

detect_gcc_major_from_gcc() {
  gcc -dumpfullversion 2>/dev/null | cut -d. -f1
}

pick_gcc_version() {
  local detected installed choices default choice
  installed="$(detect_gccjit_majors || true)"
  detected="$(detect_gcc_major_from_gcc || true)"
  choices=()
  [[ -n "${installed}" ]] && choices+=(${installed})
  [[ -n "${detected}" ]] && choices+=(${detected})
  # dedupe
  choices=($(printf "%s\n" "${choices[@]}" | sort -u))
  if ((${#choices[@]}==0)); then
    default="13"
  else
    default="${choices[-1]}"
  fi
  choice="$(prompt_default "GCC/libgccjit MAJOR version to use" "${default}")"
  echo "${choice}"
}

# =============== interactive config ===============
bold "Interactive Emacs builder (native-comp, lean)"
echo
EMACS_VER="$(prompt_default "Emacs version (tarball from ftp.gnu.org)" "30.2")"
MODE="$(choose_mode)"
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
echo "  Emacs version : ${EMACS_VER}"
echo "  Mode          : ${MODE}"
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
PREFIX_TTY="${PREFIX_BASE}/emacs-${EMACS_VER}-tty"

# =============== dependencies ===============
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
if [[ "${MODE}" == "pgtk" || "${MODE}" == "both" ]]; then
  sudo apt install -y \
    libgtk-3-dev libcairo2-dev libharfbuzz-dev libpango1.0-dev \
    libgif-dev libjpeg-dev libpng-dev libtiff-dev librsvg2-dev libxpm-dev
fi

# TTY deps
if [[ "${MODE}" == "tty" || "${MODE}" == "both" ]]; then
  sudo apt install -y libncurses-dev
fi

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
  local builddir="$1" prefix="$2" mode="$3"
  mkdir -p "${builddir}"
  pushd "${builddir}" >/dev/null
  make distclean >/dev/null 2>&1 || true
  rm -f config.cache

  local flags=("${COMMON_FLAGS[@]}")

  if [[ "${mode}" == "pgtk" ]]; then
    flags+=(
      "--with-pgtk"
      "--with-toolkit-scroll-bars"
      "--with-cairo"
      "--with-gif" "--with-jpeg" "--with-png" "--with-tiff"
      "--with-rsvg" "--with-xpm"
    )
  elif [[ "${mode}" == "tty" ]]; then
    flags+=(
      "--without-all"
      "--with-modules" "--with-native-compilation" "--with-zlib"
      "--with-gnutls" "--with-xml2" "--with-tree-sitter"
      "--with-gif=no" "--with-jpeg=no" "--with-png=no" "--with-tiff=no"
      "--with-rsvg=no" "--with-xpm=no" "--with-cairo=no"
    )
  fi

  info "Configuring (${mode})…"
  CC="gcc-${GCC_VER}" "${SRCDIR}/configure" \
    --prefix="${prefix}" \
    "${flags[@]}"

  popd >/dev/null
}

build_install() {
  local builddir="$1" prefix="$2" mode="$3"
  pushd "${builddir}" >/dev/null

  if [[ "${mode}" == "tty" ]]; then
    info "First pass (serial) bootstrap for TTY (avoids VPATH race)…"
    make -j"${MAKE_JOBS}" bootstrap NATIVE_FULL_AOT=1
  fi

  info "Building ${mode} (-j${MAKE_JOBS})…"
  make -j"${MAKE_JOBS}" NATIVE_FULL_AOT=1

  info "Installing ${mode} → ${prefix}…"
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

# =============== build(s) ===============
mkdir -p "${BIN_DIR}"

if [[ "${MODE}" == "pgtk" || "${MODE}" == "both" ]]; then
  configure_build "${WORKDIR}/build-pgtk" "${PREFIX_PGTK}" "pgtk"
  build_install    "${WORKDIR}/build-pgtk" "${PREFIX_PGTK}" "pgtk"
  ln -sf "${PREFIX_PGTK}/bin/emacs" "${BIN_DIR}/emacs"
fi

if [[ "${MODE}" == "tty" || "${MODE}" == "both" ]]; then
  configure_build "${WORKDIR}/build-tty" "${PREFIX_TTY}" "tty"
  build_install    "${WORKDIR}/build-tty" "${PREFIX_TTY}" "tty"
  ln -sf "${PREFIX_TTY}/bin/emacs" "${BIN_DIR}/emacs-tty"
fi

# =============== verify & post hints ===============
[[ "${MODE}" == "pgtk" || "${MODE}" == "both" ]] && verify_bin "${BIN_DIR}/emacs" "PGTK build"
[[ "${MODE}" == "tty"  || "${MODE}" == "both" ]] && verify_bin "${BIN_DIR}/emacs-tty" "TTY build"

# =============== optional config copy (from earlier choice) ===============
if (( HAVE_BUNDLED_EMACSD )) && [[ "${COPY_EMACSD}" == "Y" ]]; then
  info "Copying .emacs.d → \$HOME/.emacs.d"
  rsync -a --delete "${SCRIPT_DIR}/.emacs.d/" "$HOME/.emacs.d/"
else
  info "Skipping .emacs.d copy."
fi

bold ""
bold "============================================================"
bold "Success!"
echo "Install roots:"
[[ "${MODE}" == "pgtk" || "${MODE}" == "both" ]] && echo "  PGTK: ${PREFIX_PGTK}  (symlink: ${BIN_DIR}/emacs)"
[[ "${MODE}" == "tty"  || "${MODE}" == "both" ]] && echo "  TTY : ${PREFIX_TTY}   (symlink: ${BIN_DIR}/emacs-tty)"
echo ""
echo "Add to PATH (bash/zsh):"
echo "  echo 'export PATH=\"${BIN_DIR}:\$PATH\"' >> ~/.bashrc"
echo ""
echo "If you see 'libgccjit.so not found' at runtime, add (adjust GCC ${GCC_VER} if needed):"
echo "  echo 'export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/${GCC_VER}:\$LD_LIBRARY_PATH' >> ~/.bashrc"
echo ""
bold "--- Fish shell users ---"
echo "Add to PATH:"
echo "  set -U fish_user_paths ${BIN_DIR} \$fish_user_paths"
echo "If libgccjit is not found:"
echo "  set -Ux LD_LIBRARY_PATH /usr/lib/gcc/x86_64-linux-gnu/${GCC_VER}:\$LD_LIBRARY_PATH"
echo ""
bold "Done 🚀"
bold "============================================================"
