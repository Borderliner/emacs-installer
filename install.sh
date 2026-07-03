#!/bin/sh
# Download the latest emacs-installer release binary for this platform and put it
# on your PATH. POSIX sh; no dependencies beyond curl/wget and sha256sum/shasum.
#
#   curl -fsSL https://raw.githubusercontent.com/Borderliner/emacs-installer/main/install.sh | sh
#
# Overrides (env vars):
#   VERSION=v0.1.0   install a specific release instead of the latest
#   BINDIR=~/.local/bin   install to a specific directory
set -eu

REPO="Borderliner/emacs-installer"
BINARY="emacs-installer"
: "${VERSION:=latest}"
: "${BINDIR:=}"

err() { printf 'error: %s\n' "$1" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

os=$(uname -s)
case "$os" in
	Linux) os=linux ;;
	Darwin) os=darwin ;;
	*) err "unsupported OS '$os' (this build targets Linux and macOS; see the README for Windows)" ;;
esac

arch=$(uname -m)
case "$arch" in
	x86_64 | amd64) arch=amd64 ;;
	aarch64 | arm64) arch=arm64 ;;
	*) err "unsupported architecture '$arch'" ;;
esac

asset="${BINARY}-${os}-${arch}"
if [ "$VERSION" = latest ]; then
	base="https://github.com/${REPO}/releases/latest/download"
else
	base="https://github.com/${REPO}/releases/download/${VERSION}"
fi

dl() { # url dest
	if have curl; then curl -fsSL "$1" -o "$2"
	elif have wget; then wget -qO "$2" "$1"
	else err "need curl or wget"; fi
}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT INT TERM

printf 'Downloading %s (%s)...\n' "$asset" "$VERSION"
dl "${base}/${asset}" "${tmp}/${BINARY}" || err "download failed: ${base}/${asset}"

# Verify the checksum when the release ships one and we can compute it.
if dl "${base}/checksums.txt" "${tmp}/checksums.txt" 2>/dev/null; then
	want=$(grep " ${asset}\$" "${tmp}/checksums.txt" 2>/dev/null | awk '{print $1}')
	if [ -n "${want:-}" ]; then
		if have sha256sum; then got=$(sha256sum "${tmp}/${BINARY}" | awk '{print $1}')
		elif have shasum; then got=$(shasum -a 256 "${tmp}/${BINARY}" | awk '{print $1}')
		else got=""; fi
		if [ -n "$got" ] && [ "$got" != "$want" ]; then
			err "checksum mismatch (expected $want, got $got)"
		fi
		[ -n "$got" ] && printf 'Checksum OK\n'
	fi
fi
chmod +x "${tmp}/${BINARY}"

# Pick an install directory: a writable /usr/local/bin, else escalate to it, else
# fall back to ~/.local/bin (no root needed).
SUDO=""
if [ -n "$BINDIR" ]; then
	:
elif [ -w /usr/local/bin ]; then
	BINDIR=/usr/local/bin
elif have sudo; then
	BINDIR=/usr/local/bin; SUDO=sudo
elif have doas; then
	BINDIR=/usr/local/bin; SUDO=doas
else
	BINDIR="${HOME}/.local/bin"
fi

if [ -n "$SUDO" ]; then
	$SUDO mkdir -p "$BINDIR" && $SUDO mv "${tmp}/${BINARY}" "${BINDIR}/${BINARY}"
else
	mkdir -p "$BINDIR" && mv "${tmp}/${BINARY}" "${BINDIR}/${BINARY}"
fi || err "could not install to ${BINDIR} (set BINDIR=... to choose another)"

printf 'Installed %s to %s\n' "$BINARY" "${BINDIR}/${BINARY}"
case ":$PATH:" in
	*":$BINDIR:"*) ;;
	*) printf '\nNote: %s is not on your PATH. Add it with:\n  export PATH="%s:$PATH"\n' "$BINDIR" "$BINDIR" ;;
esac
printf '\nRun it with:  %s\n' "$BINARY"
