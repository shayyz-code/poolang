#!/usr/bin/env sh
set -eu

REPO="${POO_REPO:-shayyz-code/poolang}"
INSTALL_DIR="${POO_INSTALL_DIR:-/usr/local/bin}"
REQUESTED_VERSION="${1:-latest}"

detect_os() {
  case "$(uname -s)" in
    Linux) echo "Linux" ;;
    Darwin) echo "Darwin" ;;
    *)
      echo "Unsupported OS: $(uname -s)" >&2
      exit 1
      ;;
  esac
}

detect_arch() {
  case "$(uname -m)" in
    x86_64|amd64) echo "x86_64" ;;
    arm64|aarch64) echo "arm64" ;;
    *)
      echo "Unsupported architecture: $(uname -m)" >&2
      exit 1
      ;;
  esac
}

resolve_tag() {
  if [ "${REQUESTED_VERSION}" = "latest" ]; then
    curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
      | sed -n 's/.*"tag_name":[[:space:]]*"\([^"]*\)".*/\1/p' \
      | head -n1
  else
    case "${REQUESTED_VERSION}" in
      v*) echo "${REQUESTED_VERSION}" ;;
      *) echo "v${REQUESTED_VERSION}" ;;
    esac
  fi
}

OS="$(detect_os)"
ARCH="$(detect_arch)"
TAG="$(resolve_tag)"

if [ -z "${TAG}" ]; then
  echo "Failed to resolve release tag." >&2
  exit 1
fi

VERSION="${TAG#v}"

if [ "${OS}" = "Linux" ] && [ "${ARCH}" = "arm64" ]; then
  echo "Linux arm64 artifacts are not published yet." >&2
  exit 1
fi

ASSET="poo_${VERSION}_${OS}_${ARCH}.tar.gz"
DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${TAG}/${ASSET}"

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "${tmp_dir}"
}
trap cleanup EXIT INT TERM

echo "Downloading ${DOWNLOAD_URL}"
curl -fL "${DOWNLOAD_URL}" -o "${tmp_dir}/${ASSET}"
tar -xzf "${tmp_dir}/${ASSET}" -C "${tmp_dir}"

BIN_PATH="${tmp_dir}/poo"
if [ ! -f "${BIN_PATH}" ]; then
  BIN_PATH="$(find "${tmp_dir}" -type f -name poo | head -n1 || true)"
fi

if [ -z "${BIN_PATH}" ] || [ ! -f "${BIN_PATH}" ]; then
  echo "Failed to locate extracted poo binary." >&2
  exit 1
fi

mkdir -p "${INSTALL_DIR}" 2>/dev/null || true
if [ -w "${INSTALL_DIR}" ]; then
  cp "${BIN_PATH}" "${INSTALL_DIR}/poo"
  chmod +x "${INSTALL_DIR}/poo"
else
  sudo cp "${BIN_PATH}" "${INSTALL_DIR}/poo"
  sudo chmod +x "${INSTALL_DIR}/poo"
fi

echo "Installed poo to ${INSTALL_DIR}/poo"
echo "Run: poo <path_to_your_source_file>"
