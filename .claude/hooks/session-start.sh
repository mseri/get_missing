#!/bin/bash
set -euo pipefail

# Only run in remote Claude Code sessions
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Install OCaml, opam, and libgmp (needed by zarith/opam-state)
apt-get install -y --fix-missing opam libgmp-dev 2>/dev/null

# Initialize opam using the GitHub mirror of opam-repository,
# since opam.ocaml.org is blocked in this network environment.
if [ ! -f "$HOME/.opam/config" ]; then
  opam init --disable-sandboxing --no-setup -y \
    git+https://github.com/ocaml/opam-repository
fi

eval "$(opam env)"

# Update and unpack the opam repository index if not already done.
# opam 2.1+ stores the repo as a .tar.gz; the code reads packages
# directly from the filesystem, so we need the extracted tree.
if [ ! -d "$HOME/.opam/repo/default/packages" ]; then
  opam update 2>/dev/null || true
  if [ -f "$HOME/.opam/repo/default.tar.gz" ]; then
    tar xzf "$HOME/.opam/repo/default.tar.gz" -C "$HOME/.opam/repo/"
  fi
fi

# Several packages are hosted on erratique.ch or ocaml.janestreet.com,
# both of which are blocked. Pin them to their GitHub release archives.
pin_from_github() {
  local pkg=$1 url=$2
  opam pin list --short 2>/dev/null | grep -q "^$pkg$" \
    || opam pin add -y --no-action "$pkg" "$url" 2>/dev/null
}

pin_from_github uutf        "https://github.com/dbuenzli/uutf/archive/refs/tags/v1.0.4.tar.gz"
pin_from_github topkg       "https://github.com/dbuenzli/topkg/archive/refs/tags/v1.1.1.tar.gz"
pin_from_github jsonm       "https://github.com/dbuenzli/jsonm/archive/refs/tags/v1.0.2.tar.gz"
pin_from_github cmdliner    "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v2.1.1.tar.gz"
pin_from_github logs        "https://github.com/dbuenzli/logs/archive/refs/tags/v0.10.0.tar.gz"
pin_from_github ptime       "https://github.com/dbuenzli/ptime/archive/refs/tags/v1.2.0.tar.gz"
pin_from_github fpath       "https://github.com/dbuenzli/fpath/archive/refs/tags/v0.7.3.tar.gz"
pin_from_github rresult     "https://github.com/dbuenzli/rresult/archive/refs/tags/v0.7.0.tar.gz"
pin_from_github astring     "https://github.com/dbuenzli/astring/archive/refs/tags/v0.8.5.tar.gz"
pin_from_github bos         "https://github.com/dbuenzli/bos/archive/refs/tags/v0.3.0.tar.gz"
pin_from_github fmt         "https://github.com/dbuenzli/fmt/archive/refs/tags/v0.11.0.tar.gz"
pin_from_github sexplib0    "https://github.com/janestreet/sexplib0/archive/refs/tags/v0.16.0.tar.gz"
pin_from_github ppx_sexp_conv "https://github.com/janestreet/ppx_sexp_conv/archive/refs/tags/v0.16.0.tar.gz"
# ppxlib 0.38 breaks ppx_sexp_conv v0.16; pin to a compatible version.
pin_from_github ppxlib      "https://github.com/ocaml-ppx/ppxlib/archive/refs/tags/0.32.1.tar.gz"

# Install all project dependencies plus TLS for HTTPS fetches
opam install -y --assume-depexts \
  clz cohttp-lwt-unix opam-state uri tls-lwt

# Verify the project builds
cd "${CLAUDE_PROJECT_DIR:-$(dirname "$(dirname "$0")")}"
dune build
