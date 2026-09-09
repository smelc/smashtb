#!/usr/bin/env bash
#
# Create the project-local opam switch and install the build dependencies.
# Everything lands in ./_opam, nothing touches the global opam installation.

set -euo pipefail
cd "$(dirname "$0")"

OCAML_VERSION=5.5.1

if [ ! -d _opam ]; then
  echo "Creating a local opam switch with OCaml ${OCAML_VERSION}..."
  opam switch create . "${OCAML_VERSION}" --repos=default --no-install -y
fi

eval "$(opam env --switch="$PWD" --set-switch)"
opam install -y dune brr js_of_ocaml-compiler

echo
echo "Done. OCaml $(ocaml -version | awk '{print $NF}') in $PWD/_opam"
echo "Run 'direnv allow' once so the switch loads automatically, then ./run.sh"
