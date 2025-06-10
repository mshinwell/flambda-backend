#!/usr/bin/env bash

set -x -eu -o pipefail

sudo apt-get install bubblewrap

OPAM="$HOME/opam"

curl -Lo $OPAM \
  https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-i686-linux
chmod +x $OPAM

$OPAM init -a

$OPAM switch create 5.2.0+ox \
  --repos "with-extensions=\
git+https://github.com/janestreet/opam-repository.git#with-extensions,default"

eval $($OPAM env --switch 5.2.0+ox)

# $OPAM install ocamlformat merlin ocaml-lsp-server utop
$OPAM install ocaml-lsp-server

# Install extensions here rather than via customizations.vscode.extensions
# in .devcontainer.json, so the OPAM environment is set up first, to avoid
# various popups etc.
code="$(ls ~/.vscode-server*/bin/*/bin/code-server* | head -n 1)"
if [ ! -z "$code" ]; then
  $code --install-extension ocamllabs.ocaml-platform
  $code --install-extension ms-vscode.cpptools
  $code --install-extension eamodio.gitlens
else
  ls -lR ~ | grep code-server
fi
