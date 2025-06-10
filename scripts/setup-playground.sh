#!/usr/bin/env bash

set -eu -o pipefail

sudo apt-get install opam

opam switch create 5.2.0+ox \
  --repos "with-extensions=\
git+https://github.com/janestreet/opam-repository.git#with-extensions,default"

eval $(opam env --switch 5.2.0+ox)

opam install ocamlformat merlin ocaml-lsp-server utop

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
