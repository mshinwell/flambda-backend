#!/usr/bin/env bash

set -eu -o pipefail

apt-get install opam

opam switch create 5.2.0+ox \
  --repos "with-extensions=\
git+https://github.com/janestreet/opam-repository.git#with-extensions,default"

eval $(opam env --switch 5.2.0+ox)

opam install ocamlformat merlin ocaml-lsp-server utop
