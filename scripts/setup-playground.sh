#!/usr/bin/env bash
#
# set -x -eu -o pipefail
#
# curl -L -o /tmp/autoconf.tar.gz \
#   https://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz
# tar fxz /tmp/autoconf.tar.gz
# cd autoconf-2.71
# ./configure --prefix=$HOME/autoconf-2.71-install
# make
# make install
# export PATH=$HOME/autoconf-2.71-install/bin:$PATH
#
# sudo apt-get update
# sudo apt-get install -y opam
#
# opam init -a
#
# opam switch create 5.2.0+flambda2 --yes \
#   --repos "with-extensions=\
# git+https://github.com/janestreet/opam-repository.git#with-extensions,default"
#
# eval $(opam env --switch 5.2.0+flambda2)
#
# opam install --yes ocamlformat merlin ocaml-lsp-server utop
#
# # Install extensions here rather than via customizations.vscode.extensions
# # in .devcontainer.json, so the OPAM environment is set up first, to avoid
# # various popups etc.
# code="$(ls ~/.vscode-server*/bin/*/bin/code-server* | head -n 1)"
# if [ ! -z "$code" ]; then
#   $code --install-extension ocamllabs.ocaml-platform
#   $code --install-extension ms-vscode.cpptools
#   $code --install-extension eamodio.gitlens
# else
#   ls -lR ~ | grep code-server
# fi
#
