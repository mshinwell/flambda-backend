#!/bin/bash

set -eu -o pipefail

upstream_tree=$(pwd)/ocaml-install
flambda_backend_tree=$(pwd)/flambda-backend-install

archives_to_compare="\
  libasmrun.a \
  libasmrund.a \
  libasmruni.a \
  libasmrun_pic.a \
  libcamlrun.a \
  libcamlrund.a \
  libcamlruni.a \
  libcamlrun_pic.a \
  bigarray.a \
  compiler-libs/ocamlbytecomp.a \
  compiler-libs/ocamlcommon.a \
  raw_spacetime_lib.a \
  stdlib.a \
  str.a \
  threads/threads.a \
  unix.a \
  "

# dynlink.a

ensure_exists () {
  file=$1

  if [ ! -f "$file" ]; then
    echo "File $file is missing"
    exit 1
  fi
}

list_object_file_symbols () {
  file=$1
  symbols=$2

  # suffix removal:

  nm $file \
    | sed 's/^...................//' \
    | grep -v '^.LC[0-9]*$' \
    | grep -v '^.L[0-9]*$' \
    | sed 's/_[0-9]*$//' \
    > $symbols
}

compare_object_file_symbols () {
  upstream_file=$1
  flambda_backend_file=$2

  upstream_symbols=$(mktemp)
  flambda_backend_symbols=$(mktemp)

  list_object_file_symbols $upstream_file $upstream_symbols
  list_object_file_symbols $flambda_backend_file $flambda_backend_symbols

  patdiff $upstream_symbols $flambda_backend_symbols \
    || (echo "Symbols do not match."; \
        rm -f $upstream_symbols $flambda_backend_symbols; \
        exit 1)

  rm -f $upstream_symbols $flambda_backend_symbols
}

compare_archive () {
  archive=$1

  echo "Comparing archive: $archive"

  upstream=$(mktemp -d)
  flambda_backend=$(mktemp -d)

  upstream_contents=$(mktemp)
  flambda_backend_contents=$(mktemp)

  upstream_archive=$upstream_tree/$archive
  flambda_backend_archive=$flambda_backend_tree/$archive

  ensure_exists $upstream_archive
  ensure_exists $flambda_backend_archive

  cd $upstream \
    && ar xv $upstream_archive | sort > $upstream_contents
  cd $flambda_backend \
    && ar xv $flambda_backend_archive | sort > $flambda_backend_contents

  patdiff $upstream_contents $flambda_backend_contents \
    || (echo "File names inside archive $archive do not match"; \
        rm -rf $upstream; \
        rm -rf $flambda_backend; \
        rm -f $upstream_contents; \
        rm -f $flambda_backend_contents; \
        exit 1)

  files=$(ls $upstream)

  for file in $files; do
    echo "... Comparing symbols in $file"

    upstream_file=$upstream/$file
    flambda_backend_file=$flambda_backend/$file

    compare_object_file_symbols $upstream_file $flambda_backend_file
  done

  rm -rf $upstream
  rm -rf $flambda_backend
  rm -f $upstream_contents
  rm -f $flambda_backend_contents
}

#upstream_archives=$(find $upstream_tree -name "*.a" | sort)
#flambda_backend_archives=$(find $flambda_backend_tree -name "*.a" | sort)

for archive in $archives_to_compare; do
  compare_archive lib/ocaml/$archive
done

