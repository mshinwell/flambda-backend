#!/bin/bash

set -eu -o pipefail

if [ "$(uname)" != "Linux" ]; then
  echo "This script should be run on a Linux machine."
  exit 1
fi

if ! which patdiff > /dev/null 2>&1 ; then
  echo "Please install patdiff."
  exit 1
fi

# Installation root (--prefix) for the upstream and Flambda backend compilers
upstream_tree=$(pwd)/ocaml-install
flambda_backend_tree=$(pwd)/flambda-backend-install

# These filenames are the ones from the Flambda backend install tree.
# ocamloptcomp.a will diverge in time, but it's ok to compare right now.
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
  compiler-libs/ocamloptcomp.a \
  raw_spacetime_lib.a \
  stdlib.a \
  str.a \
  threads/threads.a \
  unix.a \
  raw_spacetime_lib.a \
  libraw_spacetime_lib_stubs.a \
  libraw_spacetime_lib_stubs_native.a \
  libthreads_stubs.a \
  libthreads_stubs_native.a \
  libunix_stubs.a \
  libunix_stubs_native.a \
  libstr_stubs.a \
  libstr_stubs_native.a
  "

# We don't currently check dynlink.a because the build process is quite
# different and we currently have a source code patch in the Flambda backend
# to work around limitations of Dune.

# compiler-libs/ocamlmiddleend.a is not built in the Flambda backend.
# We should try to remove this from upstream by fixing the ocamlobjinfo
# problem.  Note that ocamloptcomp.a contains the middle end, both in
# the Flambda backend and upstream builds.

# These filenames are the ones from the Flambda backend install tree.
stublibs_to_compare="\
  dllraw_spacetime_lib_stubs.so \
  dllstr_stubs.so \
  dllthreads_stubs.so \
  dllunix_stubs.so
  "

upstream_filename_of_archive_member () {
  filename=$1

  case "$filename" in
    cSEgen.o) echo CSEgen.o ;;
    cSE.o) echo CSE.o ;;
    st_stubs_byte.o) echo st_stubs_b.o ;;
    st_stubs_native.o) echo st_stubs_n.o ;;
    *) echo $filename ;;
  esac
}

upstream_filenames_of_archive_members () {
  # Dune doesn't follow the capitalisation convention of the compiler for
  # build artifacts.  This seems like a bug, but it should be harmless in
  # the following cases.
  while read ar_output; do
    filename=$(echo $ar_output | awk '{print $3}')
    upstream_filename_of_archive_member "$filename"
  done
}

upstream_filename_of_archive () {
  filename=$1

  case "$filename" in
    libthreads_stubs.a) echo libthreads.a ;;
    libthreads_stubs_native.a) echo libthreadsnat.a ;;
    libraw_spacetime_lib_stubs.a) echo libraw_spacetime_lib.a ;;
    libraw_spacetime_lib_stubs_native.a) echo libraw_spacetime_lib.a ;;
    libunix_stubs.a) echo libunix.a ;;
    libunix_stubs_native.a) echo libunix.a ;;
    libstr_stubs.a) echo libcamlstr.a ;;
    libstr_stubs_native.a) echo libcamlstr.a ;;
    *) echo $filename ;;
  esac
}

upstream_filename_of_stublibs () {
  filename=$1

  case "$filename" in
    dllraw_spacetime_lib_stubs.so) echo dllraw_spacetime_lib.so ;;
    dllstr_stubs.so) echo dllcamlstr.so ;;
    dllthreads_stubs.so) echo dllthreads.so ;;
    dllunix_stubs.so) echo dllunix.so ;;
    *) echo $filename ;;
  esac
}

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
  symbols_all=$3

  # We expect there may be some discrepancies in camlinternalMenhirLib,
  # but those are harmless.
  # The script will fail if there are differences in symbols modulo stamps.
  # If there are differences in stamps, those will be printed, but the script
  # won't fail.

  # We currently ignore references to ceil () and nextafter () which in
  # the Unix bytecode stublibs seem to be referencing the non-versioned
  # symbols instead of the glibc-versioned symbols.  This is probably some
  # artifact of exactly how the libraries were produced but seems harmless.

  nm $file \
    | sed 's/^...................//' \
    | grep -v '^.LC[0-9]*$' \
    | grep -v '^.L[0-9]*$' \
    | sed 's/_[0-9]*$//' \
    | grep -v '^ceil@@GLIBC' \
    | grep -v '^ceil$' \
    | grep -v '^nextafter@@GLIBC' \
    | grep -v '^nextafter$' \
    > $symbols

  nm $file \
    | sed 's/^...................//' \
    | grep -v '^ceil@@GLIBC' \
    | grep -v '^ceil$' \
    | grep -v '^nextafter@@GLIBC' \
    | grep -v '^nextafter$' \
    | grep -v '^camlCamlinternalMenhirLib__' \
    > $symbols_all
}

compare_object_file_symbols () {
  upstream_file=$1
  flambda_backend_file=$2

  upstream_symbols=$(mktemp)
  flambda_backend_symbols=$(mktemp)

  upstream_symbols_all=$(mktemp)
  flambda_backend_symbols_all=$(mktemp)

  list_object_file_symbols $upstream_file $upstream_symbols \
    $upstream_symbols_all
  list_object_file_symbols $flambda_backend_file $flambda_backend_symbols \
    $flambda_backend_symbols_all

  patdiff $upstream_symbols $flambda_backend_symbols \
    || (echo "Symbols do not match."; \
        rm -f $upstream_symbols $flambda_backend_symbols; \
        rm -f $upstream_symbols_all $flambda_backend_symbols_all; \
        exit 1)

  patdiff $upstream_symbols_all $flambda_backend_symbols_all || true

  rm -f $upstream_symbols $flambda_backend_symbols
  rm -f $upstream_symbols_all $flambda_backend_symbols_all
}

compare_archive () {
  archive=$1
  upstream_archive=$(upstream_filename_of_archive $archive)

  if [ "$archive" = "$upstream_archive" ]; then
    echo "Comparing archive: $archive"
  else
    echo "Comparing archive: $archive (upstream: $upstream_archive)"
  fi

  upstream=$(mktemp -d)
  flambda_backend=$(mktemp -d)

  upstream_contents=$(mktemp)
  flambda_backend_contents=$(mktemp)

  upstream_archive=$upstream_tree/lib/ocaml/$upstream_archive
  flambda_backend_archive=$flambda_backend_tree/lib/ocaml/$archive

  ensure_exists $upstream_archive
  ensure_exists $flambda_backend_archive

  cd $upstream \
    && ar xv $upstream_archive | sort > $upstream_contents
  cd $flambda_backend \
    && ar xv $flambda_backend_archive | sort > $flambda_backend_contents

  patdiff <(cat $upstream_contents | awk '{print $3}') \
    <(cat $flambda_backend_contents | upstream_filenames_of_archive_members) \
    || (echo "File names inside archive $archive do not match"; \
        rm -rf $upstream; \
        rm -rf $flambda_backend; \
        rm -f $upstream_contents; \
        rm -f $flambda_backend_contents; \
        exit 1)

  files=$(ls $flambda_backend)

  for file in $files; do
    upstream_file=$(upstream_filename_of_archive_member $file)

    if [ "$file" = "$upstream_file" ]; then
      echo "... Comparing symbols in $file"
    else
      echo "... Comparing symbols in $file (upstream $upstream_file)"
    fi

    compare_object_file_symbols $upstream/$upstream_file $flambda_backend/$file
  done

  rm -rf $upstream
  rm -rf $flambda_backend
  rm -f $upstream_contents
  rm -f $flambda_backend_contents
}

compare_stublibs () {
  stublibs=$1
  upstream_stublibs=$(upstream_filename_of_stublibs $stublibs)

  if [ "$stublibs" = "$upstream_stublibs" ]; then
    echo "Comparing stublibs: $stublibs"
  else
    echo "Comparing stublibs: $stublibs (upstream: $upstream_stublibs)"
  fi

  upstream_stublibs=$upstream_tree/lib/ocaml/stublibs/$upstream_stublibs
  flambda_backend_stublibs=$flambda_backend_tree/lib/ocaml/stublibs/$stublibs

  ensure_exists $upstream_stublibs
  ensure_exists $flambda_backend_stublibs

  compare_object_file_symbols $upstream_stublibs $flambda_backend_stublibs
}

# 1. Check immediate subdirs of installation root match (just the names of
# the subdirs, not the contents).

echo "** Immediate subdirs of installation root"

# The Flambda backend does not build or install man pages.
upstream_subdirs=$(ls -1 $upstream_tree | grep -v '^man$')
flambda_backend_subdirs=$(ls -1 $flambda_backend_tree)

if [ "$upstream_subdirs" != "$flambda_backend_subdirs" ]; then
  echo -e "Subdirs of install tree don't match, expected:\n$upstream_subdirs"
  exit 1
fi

# 2. Check that everything provided upstream in the bin/ directory that we
# want is present.

echo "** Executables in bin/"

upstream_bin=$(ls $upstream_tree/bin \
  | grep -v '^ocamlcp$' \
  | grep -v '^ocamlcp.byte$' \
  | grep -v '^ocamlcp.opt$' \
  | grep -v '^ocamloptp$' \
  | grep -v '^ocamloptp.byte$' \
  | grep -v '^ocamloptp.opt$' \
  | grep -v '^ocamlprof$' \
  | grep -v '^ocamlprof.byte$' \
  | grep -v '^ocamlprof.opt$' \
  )

flambda_backend_bin=$(ls $flambda_backend_tree/bin)

if [ "$upstream_bin" != "$flambda_backend_bin" ]; then
  echo "Executables in bin/ don't match:"
  patdiff <(echo $upstream_bin) <(echo $flambda_backend_bin)
  exit 1
fi

# 3. Make sure executables in lib/ocaml/ are present.

echo "** Executables in lib/ocaml/"

lib_exes="expunge extract_crc objinfo_helper"

for exe in $lib_exes; do
  if [ ! -f $upstream_tree/lib/ocaml/$exe ]; then
    echo "Executable $exe in lib/ocaml/ not found in upstream tree"
    exit 1
  fi
  if [ ! -f $flambda_backend_tree/lib/ocaml/$exe ]; then
    echo "Executable $exe in lib/ocaml/ not found in Flambda backend tree"
    exit 1
  fi
done

# 4. Check that immediate subdirs of lib/ match (just the names, not contents).

echo "** Immediate subdirs of lib/"

upstream_subdirs=$(ls -1 $upstream_tree/lib)
flambda_backend_subdirs=$(ls -1 $flambda_backend_tree/lib)

if [ "$upstream_subdirs" != "$flambda_backend_subdirs" ]; then
  echo -e "Subdirs of install tree don't match, expected:\n$upstream_subdirs"
  exit 1
fi

# 5. Check that the VERSION files match.

echo "** VERSION files"

upstream_version=$(cat $upstream_tree/lib/ocaml/VERSION)
flambda_backend_version=$(cat $flambda_backend_tree/lib/ocaml/VERSION)

if [ "$upstream_version" != "$flambda_backend_version" ]; then
  echo -e "VERSION files in lib/ocaml/ do not match:"
  patdiff <(echo $upstream_version) <(echo $flambda_backend_version)
  exit 1
fi

# 6. Check .ml and .mli files in lib/ocaml/ are all present and identical.

echo "** .ml and .mli files in lib/ocaml/"

upstream_files=$(cd $upstream_tree/lib/ocaml/ && ls *.ml{,i})
for file in $upstream_files; do
  if [ ! -f "$flambda_backend_tree/lib/ocaml/$file" ]; then
    echo "lib/ocaml/$file is missing"
    exit 1
  fi
  patdiff $upstream_tree/lib/ocaml/$file \
    $flambda_backend_tree/lib/ocaml/$file
done

# 7. Check all files in lib/ocaml/caml, the runtime headers, are identical.

echo "** All files in lib/ocaml/caml/"

upstream_files=$(cd $upstream_tree/lib/ocaml/caml/ && ls)
for file in $upstream_files; do
  if [ ! -f "$flambda_backend_tree/lib/ocaml/caml/$file" ]; then
    echo "lib/ocaml/$file is missing"
    exit 1
  fi
  patdiff $upstream_tree/lib/ocaml/caml/$file \
    $flambda_backend_tree/lib/ocaml/caml/$file
done

# 8. Check .a files match (archive filenames, archive member filenames, symbols
# defined by archive members).  Some rewrites are performed as above.  This is
# the closest we get to comparing compiled code.

echo "** Archive filenames, members and symbols"

for archive in $archives_to_compare; do
  compare_archive $archive
done

# 9. Check .so bytecode stubs files match (in lib/ocaml/stublibs/).

echo "** Bytecode stubs .so files"

for stublibs in $stublibs_to_compare; do
  compare_stublibs $stublibs
done

