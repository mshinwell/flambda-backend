# flambda-backend
The Flambda backend project for OCaml.

## Installation instructions

Only currently tested on Linux/x86-64 and macOS/x86-64.

One-time setup:
```
$ opam switch 4.11.1  # or "opam switch create 4.11.1" if you haven't got that switch already
$ eval $(opam env)
$ git clone https://github.com/ocaml-flambda/dune
$ cd dune  # We'll refer to this "dune" directory below as $DUNE_DIR
$ git checkout origin/special_dune
$ make release
$ cd ..
$ git clone https://github.com/ocaml-flambda/flambda-backend
$ cd flambda-backend
$ git checkout -b 4.11 origin/4.11
```

To build from clean, after one-time setup:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir --enable-middle-end=closure --with-dune=$DUNE_DIR/dune.exe
$ make  # or e.g. make -j8
$ make install
```

You can also specify `--enable-middle-end=flambda`.

Prior to `make install` you can do:
- `make runtest` to run the Flambda backend tests (which use dune);
- `make runtest-upstream` to run the upstream testsuite. The upstream
testsuite runs much faster if you install GNU parallel. This is likely
already present on Linux machines. On macOS, install Homebrew, then `brew
install parallel`.
- `make compare` to run the comparison script that finds differences
between the upstream and Flambda backend install trees.

There is also a `make ci` target (best run as e.g. `make -j8 ci`) which does a full build, test
and comparison run.  This is what is used for Github actions.

To rebuild after making changes, you can just type `make` (or `make -j8`, etc).

There is a special target `make hacking` which starts Dune in polling mode.  The rebuild
performed here is equivalent to `make ocamlopt` in the upstream distribution: it rebuilds the
compiler itself, but doesn't rebuild the stdlib or anything else with the new compiler.
This target is likely what you want for development of large features in the middle end or
backend.  Rebuild times for this target should be very fast.

