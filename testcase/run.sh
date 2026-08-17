#!/bin/bash
# Demonstrates: compiling the *callee's* unit with -flambda2-reaper turns the
# exported code for a lambda-lifted inner recursive function from Code_present
# into Metadata_only, which breaks [@zero_alloc] checks in caller units (the
# caller can no longer duplicate + specialize the loop locally, so a direct
# call to the callee unit's generic loop remains, for which no zero_alloc
# summary exists).
#
# Only lib_a.ml's flags differ between the two runs; main.ml is compiled
# identically both times.  No dependencies beyond the compiler itself.
set -u
OBJINFO=${OBJINFO:-$(dirname "$COMPILER")/ocamlobjinfo}
cd "$(dirname "$0")"

echo "compiler: $("$COMPILER" -version)"
echo

run() {
  echo "===== lib_a.ml compiled with: -O3 $*"
  rm -f ./*.cm* ./*.o
  "$COMPILER" -c -O3 "$@" lib_a.ml || exit 1
  form=$("$OBJINFO" lib_a.cmx | sed 's/\x1b\[[0-9;]*m//g' \
    | grep -A1 '(camlLib_a__loop' | grep -m1 -o 'Code_present\|Metadata_only')
  echo "  lib_a.cmx export info for the inner loop: $form"
  if "$COMPILER" -c -O3 -zero-alloc-checker-details-extra main.ml; then
    echo "  main.ml zero_alloc check: PASSED"
    echo "  loop symbols in main.o (local specialized copy):"
    nm main.o | grep -i loop | sed 's/^/    /'
  else
    echo "  main.ml zero_alloc check: FAILED (error above)"
  fi
  echo
}

run
run -flambda2-reaper
