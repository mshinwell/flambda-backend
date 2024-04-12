(* TEST
readonly_files = "marshall_for_w53.ml w53.ml w53_zero_alloc_all.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/marshall_for_w53.exe"
all_modules = "marshall_for_w53.ml"
*** run
**** ocamlc.byte
flags = "-w +A-60-70"
module = "w53.marshalled.ml"
compiler_reference = "${test_source_directory}/w53.compilers.reference"
***** check-ocamlc.byte-output
**** setup-ocamlc.byte-build-env
***** ocamlc.byte
flags = "-w +A-60-70"
module = "w53_zero_alloc_all.marshalled.ml"
compiler_reference = "${test_source_directory}/w53_zero_alloc_all.compilers.reference"
****** check-ocamlc.byte-output
*)

(* This tests that warning 53 happen appropriately when dealing with marshalled
   ASTs.  It does that by marshalling `w53.ml` to disk and then passing the
   marshalled ast to the compiler. *)
