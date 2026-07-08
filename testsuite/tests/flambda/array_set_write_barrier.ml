(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 {
   flat-float-array;
   check-fexpr-dump;
 }{
   no-flat-float-array;
   fexpr_reference_suffix = "no-flat-float-array.reference";
   check-fexpr-dump;
 }
*)

(* Check that when a generic [Array.unsafe_set] is specialised to an array
   known (from its value kind) to contain only immediates, the GC write
   barrier is removed too: the [Array_set] primitive in [mono_arr_int] must
   have the [Immediates] set kind (printed as just [%array_set.imm]), not a
   [Values] set kind (printed as [%array_set.imm.value]), which would cause
   [caml_modify] to be called. *)

[@@@ocaml.flambda_o3]

let poly_arr a x = Array.unsafe_set a 0 x

let mono_arr_int (r : int array) (x : int) = poly_arr r x
