(* TEST
 flambda2;
 flags += "-Oclassic -flambda2-reaper -X reaper-oclassic=1 -reaper-local-fields -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-reaper;
 check-fexpr-dump;
*)

(* When the closures of a set are unboxed inside a function, the set of
   closures left behind to record the specialisation of the lifted functions
   only declares the functions that are used: [g] is never called, so it has
   no declaration there (rather than a deleted one, which the fexpr printer
   does not support). *)

[@@@ocaml.warning "-26"]

let[@inline never] outer y n =
  let rec f x = if x = 0 then y else f (x - 1)
  and g z = f z + g (z - 1) in
  f n

let _ = outer 5 3
