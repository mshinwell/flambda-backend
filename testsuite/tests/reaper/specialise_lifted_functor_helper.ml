(* TEST
   modules = "specialise_lifted_functor_helper_lib.ml";
   flambda2;
   flags += "-O4";
   { native; }
 *)

module M = Specialise_lifted_functor_helper_lib.Make (struct
  let f x = x * x
end)

(* [M.apply_twice] is inlined, leaving a direct call to the lifted [helper].
   The check fails if that call goes to the generic [helper] (which calls an
   unknown [X.f]) rather than to a copy specialised on [x * x]. *)
let[@zero_alloc] fourth_power x = M.apply_twice x

let () = Printf.printf "%d\n" (fourth_power 3)
