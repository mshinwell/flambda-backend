(* TEST
   modules = "specialise_lifted_function_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O4 -dflambda-invariants";
   module = "specialise_lifted_function_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -no-flambda2-reaper -g -flambda2-expert-phantom-lets -dflambda-invariants";
   module = "";
   all_modules = "specialise_lifted_function_debug.ml";
   binary_modules = "specialise_lifted_function_lib";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

(* As specialise_lifted_function.ml, with phantom lets in this module: the
   roots that only phantom bindings use must neither keep the synthetic value
   slots of the imported specialisation site nor the site itself alive. (The
   reaper is not run on this module since it does not support phantom lets:
   it replaces any phantom binding by invalid code.) *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares = Specialise_lifted_function_lib.map_stack (fun x -> x * x) l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
