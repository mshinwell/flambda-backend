(* TEST
   modules = "specialise_lifted_function_lib.ml specialise_lifted_function_chain_mid.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O4 -dflambda-invariants";
   module = "specialise_lifted_function_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -dflambda-invariants";
   module = "specialise_lifted_function_chain_mid.ml";
   ocamlopt.opt;
   module = "";
   all_modules = "specialise_lifted_function_o3.ml";
   binary_modules = "specialise_lifted_function_lib specialise_lifted_function_chain_mid";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

(* Only the library is compiled with the reaper. The intermediate module and
   this one are compiled at -O3, as most code is: the specialisation site left
   behind by the reaper must survive the intermediate module without the
   reaper, and be used here. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares =
    Specialise_lifted_function_chain_mid.my_map (fun x -> x * x) l
  in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
