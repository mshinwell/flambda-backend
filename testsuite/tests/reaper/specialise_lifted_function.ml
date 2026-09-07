(* TEST
   modules = "specialise_lifted_function_lib.ml";
   flambda2;
   flags += "-O4";
   { native; }
 *)

(* The reaper, when compiling the other module, lambda-lifts the inner [loop]
   of [map_stack].  After [map_stack] is inlined here, the simplifier must
   still be able to produce a copy of [loop] specialised on the concrete [f],
   otherwise the residual call is to the generic [loop] (which calls an
   unknown function) and the [@zero_alloc] check fails. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares =
    Specialise_lifted_function_lib.map_stack (fun x -> x * x) l
  in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
