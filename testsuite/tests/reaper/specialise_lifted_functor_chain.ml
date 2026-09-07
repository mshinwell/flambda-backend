(* TEST
   modules = "specialise_lifted_functor_helper_lib.ml specialise_lifted_functor_body_lib.ml specialise_lifted_functor_chain_mid.ml";
   flambda2;
   flags += "-O4";
   { native; }
 *)

module Helper = Specialise_lifted_functor_chain_mid.Helper (struct
  let f x = x * x
end)

module Body = Specialise_lifted_functor_chain_mid.Body (struct
  let f x = x * x
end)

(* Both checks fail if the intermediate functor loses the information needed
   to specialise the lifted functions on the concrete callback. *)
let[@zero_alloc] fourth_power x = Helper.apply_twice x

let[@zero_alloc] sum_squares (l @ local) =
  let squares = Body.map l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () =
  Printf.printf "%d %d\n" (fourth_power 0) (fourth_power 3);
  Printf.printf "%d %d\n" (sum_squares []) (sum_squares [1; 2; 3; 4])
