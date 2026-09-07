(* See specialise_lifted_function_lib.ml. *)

let[@inline] map_stack (f @ local) (l @ local) = exclave_
  let[@inline available] rec loop (l @ local) =
    match l with
    | [] -> []
    | x :: xs ->
      exclave_
      let y = f x in
      let ys = loop xs in
      y :: ys
  in
  loop l
