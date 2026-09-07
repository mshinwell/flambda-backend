(* The non-tail recursive loop is lambda-lifted by reaper. Evaluate [f] before
   recursion so that inlining a nested fold cannot hide a lost redirection of
   the outer recursive call. *)

let[@inline] fold (f @ local) (l @ local) =
  let[@inline available] rec loop (l @ local) =
    match l with
    | [] -> 0
    | x :: xs ->
      let y = f x in
      let ys = loop xs in
      y + ys
  in
  loop l [@nontail]
