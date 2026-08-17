(* A [@zero_alloc]-checked function that uses the other unit's stack [map] with
   a concrete, heap-allocation-free [f].  All allocations on this path are
   stack allocations, so the check should succeed provided the checker can see
   through (or find a summary for) the residual call to [Lib_a]'s inner loop. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares = Lib_a.map_stack (fun x -> x * x) l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]
