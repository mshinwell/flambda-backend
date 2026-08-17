(* Mimics the shape of [Base.List]'s stack [map]: an [@inline] wrapper whose
   exported inlinable body defines a lambda-lifted inner recursive loop, closed
   over an unknown [f].  The self call is deliberately *not* a tail call (we
   cons after the recursive call, so the loop cannot be loopified and hence can
   never be inlined: a direct call to it always remains at every use site of
   [map_stack]. *)

let[@inline] map_stack (f @ local) (l @ local) = exclave_
  let rec loop (l @ local) =
    match l with
    | [] -> []
    | x :: xs ->
      exclave_
      let y = f x in
      let ys = loop xs in
      y :: ys
  in
  loop l
