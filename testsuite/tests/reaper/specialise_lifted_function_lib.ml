(* Mimics the shape of [Base.List]'s stack [map]: an [@inline] wrapper whose
   body defines an inner recursive [loop], closed over the unknown [f].  The
   self call is not a tail call, so [loop] cannot be loopified and is never
   inlined: a direct call to it remains at every use site of [map_stack].

   At -O4 the reaper lambda-lifts [loop] (the value slot for [f] becomes a
   parameter), which must not prevent callers that inline [map_stack] from
   specialising [loop] on their own [f] (see the test in
   specialise_lifted_function.ml). *)

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
