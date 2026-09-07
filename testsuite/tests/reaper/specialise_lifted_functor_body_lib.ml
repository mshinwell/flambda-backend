module type S = sig
  val f : int -> int
end

(* A functor marked [@inline] containing a function, [loop], which is never
   inlined (it is recursive with a non-tail self call) and is not exported
   from the functor's result.  At -O4 the reaper lambda-lifts [loop], turning
   its value slot for [X.f] into a parameter.  When the functor is inlined in
   another module, [loop] must still be specialised on the actual [X.f]. *)
module[@inline] Make (X : S) : sig
  val map : int list @ local -> int list @ local
end = struct
  let[@inline available] rec loop (l @ local) = exclave_
    match l with
    | [] -> []
    | x :: xs ->
      let y = X.f x in
      let ys = loop xs in
      y :: ys

  let[@inline] map (l @ local) = exclave_ loop l
end
