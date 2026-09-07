module type S = sig
  val f : int -> int
end

(* A functor marked [@inline] containing a non-recursive helper that is never
   inlined and is not exported from the functor's result.  At -O4 the reaper
   lambda-lifts [helper], turning its value slot for [X.f] into a parameter.
   When the functor is inlined in another module, [helper] must still be
   specialised on the actual [X.f] (which is then inlined into the specialised
   copy), even though the copy itself is never inlined. *)
module[@inline] Make (X : S) : sig
  val apply_twice : int -> int
end = struct
  let[@inline never] helper x = X.f (X.f x)

  let[@inline] apply_twice x = helper x
end
