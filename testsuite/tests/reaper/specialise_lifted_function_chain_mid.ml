(* An inlinable wrapper, in a third module, around the function whose inner
   [loop] has been lambda-lifted.  [f] is unknown here, so [loop] cannot be
   specialised in this module; the information needed to specialise it must
   survive into the inlinable body of [my_map]. *)

let[@inline] my_map (f @ local) (l @ local) = exclave_
  Specialise_lifted_function_lib.map_stack f l
