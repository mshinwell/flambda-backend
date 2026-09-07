(* The callbacks are still unknown when these functors are compiled.  The
   carriers for their lifted functions must survive this intermediate unit. *)

module[@inline] Helper (X : Specialise_lifted_functor_helper_lib.S) = struct
  include Specialise_lifted_functor_helper_lib.Make (X)
end

module[@inline] Body (X : Specialise_lifted_functor_body_lib.S) = struct
  include Specialise_lifted_functor_body_lib.Make (X)
end
