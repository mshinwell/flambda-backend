(* TEST *)

(* Tests that an exception in the alloc_minor callback, during a
   combined allocation, causes already-run allocation callbacks to
   be reflected by deallocation callbacks. *)

  arr


let _ = raise_in_alloc ()
