(* TEST *)

(* Tests that an exception in the alloc_minor callback propagates
   correctly to the top level. *)

module MP = Gc.Memprof

let _ =

try
