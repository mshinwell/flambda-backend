type 'a ref = { mutable contents : 'a }
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"
external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"
external incr : (int ref[@local_opt]) -> unit = "%incr"
external decr : (int ref[@local_opt]) -> unit = "%decr"
external ( +. ) : (float[@local_opt]) -> (float[@local_opt]) -> float = "%addfloat"

let[@inline] f r c1 c2 =
  r := (c1 [@inlined hint]) !r;
  r := !r +. 1.;
  (c2 [@inlined never]) r;
  r := !r +. 2.;
  !r

let g x =
  let r = ref (x +. x) in
  let y = f r (fun f -> f +. 5.) (fun _ -> ()) in
  y +. 3.

(*

let stock = ref (ref 0)

let g r = stock := r
let h () = !stock := 0

let f () =
  let r = ref 1 in
  incr r;
  g r;
  incr r;
  h ();
  !r

*)
