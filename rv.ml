type 'a ref = { mutable contents : 'a }
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"
external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"
external incr : (int ref[@local_opt]) -> unit = "%incr"
external decr : (int ref[@local_opt]) -> unit = "%decr"
external ( +. ) : (float[@local_opt]) -> (float[@local_opt]) -> float = "%addfloat"

let[@inline] f r c1 c2 =
  (c1 [@inlined never]) !r;
  r := !r +. 1.;
  (c2 [@inlined never]) !r;
  r := !r +. 1.;
  !r

let g x =
  let r = ref x in
  let y = f r (fun _ -> ()) (fun _ -> ()) in
  y +. 1.
