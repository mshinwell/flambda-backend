external make_unboxed_pair_v_v : 'a -> 'b -> ('a, 'b) unboxed_pair = "%make_unboxed_pair_v_v"
external unboxed_pair_field_0_v_v : ('a, 'b) unboxed_pair -> 'a = "%unboxed_pair_field_0_v_v"
external unboxed_pair_field_1_v_v : ('a, 'b) unboxed_pair -> 'b = "%unboxed_pair_field_1_v_v"

let f i x y =
  let p = make_unboxed_pair_v_v x y in
  if i < 0 then unboxed_pair_field_0_v_v p
  else unboxed_pair_field_1_v_v p

external make_unboxed_pair_vup_vup :
  ('a, 'b) unboxed_pair -> ('c, 'd) unboxed_pair
  -> (('a, 'b) unboxed_pair, ('c, 'd) unboxed_pair) unboxed_pair
  = "%make_unboxed_pair_vup_vup"

external unboxed_pair_field_0_vup_vup :
  (('a, 'b) unboxed_pair, _) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_0_vup_vup"

let g i x y =
  let p = make_unboxed_pair_v_v x y in
  let q = make_unboxed_pair_vup_vup p p in
  let p_again = unboxed_pair_field_0_vup_vup q in
  if i < 0 then unboxed_pair_field_0_v_v p_again
  else unboxed_pair_field_1_v_v p_again

external unboxed_pair_field_1_vup_vup :
  (_, ('a, 'b) unboxed_pair) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_1_vup_vup"

let h i x y =
  let p = make_unboxed_pair_v_v x y in
  let p' = make_unboxed_pair_v_v y x in
  let q = make_unboxed_pair_vup_vup p p' in
  let r =
    if i < 0 then unboxed_pair_field_0_vup_vup q
    else unboxed_pair_field_1_vup_vup q
  in
  if i < 0 then unboxed_pair_field_0_v_v r
  else unboxed_pair_field_1_v_v r

external make_unboxed_pair_i_i : 'a -> 'b -> ('a, 'b) unboxed_pair = "%make_unboxed_pair_i_i"
external unboxed_pair_field_0_i_i : ('a, 'b) unboxed_pair -> 'a = "%unboxed_pair_field_0_i_i"
external unboxed_pair_field_1_i_i : ('a, 'b) unboxed_pair -> 'b = "%unboxed_pair_field_1_i_i"

let[@inline never] takes_unboxed_pair (p : (int, int) unboxed_pair) =
  let p0 = unboxed_pair_field_0_i_i p in
  let p1 = unboxed_pair_field_1_i_i p in
  p0 + p1

let caller x y =
  let p = make_unboxed_pair_i_i x y in
  takes_unboxed_pair p

let[@inline never] takes_two_unboxed_pairs
      (p : (int, int) unboxed_pair)
      (q : (int, int) unboxed_pair) =
  let p0 = unboxed_pair_field_0_i_i p in
  let p1 = unboxed_pair_field_1_i_i p in
  let q0 = unboxed_pair_field_0_i_i p in
  let q1 = unboxed_pair_field_1_i_i p in
  p0 + p1 + q0 + q1

let caller2 x y =
  let p = make_unboxed_pair_i_i x y in
  let q = make_unboxed_pair_i_i y x in
  takes_two_unboxed_pairs p q

