external make_unboxed_pair_v_v : 'a -> 'b -> ('a, 'b) unboxed_pair
external unboxed_pair_field_0_v_v : ('a, 'b) unboxed_pair -> 'a
external unboxed_pair_field_1_v_v : ('a, 'b) unboxed_pair -> 'b

let f i x y =
  let p = make_unboxed_pair_v_v x y in
  if i < 0 then unboxed_pair_field_0_v_v p
  else unboxed_pair_field_1_v_v p

