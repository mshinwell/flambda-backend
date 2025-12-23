(* PC-relative addressing - C4.1.92.2 *)
let encode_adr ~op ~immlo ~immhi ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int immlo) 29) in
  let result = logor result (shift_left (of_int 0b10000) 24) in
  let result = logor result (shift_left (of_int immhi) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
