(* Move wide (immediate) encoding - C4.1.92.7 Used for MOVN, MOVZ, MOVK *)
let encode_move_wide ~sf ~opc ~hw ~imm16 ~rd =
  let open Int32 in
  if imm16 < 0 || imm16 > 0xFFFF
  then Misc.fatal_errorf "MOVZ/MOVN/MOVK immediate out of range: %d" imm16 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100101) 23) in
  let result = logor result (shift_left (of_int hw) 21) in
  let result = logor result (shift_left (of_int imm16) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
