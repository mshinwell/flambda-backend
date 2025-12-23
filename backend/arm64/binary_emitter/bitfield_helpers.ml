(* Bitfield encoding - C4.1.92.8 Used for SBFM, BFM, UBFM *)
let encode_bitfield ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100110) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
