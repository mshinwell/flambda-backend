(* Conditional select - C4.1.94.12

   Encoding: sf | op | S | 11010100 | Rm | cond | op2 | Rn | Rd *)
let encode_conditional_select ~sf ~op ~op2 ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int 0b0) 29) in
  (* S = 0 *)
  let result = logor result (shift_left (of_int 0b11010100) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int op2) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
