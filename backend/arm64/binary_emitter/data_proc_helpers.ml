(* Data-processing (2 source) - C4.1.94.1 *)
let encode_data_proc_2_source ~sf ~s ~opcode ~rm ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (1 source) - C4.1.94.2 *)
let encode_data_proc_1_source ~sf ~s ~opcode2 ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b1) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int opcode2) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (3 source) - C4.1.94.13 *)
let encode_data_proc_3_source ~sf ~op54 ~op31 ~o0 ~rm ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op54) 29) in
  let result = logor result (shift_left (of_int 0b11011) 24) in
  let result = logor result (shift_left (of_int op31) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding ra)) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
