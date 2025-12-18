(* Main module for large cross-partition test - generated, do not edit *)

let () =
  print_endline "=== Large cross-partition test (4 partitions) ===";
  print_newline ();

  (* Test 1: Direct calls to module A *)
  print_endline "Test 1: Direct calls to Large_module_a";
  let a1 = Large_module_a.increment () in
  let a2 = Large_module_a.increment () in
  Printf.printf "  Counter: %d, %d\n" a1 a2;
  let magic = Large_module_a.get_magic () in
  Printf.printf "  Magic: %d\n" magic;
  let elem = Large_module_a.arr.(5) in
  Printf.printf "  arr[5]: %d\n" elem;
  let f_result = Large_module_a.f00010 100 in
  Printf.printf "  f00010(100): %d\n" f_result;
  print_newline ();

  (* Test 2: Calls through module B to A *)
  print_endline "Test 2: Calls through Large_module_b";
  let b1 = Large_module_b.increment () in
  Printf.printf "  B.increment: %d\n" b1;
  let a_from_b = Large_module_b.call_a_increment () in
  Printf.printf "  B.call_a_increment: %d\n" a_from_b;
  let a_magic = Large_module_b.call_a_magic () in
  Printf.printf "  B.call_a_magic: %d\n" a_magic;
  let a_elem = Large_module_b.get_a_arr_element 10 in
  Printf.printf "  B.get_a_arr_element(10): %d\n" a_elem;
  let b_elem = Large_module_b.arr.(15) in
  Printf.printf "  B.arr[15]: %d\n" b_elem;
  print_newline ();

  (* Test 3: Calls through module C to A and B *)
  print_endline "Test 3: Calls through Large_module_c";
  let c1 = Large_module_c.increment () in
  Printf.printf "  C.increment: %d\n" c1;
  let a_from_c = Large_module_c.call_a_increment () in
  Printf.printf "  C.call_a_increment: %d\n" a_from_c;
  let b_from_c = Large_module_c.call_b_increment () in
  Printf.printf "  C.call_b_increment: %d\n" b_from_c;
  let c_magic = Large_module_c.call_a_magic () in
  Printf.printf "  C.call_a_magic: %d\n" c_magic;
  let a_elem_c = Large_module_c.get_a_arr_element 20 in
  Printf.printf "  C.get_a_arr_element(20): %d\n" a_elem_c;
  let b_elem_c = Large_module_c.get_b_arr_element 25 in
  Printf.printf "  C.get_b_arr_element(25): %d\n" b_elem_c;
  let c_elem = Large_module_c.arr.(30) in
  Printf.printf "  C.arr[30]: %d\n" c_elem;
  print_newline ();

  (* Test 4: call_all functions *)
  print_endline "Test 4: call_all functions";
  let a_all = Large_module_a.call_all 1 in
  Printf.printf "  A.call_all(1): %d\n" a_all;
  let b_all = Large_module_b.call_all 1 in
  Printf.printf "  B.call_all(1): %d\n" b_all;
  let c_all = Large_module_c.call_all 1 in
  Printf.printf "  C.call_all(1): %d\n" c_all;
  let cross_b = Large_module_b.call_a_functions 1 in
  Printf.printf "  B.call_a_functions(1): %d\n" cross_b;
  let cross_c = Large_module_c.call_both_functions 1 in
  Printf.printf "  C.call_both_functions(1): %d\n" cross_c;
  print_newline ();

  print_endline "=== All tests passed! ==="
