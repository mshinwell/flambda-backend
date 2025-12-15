(* Main file for huge module cross-partition test *)

let () =
  print_endline "=== Huge module cross-partition test ===";
  print_newline ();
  (* Test 1: Direct calls to A *)
  print_endline "Test 1: Direct calls to Huge_module_a";
  let c1 = Huge_module_a.increment () in
  let c2 = Huge_module_a.increment () in
  Printf.printf "  Counter: %d, %d\n" c1 c2;
  Printf.printf "  Magic: %d\n" (Huge_module_a.get_magic ());
  Printf.printf "  sum_arr1: %d\n" (Huge_module_a.sum_arr1 ());
  Printf.printf "  call_all(1): %d\n" (Huge_module_a.call_all 1);
  Printf.printf "  factorial(5): %d\n" (Huge_module_a.factorial 5);
  print_newline ();
  (* Test 2: Cross-partition calls via B *)
  print_endline "Test 2: Cross-partition calls (B -> A)";
  let c3 = Huge_module_b.call_a_increment () in
  Printf.printf "  call_a_increment: %d\n" c3;
  Printf.printf "  call_a_magic: %d\n" (Huge_module_b.call_a_magic ());
  Printf.printf "  call_a_sum: %d\n" (Huge_module_b.call_a_sum ());
  Printf.printf "  call_a_functions(1): %d\n" (Huge_module_b.call_a_functions 1);
  Printf.printf "  call_a_factorial(6): %d\n" (Huge_module_b.call_a_factorial 6);
  print_newline ();
  (* Test 3: B's own data *)
  print_endline "Test 3: Module B's own data";
  Printf.printf "  B.sum_arr1: %d\n" (Huge_module_b.sum_arr1 ());
  Printf.printf "  B.call_all(1): %d\n" (Huge_module_b.call_all 1);
  print_newline ();
  (* Test 4: Cross-partition data access *)
  print_endline "Test 4: Cross-partition data access (B accessing A's arrays)";
  Printf.printf "  A.arr1[0]: %d\n" (Huge_module_b.get_a_arr1_element 0);
  Printf.printf "  A.arr1[500]: %d\n" (Huge_module_b.get_a_arr1_element 500);
  Printf.printf "  A.arr2[0]: %d\n" (Huge_module_b.get_a_arr2_element 0);
  Printf.printf "  A.arr2[500]: %d\n" (Huge_module_b.get_a_arr2_element 500);
  print_newline ();
  (* Test 5: Cross-partition state *)
  print_endline "Test 5: Cross-partition state";
  let b_local, a_counter = Huge_module_b.increment_both () in
  Printf.printf "  increment_both: b_local=%d, a_counter=%d\n" b_local a_counter;
  print_newline ();
  (* Test 6: Module C cross-partition calls *)
  print_endline "Test 6: Cross-partition calls (C -> A, C -> B)";
  Printf.printf "  C.call_a_increment: %d\n" (Huge_module_c.call_a_increment ());
  Printf.printf "  C.call_b_increment: %d\n" (Huge_module_c.call_b_increment ());
  let a_val, b_val = Huge_module_c.call_both () in
  Printf.printf "  C.call_both: a=%d, b=%d\n" a_val b_val;
  print_newline ();
  (* Test 7: Module C's own data *)
  print_endline "Test 7: Module C's own data";
  Printf.printf "  C.sum_arr1: %d\n" (Huge_module_c.sum_arr1 ());
  Printf.printf "  C.call_all(1): %d\n" (Huge_module_c.call_all 1);
  print_newline ();
  (* Test 8: Combined sums from all modules *)
  print_endline "Test 8: Combined data from all modules";
  Printf.printf "  sum_all: %d\n" (Huge_module_c.sum_all ());
  print_newline ();
  print_endline "=== All tests passed! ==="
