(* Main module: Tests cross-partition calls between OCaml modules *)

let () =
  print_endline "=== Cross-partition OCaml reference test ===";
  print_newline ();
  (* Test 1: Direct calls to Module A *)
  print_endline "Test 1: Direct calls to Module A";
  let count1 = Module_a.increment () in
  let count2 = Module_a.increment () in
  Printf.printf "  Counter after 2 increments: %d, %d\n" count1 count2;
  Printf.printf "  Magic number: %d\n" (Module_a.get_magic ());
  Printf.printf "  Factorial 5: %d\n" (Module_a.factorial 5);
  Printf.printf "  Array sum: %d\n" (Module_a.sum_array Module_a.sample_array);
  print_newline ();
  (* Test 2: Calls via Module B (B -> A) *)
  print_endline "Test 2: Calls via Module B (B -> A)";
  let count3 = Module_b.double_increment () in
  Printf.printf "  Counter after double_increment: %d\n" count3;
  Printf.printf "  Magic doubled: %d\n" (Module_b.get_magic_doubled ());
  Printf.printf "  Factorial sum 1-5: %d\n" (Module_b.factorial_sum 5);
  Printf.printf "  Double array sum: %d\n" (Module_b.double_array_sum ());
  Printf.printf "  Greeting: %s\n" (Module_b.greet_world ());
  print_newline ();
  (* Test 3: Calls via Module C (C -> B -> A and C -> A) *)
  print_endline "Test 3: Calls via Module C (C -> B -> A and C -> A)";
  let a, b = Module_c.triple_increment () in
  Printf.printf "  Triple increment results: a=%d, b=%d\n" a b;
  Printf.printf "  Chained magic: %d\n" (Module_c.chained_magic ());
  Printf.printf "  Combined computation(3): %d\n"
    (Module_c.combined_computation 3);
  Printf.printf "  Full greeting: %s\n" (Module_c.make_full_greeting ());
  Printf.printf "  Sum all arrays: %d\n" (Module_c.sum_all_arrays ());
  print_newline ();
  (* Test 4: Distance calculations (float operations across partitions) *)
  print_endline "Test 4: Float operations across partitions";
  let p1 = Module_b.point_at 3.0 4.0 in
  let dist = Module_b.distance_from_origin p1 in
  Printf.printf "  Distance from origin to (3,4): %.1f\n" dist;
  let perimeter = Module_c.compute_triangle_perimeter 0.0 0.0 3.0 0.0 0.0 4.0 in
  Printf.printf "  Triangle perimeter: %.1f\n" perimeter;
  print_newline ();
  (* Test 5: Mutable state across partitions *)
  print_endline "Test 5: Mutable state across partitions";
  let a_direct, a_from_b, b_local = Module_c.increment_all () in
  Printf.printf "  After increment_all: a_direct=%d, a_from_b=%d, b_local=%d\n"
    a_direct a_from_b b_local;
  print_newline ();
  (* Test 6: Recursive cross-partition calls *)
  print_endline "Test 6: Recursive cross-partition calls";
  let chain_result = Module_b.compute_chain 3 in
  Printf.printf "  Compute chain(3): %d\n" chain_result;
  let nested_result = Module_c.nested_compute 2 in
  Printf.printf "  Nested compute(2): %d\n" nested_result;
  print_newline ();
  (* Final state check *)
  print_endline "=== Final state ===";
  Printf.printf "Final counter value: %d\n"
    (Module_a.get_counter_and_increment ());
  print_endline "All tests passed!"
