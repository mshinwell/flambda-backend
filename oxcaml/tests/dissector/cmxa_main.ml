(* Main program that uses mylib.cmxa *)

let () =
  print_endline "=== CMXA archive test ===";
  print_newline ();

  (* Test direct calls to A *)
  print_endline "Test 1: Direct calls to Mylib_a";
  let c1 = Mylib_a.increment () in
  let c2 = Mylib_a.increment () in
  Printf.printf "  Counter: %d, %d\n" c1 c2;
  Printf.printf "  sum_data: %d\n" (Mylib_a.sum_data ());
  Printf.printf "  double(5): %d\n" (Mylib_a.double 5);
  Printf.printf "  triple(5): %d\n" (Mylib_a.triple 5);
  Printf.printf "  square(5): %d\n" (Mylib_a.square 5);
  print_newline ();

  (* Test calls through B *)
  print_endline "Test 2: Calls through Mylib_b";
  let c3 = Mylib_b.call_increment () in
  Printf.printf "  call_increment: %d\n" c3;
  Printf.printf "  call_double(7): %d\n" (Mylib_b.call_double 7);
  Printf.printf "  combined(10): %d\n" (Mylib_b.combined 10);
  Printf.printf "  get_data_element(2): %d\n" (Mylib_b.get_data_element 2);
  print_newline ();

  print_endline "=== All tests passed! ==="
