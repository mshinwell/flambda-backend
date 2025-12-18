(* Generator for large OCaml modules to test cross-partition calls.

   Usage: ocaml generate_large_modules.ml

   This generates modules that are each larger than stdlib.a (~1.5MB)
   to ensure they end up in different partitions when using the dissector.
*)

let num_functions = 10000  (* Number of functions per module - keep under stack limit *)
let array_size = 130000   (* Size of array literals - larger to exceed stdlib size *)

let output_file name content =
  let oc = open_out name in
  output_string oc content;
  close_out oc;
  Printf.printf "Generated %s\n" name

let generate_array_literal prefix size =
  let buf = Buffer.create (size * 10) in
  Buffer.add_string buf "let arr = [|\n";
  for i = 0 to size - 1 do
    if i > 0 then Buffer.add_string buf ";";
    if i mod 20 = 0 then Buffer.add_string buf "\n  ";
    Buffer.add_string buf (string_of_int (i + prefix))
  done;
  Buffer.add_string buf "\n|]\n\n";
  Buffer.contents buf

let generate_functions prefix num =
  let buf = Buffer.create (num * 100) in
  for i = 0 to num - 1 do
    Printf.bprintf buf "let f%05d x = x + %d\n" i (prefix + i)
  done;
  Buffer.add_char buf '\n';
  (* Generate call_all that calls all functions *)
  Buffer.add_string buf "let call_all x =\n";
  for i = 0 to num - 1 do
    if i = 0 then
      Printf.bprintf buf "  f%05d x" i
    else if i mod 10 = 0 then
      Printf.bprintf buf " +\n  f%05d x" i
    else
      Printf.bprintf buf " + f%05d x" i
  done;
  Buffer.add_string buf "\n\n";
  Buffer.contents buf

let generate_module_a () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Large module A - generated, do not edit *)\n\n";
  Buffer.add_string buf (generate_array_literal 0 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n";
  Buffer.add_string buf "let get_magic () = 42\n\n";
  Buffer.add_string buf (generate_functions 1 num_functions);
  Buffer.contents buf

let generate_module_b () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Large module B - generated, do not edit *)\n";
  Buffer.add_string buf "(* Depends on Large_module_a *)\n\n";
  Buffer.add_string buf (generate_array_literal 10000 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n\n";
  Buffer.add_string buf "(* Cross-partition calls to A *)\n";
  Buffer.add_string buf "let call_a_increment () = Large_module_a.increment ()\n";
  Buffer.add_string buf "let call_a_magic () = Large_module_a.get_magic ()\n";
  Buffer.add_string buf "let call_a_functions x = Large_module_a.call_all x\n";
  Buffer.add_string buf "let get_a_arr_element i = Large_module_a.arr.(i)\n\n";
  Buffer.add_string buf (generate_functions 1001 num_functions);
  Buffer.contents buf

let generate_module_c () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Large module C - generated, do not edit *)\n";
  Buffer.add_string buf "(* Depends on Large_module_a and Large_module_b *)\n\n";
  Buffer.add_string buf (generate_array_literal 20000 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n\n";
  Buffer.add_string buf "(* Cross-partition calls to A and B *)\n";
  Buffer.add_string buf "let call_a_increment () = Large_module_a.increment ()\n";
  Buffer.add_string buf "let call_b_increment () = Large_module_b.increment ()\n";
  Buffer.add_string buf "let call_a_magic () = Large_module_a.get_magic ()\n";
  Buffer.add_string buf "let call_both_functions x = Large_module_a.call_all x + Large_module_b.call_all x\n";
  Buffer.add_string buf "let get_a_arr_element i = Large_module_a.arr.(i)\n";
  Buffer.add_string buf "let get_b_arr_element i = Large_module_b.arr.(i)\n\n";
  Buffer.add_string buf (generate_functions 2001 num_functions);
  Buffer.contents buf

let generate_main () =
  {|(* Main module for large cross-partition test - generated, do not edit *)

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
|}

let () =
  output_file "large_module_a.ml" (generate_module_a ());
  output_file "large_module_b.ml" (generate_module_b ());
  output_file "large_module_c.ml" (generate_module_c ());
  output_file "large_cross_main.ml" (generate_main ());
  print_endline "Done!"
