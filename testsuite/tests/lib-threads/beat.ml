(* TEST
 {
   not-macos;
   include systhreads;
   hassysthreads;
   { bytecode; }
   { native; }
 }{
<<<<<<< HEAD
   macos;
   reason = "off-by-one error on MacOS+Clang (#408)";
||||||| 23e84b8c4d
   reason = "off-by-one error on MacOS+Clang (#408)";
=======
   reason = "off-by-one error on MacOS+Clang (https://github.com/ocaml-multicore/ocaml-multicore/issues/408)";
>>>>>>> ocaml/5.4
   skip;
 }
*)

(* Test Thread.delay and its scheduling *)

open Printf

let tick (delay, count) =
  while true do
    Thread.delay delay;
    incr count
  done

let _ =
  let c1 = ref 0 and c2 = ref 0 in
  ignore (Thread.create tick (0.333333333, c1));
  ignore (Thread.create tick (0.5, c2));
  Thread.delay 3.0;
  let n1 = !c1 and n2 = !c2 in
  if n1 >= 8 && n1 <= 10 && n2 >= 5 && n2 <= 7
  then printf "passed\n"
  else printf "FAILED (n1 = %d, n2 = %d)\n" n1 n2
