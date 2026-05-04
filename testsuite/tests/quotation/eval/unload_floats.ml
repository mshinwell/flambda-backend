(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Closures capturing floats. Floats are stored as double-words (not as
   tagged ints), so the compiler may either box them on the heap (a
   Double_tag block reachable as a scannable env field) or unbox them
   into the closure's non-scannable env. Either way, the runtime must
   keep the eval'd unit alive so long as the closure is held — and the
   closure-slot scan must not misclassify the slot when non-scannable
   float words live in the prefix. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 7

let f_unboxed = ref (fun n -> float_of_int n)
let f_boxed = ref (fun n -> n)

let[@inline never] populate () =
  (* Single float capture: typically unboxed into the closure's
     non-scannable env. *)
  f_unboxed := Eval.eval <[
    let scale = 1.0 +. Random.float 9.0 in
    fun n -> float_of_int n *. scale
  ]>;
  (* Float array capture: boxed/heap. *)
  f_boxed := Eval.eval <[
    let xs = Array.init 5 (fun _ -> Random.float 100.0) in
    fun n ->
      let s = ref 0.0 in
      Array.iter (fun x -> s := !s +. x) xs;
      n + int_of_float !s
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "f_unboxed(10) = %.2f\n" (!f_unboxed 10);
  Printf.printf "f_unboxed(20) = %.2f\n" (!f_unboxed 20);
  let a = !f_boxed 100 in
  let b = !f_boxed 200 in
  Printf.printf "f_boxed: a=%d b=%d (b-a=%d)\n" a b (b - a);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: f_unboxed(10) = %.2f\n" (!f_unboxed 10);
  let c = !f_boxed 100 in
  assert (c = a);
  report "after 2x Gc.compact (held)";

  f_unboxed := (fun n -> float_of_int n);
  f_boxed := (fun n -> n);
  Gc.compact ();
  report "after release"
