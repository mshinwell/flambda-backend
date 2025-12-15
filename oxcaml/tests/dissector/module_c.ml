(* Module C: Depends on both Module A and Module B *)
(* Tests cross-partition calls in multiple directions *)

(* Call functions from both modules *)
let triple_increment () =
  let b_result = Module_b.double_increment () in
  let a_result = Module_a.increment () in
  a_result, b_result

(* Chain of calls: C -> B -> A *)
let chained_magic () = Module_b.get_magic_doubled () + Module_a.get_magic ()

(* Use types from A via B *)
let compute_triangle_perimeter x1 y1 x2 y2 x3 y3 =
  let p1 = Module_b.point_at x1 y1 in
  let p2 = Module_b.point_at x2 y2 in
  let p3 = Module_b.point_at x3 y3 in
  Module_a.compute_distance p1 p2
  +. Module_a.compute_distance p2 p3
  +. Module_a.compute_distance p3 p1

(* Complex computation using both modules *)
let combined_computation n =
  let fact_sum = Module_b.factorial_sum n in
  let arr_sum = Module_b.double_array_sum () in
  let magic = Module_a.get_magic () in
  fact_sum + arr_sum + magic

(* String operations across modules *)
let make_full_greeting () =
  let greeting = Module_b.greet_world () in
  greeting ^ " (via Module C)"

(* Test mutable state across modules *)
let increment_all () =
  let b_local, a_from_b = Module_b.increment_both () in
  let a_direct = Module_a.increment () in
  a_direct, a_from_b, b_local

(* Own data *)
let c_data = [| 100; 200; 300 |]

let sum_all_arrays () =
  Module_a.sum_array Module_a.sample_array + Module_a.sum_array c_data

(* Recursive with cross-module calls *)
let rec nested_compute depth =
  if depth <= 0
  then Module_a.get_magic ()
  else
    let chain_result = Module_b.compute_chain depth in
    chain_result + nested_compute (depth - 1)
