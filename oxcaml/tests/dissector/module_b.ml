(* Module B: Depends on Module A, tests cross-partition function calls *)

(* Call functions from Module A *)
let double_increment () =
  let _ = Module_a.increment () in
  Module_a.increment ()

(* Access data from Module A *)
let get_magic_doubled () =
  Module_a.get_magic () * 2

(* Use Module A's types and functions *)
let point_at x y : Module_a.point = { x; y }

let distance_from_origin p =
  Module_a.compute_distance p Module_a.origin

(* More complex cross-module interaction *)
let factorial_sum n =
  let rec loop acc i =
    if i > n then acc
    else loop (acc + Module_a.factorial i) (i + 1)
  in
  loop 0 1

(* Access Module A's array *)
let double_array_sum () =
  2 * Module_a.sum_array Module_a.sample_array

(* String operations using Module A *)
let greet_world () =
  Module_a.make_greeting "World"

(* Own state that interacts with Module A *)
let combined_counter = ref 0

let increment_both () =
  incr combined_counter;
  let a_count = Module_a.increment () in
  (!combined_counter, a_count)

(* Recursive function that calls into Module A *)
let rec compute_chain n =
  if n <= 0 then Module_a.get_magic ()
  else
    let _ = Module_a.increment () in
    compute_chain (n - 1) + 1
