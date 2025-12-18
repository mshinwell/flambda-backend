(* Module A: Defines functions and data that will be called from other modules *)

(* A global mutable reference - tests cross-partition data access *)
let counter = ref 0

(* A global immutable value *)
let magic_number = 42

(* A record type and value *)
type point = { x : float; y : float }
let origin = { x = 0.0; y = 0.0 }

(* Simple function that will be called cross-partition *)
let increment () =
  incr counter;
  !counter

(* Function that returns data *)
let get_magic () = magic_number

(* Function with more complex computation to increase code size *)
let compute_distance p1 p2 =
  let dx = p1.x -. p2.x in
  let dy = p1.y -. p2.y in
  Float.sqrt (dx *. dx +. dy *. dy)

(* Recursive function *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

(* Function that uses the global state *)
let get_counter_and_increment () =
  let old = !counter in
  incr counter;
  old

(* Array operations to test more data references *)
let sample_array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]

let sum_array arr =
  Array.fold_left ( + ) 0 arr

(* String data *)
let greeting = "Hello from module A"

let make_greeting name =
  greeting ^ ", " ^ name ^ "!"
