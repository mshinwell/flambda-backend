(* Library module A *)

let counter = ref 0

let increment () =
  incr counter;
  !counter

let get_value () = !counter

(* Some data *)
let data = [| 10; 20; 30; 40; 50 |]

let sum_data () = Array.fold_left (+) 0 data

(* Functions *)
let double x = x * 2
let triple x = x * 3
let square x = x * x
