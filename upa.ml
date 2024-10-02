[@@@ocaml.flambda_o3]

(* Length *)

external[@layout_poly] len : ('a : any) . 'a array -> int = "%array_length"

let length_scannable (x : #(int * float * string) array) = len x

let length_ignorable (x : #(float# * int * int64# * bool) array) = len x


(* Normal get and set *)

external[@layout_poly] get : ('a : any) . 'a array -> int -> 'a =
  "%array_unsafe_get"

let unsafe_get_scannable (x : #(int * float * string) array) = get x 42

let unsafe_get_ignorable (x : #(float# * int * int64# * bool) array) = get x 42


external[@layout_poly] get : ('a : any) . 'a array -> int -> 'a =
  "%array_safe_get"

let safe_get_scannable (x : #(int * float * string) array) = get x 42

let safe_get_ignorable (x : #(float# * int * int64# * bool) array) = get x 42


external[@layout_poly] set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"

let unsafe_set_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let unsafe_set_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)


external[@layout_poly] set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_safe_set"

let safe_set_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let safe_set_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)


(* Reinterpret load/store ops (non-scannable) *)

external get : int64# array -> int -> #(float# * int * int64# * bool) =
  "%unboxed_int64_array_unsafe_get_reinterpret"

let reinterpret_unsafe_get x : #(float# * int * int64# * bool) = get x 42


external[@layout_poly] get : ('a : any) . int64# array -> int -> 'a =
  "%unboxed_int64_array_safe_get_reinterpret"

let reinterpret_safe_get x : #(float# * int * int64# * bool) = get x 42


external[@layout_poly] set : ('a : any) . int64# array -> int -> 'a -> unit =
  "%unboxed_int64_array_unsafe_set_reinterpret"

let reinterpret_unsafe_set x =
  set x 42 #(#1.0, 2, #3L, true)

external[@layout_poly] set : ('a : any) . int64# array -> int -> 'a -> unit =
  "%unboxed_int64_array_safe_set_reinterpret"

let reinterpret_safe_set x =
  set x 42 #(#1.0, 2, #3L, true)


(* Creation via the magic primitive *)

external[@layout_poly] make_unboxed_tuple_vect : ('a : any) .
  int -> 'a -> 'a array = "%make_unboxed_tuple_vect"

(* ... with the temporary used to communicate to the C runtime stub
   being constant, and statically allocated *)

let make_unboxed_tuple_vect_scannable () =
  make_unboxed_tuple_vect 42 #(1, 2.0, "3")

let make_unboxed_tuple_vect_ignorable () =
  make_unboxed_tuple_vect 42 #(#1.0, 2, #3L, true)

let _ = make_unboxed_tuple_vect_scannable ()
let _ = make_unboxed_tuple_vect_ignorable ()

(* ... with the temporary used to communicate to the C runtime stub
   being inconstant, and statically allocated *)

let[@inline] make_unboxed_tuple_vect_scannable_inconstant () =
  make_unboxed_tuple_vect 42 #(Sys.opaque_identity 1, 2.0, "3")

let[@inline] make_unboxed_tuple_vect_ignorable_inconstant () =
  make_unboxed_tuple_vect 42 #(#1.0, (Random.int [@inlined never]) 42, #3L, true)

let _ = make_unboxed_tuple_vect_scannable_inconstant ()
let _ = make_unboxed_tuple_vect_ignorable_inconstant ()

(* ... with the temporary used to communicate to the C runtime stub
   being dynamically allocated on the local stack. *)

let[@inline never] make_unboxed_tuple_vect_scannable_dynamic () =
  make_unboxed_tuple_vect 42 #(Sys.opaque_identity 1, 2.0, "3")

let[@inline never] make_unboxed_tuple_vect_ignorable_dynamic () =
  make_unboxed_tuple_vect 42 #(#1.0, (Random.int [@inlined never]) 42, #3L, true)

let _ = make_unboxed_tuple_vect_scannable_dynamic ()
let _ = make_unboxed_tuple_vect_ignorable_dynamic ()


(* Simplification via iarrays *)

external int64_u_of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
[@@warning "-187"]
external int64_u_to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
[@@warning "-187"]
external float_u_to_float: float# -> (float[@local_opt]) = "%box_float"
[@@warning "-187"]
external[@layout_poly] reinterpret_get
  : ('a : any) . int64# iarray -> int -> 'a =
  "%unboxed_int64_array_safe_get_reinterpret"
external[@layout_poly] reinterpret_set
  : ('a : any) . int64# iarray -> int -> 'a -> unit =
  "%unboxed_int64_array_safe_set_reinterpret"
external[@layout_poly] iarray_length : ('a : any) .
  local_ 'a iarray -> int = "%array_length"

let () =
  let i1 = Int64.bits_of_float 1.0 |> int64_u_of_int64 in
  let i2 = 2L |> int64_u_of_int64 in
  let i3 = Int64.bits_of_float 3.0 |> int64_u_of_int64 in
  let arr : int64# iarray = [:
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
  :]
  in
  let rec loop i =
    if i >= iarray_length arr / 3 - 1 then ()
    else (
      let #(i1, i2, i3) : #(float# * int64# * float#) = reinterpret_get arr (i * 3) in
      Printf.printf "%f %Ld %f\n"
        (float_u_to_float i1) (int64_u_to_int64 i2) (float_u_to_float i3);
      (*
      let #(i1, i2, i3) : #(int64# * int64# * int64#) = reinterpret_get arr (i * 3) in
      Printf.printf "%Ld %Ld %Ld\n"
        (int64_u_to_int64 i1)
        (int64_u_to_int64 i2)
        (int64_u_to_int64 i3); *)
      loop (i + 1)
    )
  in
  loop 0

(* All array ops here should be evaluated fully at compile time *)
let[@inline never] compute () =
  let i1 = Int64.bits_of_float 1.0 |> int64_u_of_int64 in
  let i2 = 2L |> int64_u_of_int64 in
  let i3 = Int64.bits_of_float 3.0 |> int64_u_of_int64 in
  let arr : int64# iarray = [:
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
    i1; i2; i3;
  :]
  in
  let[@loop never] rec loop i total =
    if i >= iarray_length arr / 3 - 1 then total
    else (
      let #(i1, i2, i3) : #(float# * int64# * float#) = reinterpret_get arr (i * 3) in
      let f1 = float_u_to_float i1 in
      let i2 = int64_u_to_int64 i2 in
      let f3 = float_u_to_float i3 in
      let total = Float.to_int f1 + Int64.to_int i2 + Float.to_int f3 + total in
      loop (i + 1) total
    )
  in
  (loop [@unrolled 10]) 0 0

let () =
  Printf.printf "total %d\n" (compute ())

