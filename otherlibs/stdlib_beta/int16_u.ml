(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

type t = int16#

[@@@ocaml.flambda_o3]

(** Tag a [int16#] *)
external to_int16 : int16# -> int16 = "%tag_int16" [@@warning "-187"]

(** Untag a tagged [int16] *)
external of_int16 : int16 -> int16# = "%untag_int16" [@@warning "-187"]

let size = Int16.size

external of_int : int -> int16# = "%int16#_of_int"
external to_int : int16# -> int = "%int_of_int16#"

external sub : int16# -> int16# -> int16# = "%sub_int16#"

let[@inline always] to_int t = Int16.to_int (to_int16 t)

let[@inline always] of_int i = of_int16 (Int16.of_int i)

let[@inline always] zero () = of_int16 Int16.zero

let[@inline always] one () = of_int16 Int16.one

let[@inline always] minus_one () = of_int16 Int16.minus_one

let[@inline always] max_int () = of_int16 Int16.max_int

let[@inline always] min_int () = of_int16 Int16.min_int

external add : int16# -> int16# -> int16# = "%add_int16#"

external sub : int16# -> int16# -> int16# = "%sub_int16#"

external mul : int16# -> int16# -> int16# = "%mul_int16#"

external div : int16# -> int16# -> int16# = "%sdiv_int16#"

external rem : int16# -> int16# -> int16# = "%srem_int16#"

external ( >= ) : int16# -> int16# -> bool = "%sge_int16#"

let[@inline always] neg x = sub (zero()) x

let[@inline always] succ x = add x (one ())

let[@inline always] pred x = sub x (one ())

let[@inline always] abs x = if x >= zero() then x else neg x

external logand : int16# -> int16# -> int16# = "%and_int16#"

external logor : int16# -> int16# -> int16# = "%or_int16#"

external logxor : int16# -> int16# -> int16# = "%xor_int16#"

let[@inline always] lognot x = logxor x (minus_one ())

external shift_left_unboxed : int16# -> int16# -> int16# = "%shl_int16#"
external shift_right_unboxed : int16# -> int16# -> int16# = "%ashr_int16#"
external shift_right_logical_unboxed : int16# -> int16# -> int16# = "%lshr_int16#"

let[@inline always] shift_left x y = shift_left_unboxed x (of_int16 y)

let[@inline always] shift_right x y = of_int16 (Int16.shift_right (to_int16 x) y)

let[@inline always] shift_right_logical x y = of_int16 (Int16.shift_right_logical (to_int16 x) y)

let[@inline always] equal x y = Int16.equal (to_int16 x) (to_int16 y)

let[@inline always] compare x y = Int16.compare (to_int16 x) (to_int16 y)

let[@inline always] min x y = of_int16 (Int16.min (to_int16 x) (to_int16 y))

let[@inline always] max x y = of_int16 (Int16.max (to_int16 x) (to_int16 y))

let[@inline always] of_float f = of_int16 (Int16.of_float f)

let[@inline always] to_float t = Int16.to_float (to_int16 t)

let[@inline always] to_string t = Int16.to_string (to_int16 t)
