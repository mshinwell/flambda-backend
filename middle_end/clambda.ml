(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

type arity = {
  function_kind : Lambda.function_kind ;
  params_layout : Lambda.layout list ;
  return_layout : Lambda.layout ;
}

type ustructured_constant =
  | Uconst_float of float
  | Uconst_int32 of int32
  | Uconst_int64 of int64
  | Uconst_nativeint of nativeint
  | Uconst_vec128 of { high : int64; low : int64 }
  | Uconst_block of int * uconstant list
  | Uconst_float_array of float list
  | Uconst_string of string

and uconstant =
  | Uconst_ref of string * ustructured_constant option
  | Uconst_int of int

(* Comparison functions for constants.  We must not use Stdlib.compare
   because it compares "0.0" and "-0.0" equal.  (PR#6442) *)

let compare_floats x1 x2 =
  Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)

let rec compare_float_lists l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | h1::t1, h2::t2 ->
      let c = compare_floats h1 h2 in
      if c <> 0 then c else compare_float_lists t1 t2

let compare_constants c1 c2 =
  match c1, c2 with
  | Uconst_ref(lbl1, _c1), Uconst_ref(lbl2, _c2) -> String.compare lbl1 lbl2
      (* Same labels -> same constants.
         Different labels -> different constants, even if the contents
           match, because of string constants that must not be
           reshared. *)
  | Uconst_int n1, Uconst_int n2 -> Stdlib.compare n1 n2
  | Uconst_ref _, _ -> -1
  | Uconst_int _, Uconst_ref _ -> 1

let rec compare_constant_lists l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | h1::t1, h2::t2 ->
      let c = compare_constants h1 h2 in
      if c <> 0 then c else compare_constant_lists t1 t2

let rank_structured_constant = function
  | Uconst_float _ -> 0
  | Uconst_int32 _ -> 1
  | Uconst_int64 _ -> 2
  | Uconst_nativeint _ -> 3
  | Uconst_block _ -> 4
  | Uconst_float_array _ -> 5
  | Uconst_string _ -> 6
  | Uconst_vec128 _ -> 8

let compare_structured_constants c1 c2 =
  match c1, c2 with
  | Uconst_float x1, Uconst_float x2 -> compare_floats x1 x2
  | Uconst_int32 x1, Uconst_int32 x2 -> Int32.compare x1 x2
  | Uconst_int64 x1, Uconst_int64 x2 -> Int64.compare x1 x2
  | Uconst_nativeint x1, Uconst_nativeint x2 -> Nativeint.compare x1 x2
  | Uconst_block(t1, l1), Uconst_block(t2, l2) ->
      let c = t1 - t2 (* no overflow possible here *) in
      if c <> 0 then c else compare_constant_lists l1 l2
  | Uconst_float_array l1, Uconst_float_array l2 ->
      compare_float_lists l1 l2
  | Uconst_string s1, Uconst_string s2 -> String.compare s1 s2
  | Uconst_vec128 { high = l0; low = l1},
    Uconst_vec128 { high = r0; low = r1} ->
    let cmp = Int64.compare l0 r0 in
    if cmp = 0 then Int64.compare l1 r1 else cmp
  | _, _ ->
    (* no overflow possible here *)
    rank_structured_constant c1 - rank_structured_constant c2
