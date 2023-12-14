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

(* Comparison functions for constants *)

val compare_structured_constants:
        ustructured_constant -> ustructured_constant -> int
val compare_constants:
        uconstant -> uconstant -> int
