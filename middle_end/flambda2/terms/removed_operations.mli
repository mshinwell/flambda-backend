(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  { call : int;
    alloc : int;
    prim : int;
    branch : int;
    direct_call_of_indirect : int;
    specialized_poly_compare : int;
    requested_inline : int
  }

val zero : t

val call : t

val branch : t

val prim : Flambda_primitive.t -> t

val alloc : t

val direct_call_of_indirect : t

val specialized_poly_compare : t

val ( + ) : t -> t -> t

val print : Format.formatter -> t -> unit

val evaluate : args:Inlining_arguments.t -> t -> float

val equal : t -> t -> bool
