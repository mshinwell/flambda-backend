(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Set of all arguments related to inlining. This set is stored inside the term
    language and is used to access the set of parameters to use in the
    simplifier.

    This module also allows to [meet] two sets of arguments, that is given t1
    and t2 to return the set of arguments that will inline at most as much as
    either t1 and t2 would. *)

type t

val create : round:int -> t

val print : Format.formatter -> t -> unit

(* [meet A B] constructs a set of argument that inline at most as strongly as
   [A] and [B] would *)
val meet : t -> t -> t

val equal : t -> t -> bool

val max_inlining_depth : t -> int

val call_cost : t -> float

val alloc_cost : t -> float

val prim_cost : t -> float

val branch_cost : t -> float

val indirect_call_cost : t -> float

val poly_compare_cost : t -> float

val small_function_size : t -> int

val large_function_size : t -> int

val threshold : t -> float
