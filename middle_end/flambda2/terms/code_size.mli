(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* Computes an approximation for the code size corresponding to flambda terms.
   The code size of a given term should be a rough estimate of the size of the
   generated machine code. *)

(** Values of type [t] may be negative *)
type t

(* Both are only there temporarly *)
val of_int : int -> t

val to_int : t -> int

val zero : t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( <= ) : t -> t -> bool

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit

val prim : Flambda_primitive.t -> t

val simple : Simple.t -> t

val static_consts : unit -> t

val apply : Apply_expr.t -> t

val apply_cont : Apply_cont_expr.t -> t

val switch : Switch_expr.t -> t

val invalid : t

val evaluate : args:Inlining_arguments.t -> t -> float

val alloc_size : t
