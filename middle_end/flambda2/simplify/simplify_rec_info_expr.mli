(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification functions on [Rec_info_expr.t]. *)

val simplify_rec_info_expr :
  Downwards_acc.t -> Rec_info_expr.t -> Rec_info_expr.t

module Evaluated_rec_info_expr : sig
  type t = private
    { depth : int Or_infinity.t;
      unrolling : Rec_info_expr.Unrolling_state.t
    }

  val print : Format.formatter -> t -> unit
end

val evaluate_rec_info_expr :
  Downwards_acc.t -> Rec_info_expr.t -> Evaluated_rec_info_expr.t

val depth_may_exceed : Downwards_acc.t -> Rec_info_expr.t -> int -> bool

val known_remaining_unrolling_depth :
  Downwards_acc.t -> Rec_info_expr.t -> int option

val can_unroll : Downwards_acc.t -> Rec_info_expr.t -> bool
