(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = private
  { simplified_named : Simplified_named.t Or_invalid.t;
    extra_bindings : Expr_builder.binding_to_place list;
    try_reify : bool;
    dacc : Downwards_acc.t
  }

val create :
  ?extra_bindings:Expr_builder.binding_to_place list ->
  Flambda.Named.t ->
  try_reify:bool ->
  Downwards_acc.t ->
  t

val create_simplified :
  Simplified_named.t -> try_reify:bool -> Downwards_acc.t -> t

val create_invalid : Downwards_acc.t -> t

val create_unit :
  Downwards_acc.t ->
  result_var:Bound_var.t ->
  original_term:Flambda.Named.t ->
  t

(** [original_term] is in the majority of cases the pre-simplification term
    corresponding to the primitive, but it is fine for it to be a new term,
    whose type is unknown. *)
val create_unknown :
  Downwards_acc.t ->
  result_var:Bound_var.t ->
  Flambda_kind.t ->
  original_term:Flambda.Named.t ->
  t

val with_dacc : t -> Downwards_acc.t -> t
