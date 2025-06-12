(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Flambda.Import

type t

val create : Downwards_acc.t -> Expr_builder.binding_to_place list -> t

val create_have_lifted_set_of_closures :
  Downwards_acc.t ->
  (Bound_var.t * Symbol.t) list ->
  original_defining_expr:Named.t ->
  t

val dacc : t -> Downwards_acc.t

val bindings_to_place : t -> Expr_builder.binding_to_place list

val no_bindings : t -> bool

val was_lifted_set_of_closures : t -> bool

val with_dacc : dacc:Downwards_acc.t -> t -> t
