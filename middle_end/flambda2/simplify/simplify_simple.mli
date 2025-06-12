(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification functions on [Simple.t]. *)

(** This function is guaranteed to return an alias type. *)
val simplify_simple :
  Downwards_acc.t ->
  Simple.t ->
  min_name_mode:Name_mode.t ->
  Flambda2_types.t * Simple.t

val simplify_simple_if_in_scope :
  Downwards_acc.t ->
  Simple.t ->
  min_name_mode:Name_mode.t ->
  Flambda2_types.t option

type simplify_simples_result = private
  { simples : Simple.t list;
    simple_tys : Flambda2_types.t list
  }

val simplify_simples :
  Downwards_acc.t -> Simple.t list -> simplify_simples_result
