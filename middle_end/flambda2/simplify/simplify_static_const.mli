(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of statically-allocated constants bound to symbols. *)

open! Flambda

val simplify_static_consts :
  Downwards_acc.t ->
  Bound_static.t ->
  Static_const_group.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Bound_static.t * Rebuilt_static_const.Group.t * Downwards_acc.t
