(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of primitives taking one argument. *)

val simplify_unary_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.unary_primitive ->
  arg:Simple.t ->
  arg_ty:Flambda2_types.t ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
