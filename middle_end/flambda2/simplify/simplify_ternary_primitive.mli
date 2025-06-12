(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of primitives taking three arguments. *)

val simplify_ternary_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.ternary_primitive ->
  arg1:Simple.t ->
  arg1_ty:Flambda2_types.t ->
  arg2:Simple.t ->
  arg2_ty:Flambda2_types.t ->
  arg3:Simple.t ->
  arg3_ty:Flambda2_types.t ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
