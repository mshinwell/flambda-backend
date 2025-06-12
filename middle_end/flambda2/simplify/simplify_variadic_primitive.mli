(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of primitives taking variable numbers of arguments. *)

val simplify_variadic_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.variadic_primitive ->
  args_with_tys:(Simple.t * Flambda2_types.t) list ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
