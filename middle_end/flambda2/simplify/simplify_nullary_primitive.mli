(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of primitives taking no argument. *)

val simplify_nullary_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.nullary_primitive ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
