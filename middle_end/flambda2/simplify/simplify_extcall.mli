(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of external calls *)

type t =
  | Unchanged of { return_types : Flambda2_types.t list Or_unknown.t }
  | Specialised of Downwards_acc.t * Flambda.Expr.t * Removed_operations.t
  | Invalid

val simplify_extcall :
  Downwards_acc.t ->
  Flambda.Apply.t ->
  callee_ty:Flambda2_types.t ->
  arg_types:Flambda2_types.t list ->
  t
