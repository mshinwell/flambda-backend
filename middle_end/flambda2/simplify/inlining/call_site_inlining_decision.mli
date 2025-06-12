(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Flambda.Import

val make_decision :
  Downwards_acc.t ->
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  function_type:Flambda2_types.Function_type.t ->
  apply:Apply.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  Call_site_inlining_decision_type.t

val get_rec_info :
  Downwards_acc.t ->
  function_type:Flambda2_types.Function_type.t ->
  Rec_info_expr.t
