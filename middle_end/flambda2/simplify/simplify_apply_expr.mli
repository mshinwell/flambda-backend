(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Flambda

val simplify_apply :
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  Apply.t Simplify_common.expr_simplifier
