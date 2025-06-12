(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Flambda

val simplify_let_cont :
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  Let_cont.t Simplify_common.expr_simplifier

val simplify_as_recursive_let_cont :
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  (Expr.t * Continuation_handler.t Continuation.Lmap.t)
  Simplify_common.expr_simplifier
