(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Simplify_import

val simplify_let :
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Let.t Simplify_common.expr_simplifier

val simplify_let_with_bound_pattern :
  simplify_expr_with_bound_pattern:
    (Bound_pattern.t * Expr.t) Simplify_common.expr_simplifier ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Let.t Simplify_common.expr_simplifier
