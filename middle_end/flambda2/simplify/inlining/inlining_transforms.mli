(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

open! Flambda.Import

val inline :
  Downwards_acc.t ->
  apply:Apply.t ->
  unroll_to:int option ->
  was_inline_always:bool ->
  Flambda2_types.Function_type.t ->
  Downwards_acc.t * Expr.t
