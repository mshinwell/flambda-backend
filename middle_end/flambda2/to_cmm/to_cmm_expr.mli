(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Translation of Flambda expressions to Cmm. *)

val expr :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Flambda.Expr.t ->
  Cmm.expression * To_cmm_env.free_vars * To_cmm_result.t
