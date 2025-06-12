(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Translation of statically-allocated constants to Cmm. *)

open! Flambda.Import

val static_consts :
  To_cmm_env.t ->
  To_cmm_result.t ->
  params_and_body:
    (To_cmm_env.t ->
    To_cmm_result.t ->
    Code_id.t ->
    Function_params_and_body.t ->
    result_arity:[`Unarized] Flambda_arity.t ->
    fun_dbg:Debuginfo.t ->
    zero_alloc_attribute:Zero_alloc_attribute.t ->
    Cmm.fundecl * To_cmm_result.t) ->
  Bound_static.t ->
  Static_const_group.t ->
  To_cmm_env.t * To_cmm_result.t * To_cmm_env.expr_with_info option
