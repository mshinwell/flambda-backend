(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Translation of Flambda primitives to Cmm. *)

val trans_prim : To_cmm_env.t To_cmm_env.trans_prim

val prim_simple :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  To_cmm_env.simple To_cmm_env.bound_expr
  * To_cmm_env.extra_info option
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val prim_complex :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  To_cmm_env.complex To_cmm_env.bound_expr
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t
