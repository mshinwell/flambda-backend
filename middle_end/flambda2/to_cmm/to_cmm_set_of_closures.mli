(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Translation of sets of closures to Cmm. *)

open! Flambda.Import

type translate_expr =
  To_cmm_env.t -> To_cmm_result.t -> Expr.t -> Cmm.expression * To_cmm_result.t

val let_static_set_of_closures :
  To_cmm_env.t ->
  Symbol.t Function_slot.Map.t ->
  Set_of_closures.t ->
  Slot_offsets.layout ->
  prev_updates:Cmm.expression option ->
  To_cmm_env.t * Cmm.data_item list * Cmm.expression option

val let_dynamic_set_of_closures :
  To_cmm_env.t ->
  To_cmm_result.t ->
  body:Expr.t ->
  bound_vars:Bound_var.t list ->
  num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
  Set_of_closures.t ->
  translate_expr:translate_expr ->
  let_expr_bind:
    (?extra:To_cmm_env.extra_info ->
    To_cmm_env.t ->
    Variable.t ->
    num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
    Cmm.expression ->
    effects_and_coeffects_of_defining_expr:Effects_and_coeffects.t ->
    To_cmm_env.t) ->
  Cmm.expression * To_cmm_result.t

val params_and_body :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Code_id.t ->
  Function_params_and_body.t ->
  fun_dbg:Debuginfo.t ->
  translate_expr:
    (To_cmm_env.t ->
    To_cmm_result.t ->
    Expr.t ->
    Cmm.expression * To_cmm_result.t) ->
  Cmm.fundecl * To_cmm_result.t
