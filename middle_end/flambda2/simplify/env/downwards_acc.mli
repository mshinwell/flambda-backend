(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Print a downwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

(** Create a downwards accumulator. *)
val create : Downwards_env.t -> Continuation_uses_env.t -> t

(** Extract the environment component of the given downwards accumulator. *)
val denv : t -> Downwards_env.t

(** Map the environment component of the given downwards accumulator. *)
val map_denv : t -> f:(Downwards_env.t -> Downwards_env.t) -> t

(** Replace the environment component of the given downwards accumulator. *)
val with_denv : t -> Downwards_env.t -> t

(** Extract the dataflow analysis accumulator *)
val data_flow : t -> Data_flow.t

(** Map the dataflow analysis accumulator of the given dacc. *)
val map_data_flow : t -> f:(Data_flow.t -> Data_flow.t) -> t

include Continuation_uses_env_intf.S with type t := t

val continuation_uses_env : t -> Continuation_uses_env.t

val with_continuation_uses_env : t -> cont_uses_env:Continuation_uses_env.t -> t

(** Mark that an exception handler continuation should be converted to a normal
    continuation. This is used when turning local exceptions into jumps. *)
val demote_exn_handler : t -> Continuation.t -> t

val demoted_exn_handlers : t -> Continuation.Set.t

val code_age_relation : t -> Code_age_relation.t

val with_code_age_relation : t -> Code_age_relation.t -> t

val typing_env : t -> Flambda2_types.Typing_env.t

val add_variable : t -> Bound_var.t -> Flambda2_types.t -> t

val no_lifted_constants : t -> bool

val add_to_lifted_constant_accumulator :
  ?also_add_to_env:unit -> t -> Lifted_constant_state.t -> t

val get_lifted_constants : t -> Lifted_constant_state.t

val get_and_clear_lifted_constants : t -> t * Lifted_constant_state.t

val clear_lifted_constants : t -> t

val set_lifted_constants : t -> Lifted_constant_state.t -> t

val find_shareable_constant : t -> Static_const.t -> Symbol.t option

val consider_constant_for_sharing : t -> Symbol.t -> Static_const.t -> t

val with_shareable_constants :
  t -> shareable_constants:Symbol.t Static_const.Map.t -> t

val shareable_constants : t -> Symbol.t Static_const.Map.t

val add_use_of_closure_var : t -> Var_within_closure.t -> t

val used_closure_vars : t -> Name_occurrences.t

val with_used_closure_vars : t -> used_closure_vars:Name_occurrences.t -> t

val add_code_ids_to_remember : t -> Code_id.Set.t -> t

val code_ids_to_remember : t -> Code_id.Set.t

val with_code_ids_to_remember : t -> code_ids_to_remember:Code_id.Set.t -> t

val set_do_not_rebuild_terms_and_disable_inlining : t -> t

val are_rebuilding_terms : t -> Are_rebuilding_terms.t

val do_not_rebuild_terms : t -> bool

val closure_offsets : t -> Closure_offsets.t Code_id.Map.t

val with_closure_offsets :
  t -> closure_offsets:Closure_offsets.t Code_id.Map.t -> t
