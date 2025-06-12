(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Compute, for each parameter of a continuation, the join of all corresponding
    argument types across the recorded uses; together with the environment to be
    used for simplifying the continuation itself. *)

type result = private
  { handler_env : Downwards_env.t;
    extra_params_and_args : Continuation_extra_params_and_args.t;
    is_single_inlinable_use : bool;
    escapes : bool
  }

val compute_handler_env :
  ?replay:Replay_history.t * bool ->
  ?cut_after:Scope.t ->
  One_continuation_use.t list ->
  is_recursive:bool ->
  env_at_fork:Downwards_env.t ->
  consts_lifted_after_fork:Lifted_constant_state.t ->
  params:Bound_parameters.t ->
  previous_extra_params_and_args:Continuation_extra_params_and_args.t ->
  result
