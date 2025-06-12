(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Handling of the extra params and args required for the unboxing of a
    continuation's parameter(s). *)

exception Invalid_apply_cont

type unboxed_arg =
  | Poison (* used for recursive calls *)
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int }

val compute_extra_args_for_one_decision_and_use :
  pass:Unboxing_types.pass ->
  Apply_cont_rewrite_id.t ->
  typing_env_at_use:Flambda2_types.Typing_env.t ->
  unboxed_arg ->
  Unboxing_types.decision ->
  Unboxing_types.decision

val add_extra_params_and_args :
  Continuation_extra_params_and_args.t ->
  invalids:Apply_cont_rewrite_id.Set.t ->
  Unboxing_types.decision ->
  Continuation_extra_params_and_args.t
