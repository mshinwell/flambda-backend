(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Compute the typing environment for a join point given the environments at
    each of the corresponding continuation's uses. *)

val cut_and_n_way_join :
  Typing_env.t ->
  (Typing_env.t * Apply_cont_rewrite_id.t * Continuation_use_kind.t) list ->
  params:Bound_parameters.t ->
  cut_after:Scope.t ->
  extra_lifted_consts_in_use_envs:Symbol.Set.t ->
  extra_allowed_names:Name_occurrences.t ->
  Typing_env.t
