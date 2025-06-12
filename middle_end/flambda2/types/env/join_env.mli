(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type env_id

type 'a join_arg = env_id * 'a

type t

val target_join_env : t -> Typing_env.t

val joined_env : t -> env_id -> Typing_env.t

val n_way_join_simples :
  t -> Flambda_kind.t -> Simple.t join_arg list -> Simple.t Or_bottom.t * t

type n_way_join_type =
  t -> Type_grammar.t join_arg list -> Type_grammar.t Or_unknown.t * t

val n_way_join_env_extension :
  n_way_join_type:n_way_join_type ->
  meet_type:Typing_env.meet_type ->
  t ->
  Typing_env_extension.t join_arg list ->
  (Typing_env_extension.t * t) Or_bottom.t

val cut_and_n_way_join :
  n_way_join_type:n_way_join_type ->
  meet_type:Typing_env.meet_type ->
  cut_after:Scope.t ->
  Typing_env.t ->
  Typing_env.t list ->
  Typing_env.t
