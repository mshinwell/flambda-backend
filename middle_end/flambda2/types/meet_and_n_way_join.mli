(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Greatest lower bound of two types. *)
val meet :
  Typing_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t * Typing_env.t) Or_bottom.t

(** Least upper bound of many types. *)
val n_way_join :
  Join_env.t ->
  Type_grammar.t Join_env.join_arg list ->
  Type_grammar.t Or_unknown.t * Join_env.t

val meet_shape :
  Typing_env.t ->
  Type_grammar.t ->
  shape:Type_grammar.t ->
  Typing_env.t Or_bottom.t

(* This function has a slightly different interface; it is meant to be used only
   by functions in Typing_env *)
val meet_type :
  Typing_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t Typing_env.meet_return_value * Typing_env.t) Or_bottom.t
