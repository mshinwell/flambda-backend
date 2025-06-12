(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

val meet :
  Typing_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t * Typing_env.t) Or_bottom.t

val meet_type : unit -> Typing_env.meet_type

val meet_shape :
  Typing_env.t ->
  Type_grammar.t ->
  shape:Type_grammar.t ->
  Typing_env.t Or_bottom.t
