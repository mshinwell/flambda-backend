(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Greatest lower bound of two types. *)
val meet :
  Typing_env.Meet_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t * Typing_env_extension.t) Or_bottom.t

(** Least upper bound of two types. *)
val join :
  ?bound_name:Name.t ->
  Typing_env.Join_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  Type_grammar.t Or_unknown.t

val meet_shape :
  Typing_env.t ->
  Type_grammar.t ->
  shape:Type_grammar.t ->
  result_var:Bound_var.t ->
  result_kind:Flambda_kind.t ->
  Typing_env_extension.t Or_bottom.t

val meet_env_extension :
  Typing_env.Meet_env.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t Or_bottom.t
