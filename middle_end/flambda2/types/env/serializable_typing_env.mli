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

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val create :
  defined_symbols:Symbol.Set.t ->
  code_age_relation:Code_age_relation.t ->
  just_after_level:Cached_level.t ->
  next_binding_time:Binding_time.t ->
  t

val print : Format.formatter -> t -> unit

val all_ids_for_export : t -> Ids_for_export.t

val apply_renaming : t -> Renaming.t -> t

val merge : t -> t -> t

val defined_symbols : t -> Symbol.Set.t

val code_age_relation : t -> Code_age_relation.t

val just_after_level : t -> Cached_level.t

val next_binding_time : t -> Binding_time.t
