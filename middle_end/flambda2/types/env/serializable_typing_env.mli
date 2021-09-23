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

module Make (Type_grammar : sig
  type t

  val print : Format.formatter -> t -> unit

  include Contains_ids.S with type t := t
end) (Cached : sig
  type t

  val apply_renaming : t -> Renaming.t -> t

  val aliases : t -> Aliases.t

  val symbol_projections : t -> Symbol_projection.t Variable.Map.t

  val names_to_types :
    t -> (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t

  val merge : t -> t -> t
end) : sig
  type t

  val create :
    defined_symbols:Symbol.Set.t ->
    code_age_relation:Code_age_relation.t ->
    just_after_level:Cached.t ->
    next_binding_time:Binding_time.t ->
    t

  val print : Format.formatter -> t -> unit

  val all_ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  val merge : t -> t -> t

  val defined_symbols : t -> Symbol.Set.t

  val code_age_relation : t -> Code_age_relation.t

  val just_after_level : t -> Cached.t

  val next_binding_time : t -> Binding_time.t
end
