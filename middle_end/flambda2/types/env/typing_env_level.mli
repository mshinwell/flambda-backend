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

  val all_ids_for_export : t -> Ids_for_export.t

  val free_names : t -> Name_occurrences.t

  val apply_renaming : t -> Renaming.t -> t

  val print : Format.formatter -> t -> unit

  val check_equation : Name.t -> t -> unit

  val is_obviously_unknown : t -> bool
end) : Typing_env_level_intf.S with type flambda_type := Type_grammar.t
