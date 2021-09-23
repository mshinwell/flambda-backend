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

module Make (Head : sig
  type t

  include Contains_ids.S with type t := t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val apply_coercion : t -> Coercion.t -> t Or_bottom.t
end) : sig
  module Descr : sig
    type t = private
      | No_alias of Head.t Or_unknown_or_bottom.t
      | Equals of Simple.t

    val print : Format.formatter -> t -> unit

    val apply_renaming : t -> Renaming.t -> t

    val free_names : t -> Name_occurrences.t
  end

  type t

  val descr : t -> Descr.t

  val peek_descr : t -> Descr.t

  val free_names : t -> Name_occurrences.t

  val apply_renaming : t -> Renaming.t -> t

  val all_ids_for_export : t -> Ids_for_export.t

  val print : Format.formatter -> t -> unit

  val create_no_alias : Head.t Or_unknown_or_bottom.t -> t

  val create_equals : Simple.t -> t

  val bottom : unit -> t

  val unknown : unit -> t

  val create : Head.t -> t

  val is_obviously_bottom : t -> bool

  val is_obviously_unknown : t -> bool

  val get_alias_exn : t -> Simple.t

  val apply_coercion : t -> Coercion.t -> t Or_bottom.t
end
