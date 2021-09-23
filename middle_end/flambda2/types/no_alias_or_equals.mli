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

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit
end) : sig
  type t =
    | No_alias of Head.t Or_unknown_or_bottom.t
    | Equals of Simple.t

  val print : Format.formatter -> t -> unit

  val apply_renaming : t -> Renaming.t -> t

  val free_names : t -> Name_occurrences.t
end
