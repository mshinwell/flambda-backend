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

module type S = sig
  type typing_env

  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> t

  val env : t -> typing_env

  (** Note that we are now in the process of meeting the given two [Simple]s. *)
  val now_meeting : t -> Simple.t -> Simple.t -> t

  (** Determine whether we are now in the process of meeting the given two
      [Simple]s. The arguments do not have to be provided in the same order as
      when [now_meeting] was called. *)
  val already_meeting : t -> Simple.t -> Simple.t -> bool
end
