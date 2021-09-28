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

module Make (Typing_env : sig
  type t

  val print : Format.formatter -> t -> unit
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : Typing_env.t -> t

  val env : t -> Typing_env.t

  val now_meeting : t -> Simple.t -> Simple.t -> t

  val already_meeting : t -> Simple.t -> Simple.t -> bool
end
