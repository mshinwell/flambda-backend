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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** A [Name] equipped with extra information that arises from the name occurring
    in binding position. *)

type t

val create : Name.t -> Name_mode.t -> t

val create_var : Bound_var.t -> t

val create_symbol : Symbol.t -> t

val name : t -> Name.t

val name_mode : t -> Name_mode.t

val is_symbol : t -> bool

val print : Format.formatter -> t -> unit
