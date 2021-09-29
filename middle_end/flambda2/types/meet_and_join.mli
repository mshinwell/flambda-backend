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

module Meet_env = Typing_env.Meet_env
module Join_env = Typing_env.Join_env
module TG = Type_grammar
module TEE = Typing_env_extension

(** Greatest lower bound of two types. *)
val meet : Meet_env.t -> TG.t -> TG.t -> (TG.t * TEE.t) Or_bottom.t

(** Least upper bound of two types. *)
val join : ?bound_name:Name.t -> Join_env.t -> TG.t -> TG.t -> TG.t Or_unknown.t
