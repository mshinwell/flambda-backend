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

type t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val empty : t

val create : Bound_parameter.t list -> t

val cons : Bound_parameter.t -> t -> t

val append : t -> t -> t

val to_list : t -> Bound_parameter.t list

val rename : t -> t

val renaming : t -> guaranteed_fresh:t -> Renaming.t

val is_empty : t -> bool

val same_number : t -> t -> bool

val arity : t -> Flambda_arity.t

val arity_with_subkinds : t -> Flambda_arity.With_subkinds.t

val check_no_duplicates : t -> unit

val cardinal : t -> int

val simples : t -> Reg_width_things.Simple.t list

val to_set : t -> Bound_parameter.Set.t

val vars : t -> Variable.t list

val var_set : t -> Variable.Set.t

val name_set : t -> Name.Set.t

val filter : (Bound_parameter.t -> bool) -> t -> t

val exists : (Bound_parameter.t -> bool) -> t -> bool
