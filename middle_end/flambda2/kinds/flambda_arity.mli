(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Arities are lists of kinds (with subkinds) used to describe things
    such as the kinding of function and continuation parameter lists. *)

type t

val nullary : t

type for_creation =
  | Singleton of Flambda_kind.With_subkind.t
  | Unboxed_product of for_creation list

val create : for_creation list -> t

val create_singletons : Flambda_kind.With_subkind.t list -> t

val unarize : t -> Flambda_kind.With_subkind.t list

val cardinal_not_unarized : t -> int

val is_singleton_value_not_unarized : t -> bool

val print : Format.formatter -> t -> unit

val equal_ignoring_subkinds : t -> t -> bool

module Component : sig
  type t = private
    | Singleton of Flambda_kind.With_subkind.t
    | Unboxed_product of t list
end

val to_list_not_unarized : t -> Component.t list
