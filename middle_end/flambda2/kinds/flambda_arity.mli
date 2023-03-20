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

(** Arities are used to describe things such as the kinding of function and
    continuation parameter lists. *)

type t

type for_creation =
  | Singleton of Flambda_kind.With_subkind.t
    (* The nullary unboxed product is called "void". *)
  | Unboxed_product of for_creation list

(** One component per function or continuation parameter, for example. Each
    component may in turn have an arity describing an unboxed product. *)
val create : for_creation list -> t

val create_singletons : Flambda_kind.With_subkind.t list -> t

(** "No parameters".  (Not e.g. "one parameter of type void".) *)
val nullary : t

val print : Format.formatter -> t -> unit

val equal_ignoring_subkinds : t -> t -> bool

val cardinal_not_unarized : t -> int

val is_singleton_value_not_unarized : t -> bool

module Component : sig
  type t = private
    | Singleton of Flambda_kind.With_subkind.t
    | Unboxed_product of t list
end

val to_list_not_unarized : t -> Component.t list

(** Convert, in a left-to-right depth-first order, an arity into a flattened
    list of kinds for each parameter. *)
val unarize : t -> Flambda_kind.With_subkind.t list list
