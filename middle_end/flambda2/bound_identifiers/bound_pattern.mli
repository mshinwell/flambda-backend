(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** The patterns that occur on the left-hand sides of [Let]-expressions. *)

type t = private
  | Variable of Bound_var.t
      (** The binding of a single variable, which is statically scoped. *)
  | Set_of_closures of Bound_var.t list
      (** The binding of one or more variables to the individual closures in a
          set of closures. The variables are statically scoped. *)
  | Symbols of Bound_symbols.t
      (** The binding of one or more symbols to statically-allocated
          constant(s). Symbols have dominator scoping. *)
  | Code of Code_id.t
      (** The binding of a piece of code to a code ID. Code IDs have dominator
          scoping, like symbols. *)

include Bindable.S with type t := t

include Contains_ids.S with type t := t

val variable : Bound_var.t -> t

val set_of_closures : Bound_var.t list -> t

val symbols : Bound_symbols.t -> t

val must_be_variable : t -> Bound_var.t

val must_be_variable_opt : t -> Bound_var.t option

val must_be_set_of_closures : t -> Bound_var.t list

val must_be_symbols : t -> Bound_symbols.t

val may_be_symbols : t -> Bound_symbols.t option

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

val fold_all_bound_vars : t -> init:'a -> f:('a -> Bound_var.t -> 'a) -> 'a

val fold_all_bound_names :
  t ->
  init:'a ->
  var:('a -> Bound_var.t -> 'a) ->
  symbol:('a -> Symbol.t -> 'a) ->
  code_id:('a -> Code_id.t -> 'a) ->
  'a
