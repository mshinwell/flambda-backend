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

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val print_with_extra_fields :
  (Format.formatter -> unit) -> Format.formatter -> t -> unit

val is_empty : t -> bool

(** Create a set of closures given the code for its functions and the closure
    variables. See below regarding [synthetic_value_slots]. *)
val create :
  ?is_specialisation_site:bool ->
  ?synthetic_value_slots:Simple.t Value_slot.Map.t ->
  value_slots:Simple.t Value_slot.Map.t ->
  Function_declarations.t ->
  t

(** A specialisation site is a closed set of closures left behind by lambda
    lifting (see [Rebuild.rebuild_specialisation_carrier]). Its binding is
    retained, in code that may be simplified again, as long as the code of one
    of its functions may still be called, even if no call uses its closures as
    callees and even if its synthetic value slots have been removed. The
    contents of the synthetic value slots are weak hints: retaining the site
    must not keep otherwise dead values alive. *)
val is_specialisation_site : t -> bool

(** The function declarations associated with the set of closures. *)
val function_decls : t -> Function_declarations.t

(** The values of each value slot (the environment, or captured variables). *)
val value_slots : t -> Simple.t Value_slot.Map.t

(** Value slots which are not allocated in the closures, but whose contents are
    nonetheless recorded, because some parameters of the functions in the set
    are known to be equal to them (see [Function_params_and_body.create]). Such
    slots arise when the reaper lambda-lifts a function: a value slot that is
    turned into a parameter is moved here, so that the function can still be
    specialised on it if the set of closures is simplified again (typically in
    another compilation unit, after inlining). The code of the functions must
    not project these slots. Dropping any of them is always sound. *)
val synthetic_value_slots : t -> Simple.t Value_slot.Map.t

(** Returns true iff the given set of closures has no value slots. *)
val is_closed : t -> bool

val filter_function_declarations :
  t ->
  f:
    (Function_slot.t ->
    Function_declarations.code_id_in_function_declaration ->
    bool) ->
  t

include Container_types.S with type t := t

(** Replace the value slots (of both kinds), keeping everything else. *)
val with_value_slots :
  t ->
  value_slots:Simple.t Value_slot.Map.t ->
  synthetic_value_slots:Simple.t Value_slot.Map.t ->
  t
