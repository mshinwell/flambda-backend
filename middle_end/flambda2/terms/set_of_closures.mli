(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val is_empty : t -> bool

(** Create a set of closures given the code for its functions and the closure
    variables. *)
val create :
  value_slots:Simple.t Value_slot.Map.t ->
  Alloc_mode.For_allocations.t ->
  Function_declarations.t ->
  t

(** The function declarations associated with the set of closures. *)
val function_decls : t -> Function_declarations.t

(** The values of each value slot (the environment, or captured variables). *)
val value_slots : t -> Simple.t Value_slot.Map.t

(** Returns true iff the given set of closures has no value slots. *)
val is_closed : t -> bool

val alloc_mode : t -> Alloc_mode.For_allocations.t

val filter_function_declarations :
  t ->
  f:
    (Function_slot.t ->
    Function_declarations.code_id_in_function_declaration ->
    bool) ->
  t

include Container_types.S with type t := t
