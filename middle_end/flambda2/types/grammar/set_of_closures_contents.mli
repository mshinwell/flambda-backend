(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Descriptions of the entities inside sets of closures: - closures; - closure
    variables. These descriptions do not necessarily describe the entire
    contents of any particular set of closures. *)

type t

val create : Function_slot.Set.t -> Value_slot.Set.t -> t

include Container_types.S with type t := t

val subset : t -> t -> bool

val inter : t -> t -> t

val union : t -> t -> t

val closures : t -> Function_slot.Set.t

val value_slots : t -> Value_slot.Set.t

include Contains_names.S with type t := t

val remove_unused_value_slots : t -> used_value_slots:Value_slot.Set.t -> t

module With_function_slot : sig
  type nonrec t = Function_slot.t * t

  include Container_types.S with type t := t
end

module With_function_slot_or_unknown : sig
  type nonrec t = Function_slot.t Or_unknown.t * t

  include Container_types.S with type t := t
end
