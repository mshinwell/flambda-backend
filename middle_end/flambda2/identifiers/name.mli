(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** The sum type holding a [Variable] or a [Symbol]. The injection functions
    into the sum type are the identity. *)

include module type of struct
  include Int_ids.Name
end

val set_of_var_set : Variable.Set.t -> Set.t

val set_of_symbol_set : Symbol.Set.t -> Set.t

val set_to_var_set : Set.t -> Variable.Set.t

val set_to_symbol_set : Set.t -> Symbol.Set.t

val is_var : t -> bool

val is_symbol : t -> bool

val must_be_symbol : t -> Symbol.t

val compilation_unit : t -> Compilation_unit.t

val is_imported : t -> bool

val must_be_var_opt : t -> Variable.t option

val must_be_symbol_opt : t -> Symbol.t option

module Pair : sig
  type nonrec t = t * t

  include Container_types.S with type t := t
end
