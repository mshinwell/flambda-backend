(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module type S = sig
  include Container_types.S

  module Lmap : Lmap.S with type key = t

  val create :
    Compilation_unit.t ->
    name:string ->
    is_always_immediate:bool ->
    Flambda_kind.t ->
    t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val to_string : t -> string

  val name : t -> string

  val kind : t -> Flambda_kind.t

  val is_always_immediate : t -> bool

  val rename : t -> t
end

module Make (_ : sig
  val colour : Format.formatter -> unit
end) : S
