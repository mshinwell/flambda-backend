(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** State that is shared amongst the various dwarf_* modules. *)

open Asm_targets
open Dwarf_low
open Dwarf_high

type t

type dwarf_state = t

val create :
  Asm_section.normal_or_dwo ->
  compilation_unit_header_label:Asm_label.t ->
  compilation_unit_proto_die:Proto_die.t ->
  value_type_proto_die:Proto_die.t option ->
  (* [value_type_proto_die] is not provided for skeleton units. *)
  start_of_code_symbol:Asm_symbol.t ->
  Address_table.t ->
  Location_list_table.t ->
  Range_list_table.t ->
  t

val normal_or_dwo : t -> Asm_section.normal_or_dwo

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val value_type_proto_die : t -> Proto_die.t option

val start_of_code_symbol : t -> Asm_symbol.t

val address_table : t -> Address_table.t

val location_list_table : t -> Location_list_table.t

val range_list_table : t -> Range_list_table.t

val function_abstract_instances :
  t -> (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t

val can_reference_dies_across_units : t -> bool

module Debug : sig
  val log : ('a, Format.formatter, unit) format -> 'a
end

module Serialized : sig
  type t

  val create : dwarf_state -> t

  val to_dwarf_state : t -> dwarf_state
end

val get_dwarf_world_state : t -> Dwarf_world.State.t
