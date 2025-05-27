(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper for emitting the various DWARF sections required for full debugging
    information. *)

open Asm_targets
open Dwarf_low

module State : sig
  (** DWARF IR for a single compilation unit. *)
  type t

  val create :
    compilation_unit_proto_die:Proto_die.t ->
    compilation_unit_header_label:Asm_label.t ->
    debug_loc_table:Debug_loc_table.t ->
    debug_ranges_table:Debug_ranges_table.t ->
    address_table:Address_table.t ->
    location_list_table:Location_list_table.t ->
    t

  val compilation_unit_proto_die : t -> Proto_die.t

  val compilation_unit_header_label : t -> Asm_label.t

  val debug_loc_table : t -> Debug_loc_table.t

  val debug_ranges_table : t -> Debug_ranges_table.t

  val address_table : t -> Address_table.t

  val location_list_table : t -> Location_list_table.t
end

(** DWARF IR for one or more compilation units, which will be converted to
    DWARF as a single entity. *)
type t

val create : State.t list -> t

val emit :
  t ->
  asm_directives:Asm_directives_dwarf.t ->
  Asm_section.normal_or_dwo ->
  basic_block_sections:bool ->
  binary_backend_available:bool ->
  unit
