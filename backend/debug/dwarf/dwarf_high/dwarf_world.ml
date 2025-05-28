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

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_low
module A = Asm_directives

module State = struct
  type t =
    { compilation_unit_proto_die : Proto_die.t;
      compilation_unit_header_label : Asm_label.t;
      address_table : Address_table.t;
      location_list_table : Location_list_table.t;
      range_list_table : Range_list_table.t
    }

  let create ~compilation_unit_proto_die ~compilation_unit_header_label
      ~address_table ~location_list_table ~range_list_table =
    { compilation_unit_proto_die;
      compilation_unit_header_label;
      address_table;
      location_list_table;
      range_list_table
    }

  let compilation_unit_proto_die t = t.compilation_unit_proto_die

  let compilation_unit_header_label t = t.compilation_unit_header_label

  let address_table t = t.address_table

  let location_list_table t = t.location_list_table

  let range_list_table t = t.range_list_table
end

type t = { states : State.t list }

let create states = { states }

let emit_for_one_unit ~asm_directives
    (normal_or_dwo : Asm_section.normal_or_dwo) (state : State.t)
    assigned_abbrevs =
  let compilation_unit_header_label = state.compilation_unit_header_label in
  let address_table = state.address_table in
  let location_list_table = state.location_list_table in
  let range_list_table = state.range_list_table in
  let assigned_abbrevs =
    Profile.record "assign_abbrevs"
      (fun () ->
        Assign_abbrevs.run assigned_abbrevs
          ~proto_die_root:state.compilation_unit_proto_die)
      ()
  in
  let debug_abbrev_label =
    Asm_label.for_dwarf_section (Debug_abbrev normal_or_dwo)
  in
  let debug_info =
    Profile.record "debug_info_section"
      (fun () ->
        Debug_info_section.create normal_or_dwo ~dies:assigned_abbrevs.dies
          ~debug_abbrev_label ~compilation_unit_header_label)
      ()
  in
  Profile.record "dwarf_world_emit"
    (fun () ->
      A.switch_to_section (DWARF (Debug_info normal_or_dwo));
      Profile.record "debug_info_section"
        (Debug_info_section.emit ~asm_directives)
        debug_info;
      (* .debug_addr is only emitted during compilation, to the object file. It
         never appears in a .dwo section. When generating split DWARF, both the
         skeleton and main states share the address table, but only the former
         will emit it. *)
      (match normal_or_dwo with
      | Normal ->
        Profile.record "addr_table"
          (Address_table.emit ~asm_directives)
          address_table
      | Dwo -> ());
      A.switch_to_section (DWARF (Debug_loclists normal_or_dwo));
      Profile.record "loclists_table"
        (Location_list_table.emit ~asm_directives)
        location_list_table;
      Profile.record "rnglists_table"
        (Range_list_table.emit ~asm_directives)
        range_list_table)
    ();
  assigned_abbrevs

let emit t ~asm_directives normal_or_dwo ~basic_block_sections
    ~binary_backend_available =
  if (* CR mshinwell: support function sections *)
     !Clflags.function_sections || basic_block_sections
     (* CR mshinwell: support the internal assembler *)
     || binary_backend_available
  then ()
  else
    let assigned_abbrevs =
      List.fold_left
        (fun assigned_abbrevs state ->
          emit_for_one_unit ~asm_directives normal_or_dwo state assigned_abbrevs)
        (Assign_abbrevs.create ()) t.states
    in
    Profile.record "emit_debug_abbrev"
      (fun () ->
        A.switch_to_section (DWARF (Debug_abbrev normal_or_dwo));
        Profile.record "abbreviations_table"
          (Abbreviations_table.emit ~asm_directives)
          assigned_abbrevs.abbrev_table)
      ();
    Profile.record "emit_debug_str"
      (fun () ->
        A.switch_to_section (DWARF Debug_str);
        A.emit_cached_strings ())
      ()
