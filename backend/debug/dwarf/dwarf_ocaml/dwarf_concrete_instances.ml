(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high

type fundecl =
  { fun_name : string;
    fun_dbg : Debuginfo.t;
    fun_end_label : Asm_label.t
  }

module DAH = Dwarf_attribute_helpers

let for_fundecl ~get_file_id state fundecl =
  let parent = Dwarf_state.compilation_unit_proto_die state in
  let fun_name = fundecl.fun_name in
  let linkage_name =
    match fundecl.fun_dbg with
    | [item] -> Debuginfo.Scoped_location.string_of_scopes item.dinfo_scopes
    (* Not sure what to do in the cases below *)
    | [] | _ :: _ -> fun_name
  in
  let start_sym = Asm_symbol.create fun_name in
  let location_attributes =
    let loc = Debuginfo.to_location fundecl.fun_dbg in
    if Location.is_none loc
    then [DAH.create_artificial ()]
    else
      let file, line, startchar = Location.get_pos_info loc.loc_start in
      let attributes = [DAH.create_decl_file (get_file_id file)] in
      if line < 0
      then attributes
      else if startchar < 0
      then DAH.create_decl_line line :: attributes
      else
        (* Both line and startchar are >= 0 *)
        DAH.create_decl_line line
        :: DAH.create_decl_column startchar
        :: attributes
  in
  let attribute_values =
    location_attributes
    @ [ DAH.create_name fun_name;
        DAH.create_linkage_name ~linkage_name;
        DAH.create_low_pc_from_symbol start_sym;
        DAH.create_high_pc ~low_pc:start_sym fundecl.fun_end_label;
        (* CR mshinwell: Probably no need to set this at the moment since the
           low PC value should be assumed, which is correct. *)
        DAH.create_entry_pc_from_symbol start_sym;
        DAH.create_stmt_list
          ~debug_line_label:(Asm_label.for_dwarf_section Asm_section.Debug_line)
      ]
  in
  let _concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values ()
  in
  (* CR mshinwell: When cross-referencing of DIEs across files is necessary we
     need to be careful about symbol table size. let name = Printf.sprintf
     "__concrete_instance_%s" fun_name in Proto_die.set_name
     concrete_instance_proto_die (Asm_symbol.create name) *)
  ()
