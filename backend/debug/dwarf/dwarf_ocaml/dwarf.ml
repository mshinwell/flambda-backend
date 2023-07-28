(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high
open Dwarf_low
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state

type t =
  { state : DS.t;
    asm_directives : (module Asm_directives.S);
    get_file_id : string -> int;
    mutable emitted : bool
  }

(* CR mshinwell: On OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let create ~sourcefile ~unit_name ~asm_directives ~get_file_id ~code_begin
    ~code_end =
  (match !Dwarf_flags.gdwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four);
  let compilation_unit_proto_die =
    Dwarf_compilation_unit.compile_unit_proto_die ~sourcefile ~unit_name
      ~code_begin ~code_end
  in
  let compilation_unit_header_label = Asm_label.create (DWARF Debug_info) in
  let value_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die) ~tag:Base_type
      ~attribute_values:
        [ DAH.create_name "ocaml_value";
          DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr ]
      ()
  in
  let start_of_code_symbol =
    Cmm_helpers.make_symbol "code_begin" |> Asm_symbol.create
    (* Dwarf_name_laundry.mangle_symbol Text (Symbol.of_global_linkage
       (Compilation_unit.get_current_exn ()) (Linkage_name.create
       "code_begin")) *)
  in
  let debug_loc_table = Debug_loc_table.create () in
  let debug_ranges_table = Debug_ranges_table.create () in
  let address_table = Address_table.create () in
  let location_list_table = Location_list_table.create () in
  let state =
    DS.create ~compilation_unit_header_label ~compilation_unit_proto_die
      ~value_type_proto_die ~start_of_code_symbol debug_loc_table
      debug_ranges_table address_table location_list_table
  in
  { state; asm_directives; emitted = false; get_file_id }

let dwarf_for_fundecl t (result : Debug_passes.result) =
  Dwarf_concrete_instances.for_fundecl ~get_file_id:t.get_file_id t.state
    result.fundecl result.available_ranges_vars

let emit t =
  if t.emitted
  then
    Misc.fatal_error
      "Cannot call [Dwarf.emit] more than once on a given value of type \
       [Dwarf.t]";
  t.emitted <- true;
  Dwarf_world.emit ~asm_directives:t.asm_directives
    ~compilation_unit_proto_die:(DS.compilation_unit_proto_die t.state)
    ~compilation_unit_header_label:(DS.compilation_unit_header_label t.state)
    ~debug_loc_table:(DS.debug_loc_table t.state)
    ~debug_ranges_table:(DS.debug_ranges_table t.state)
    ~address_table:(DS.address_table t.state)
    ~location_list_table:(DS.location_list_table t.state)

let emit t = Profile.record "emit_dwarf" emit t
