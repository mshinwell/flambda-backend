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

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_low
open Dwarf_high

type t =
  { normal_or_dwo : Asm_section.normal_or_dwo;
    state : Dwarf_world.State.t;
    value_type_proto_die : Proto_die.t option;
    start_of_code_symbol : Asm_symbol.t;
    function_abstract_instances : (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t
  }

type dwarf_state = t

let create normal_or_dwo ~compilation_unit_header_label
    ~compilation_unit_proto_die ~value_type_proto_die ~start_of_code_symbol
    address_table location_list_table range_list_table =
  let state =
    Dwarf_world.State.create ~compilation_unit_header_label
      ~compilation_unit_proto_die ~address_table ~location_list_table
      ~range_list_table
  in
  { normal_or_dwo;
    state;
    value_type_proto_die;
    start_of_code_symbol;
    function_abstract_instances = Asm_symbol.Tbl.create 42
  }

let normal_or_dwo t = t.normal_or_dwo

let get_dwarf_world_state t = t.state

let compilation_unit_header_label t =
  Dwarf_world.State.compilation_unit_header_label t.state

let compilation_unit_proto_die t =
  Dwarf_world.State.compilation_unit_proto_die t.state

let value_type_proto_die t = t.value_type_proto_die

let start_of_code_symbol t = t.start_of_code_symbol

let address_table t = Dwarf_world.State.address_table t.state

let location_list_table t = Dwarf_world.State.location_list_table t.state

let range_list_table t = Dwarf_world.State.range_list_table t.state

let function_abstract_instances t = t.function_abstract_instances

let can_reference_dies_across_units _t = true

module Debug = struct
  let log f =
    match Sys.getenv "DWARF_DEBUG" with
    | exception Not_found -> Format.ifprintf Format.err_formatter f
    | _ -> Format.eprintf f
end

module Serialized = struct
  type t = dwarf_state

  let create t = t

  let to_dwarf_state t = t
end
