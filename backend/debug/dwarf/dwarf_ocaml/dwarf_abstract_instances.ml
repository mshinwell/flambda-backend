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

open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear

let attributes fun_name =
  [DAH.create_name fun_name; DAH.create_external ~is_visible_externally:true]

let abstract_instance_proto_die_symbol ~fun_symbol =
  Asm_symbol.create (Asm_symbol.to_raw_string fun_symbol ^ "_absinst")

let add_empty state ~parent ~fun_symbol =
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values:[]
      ()
  in
  let abstract_instance_proto_die_symbol =
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Asm_symbol.Tbl.add
    (DS.function_abstract_instances state)
    fun_symbol
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let add_root state ~function_proto_die:parent ~demangled_name fun_symbol
    ~location_attributes =
  let attributes =
    [ DAH.create_name (Asm_symbol.encode fun_symbol);
      DAH.create_linkage_name ~linkage_name:demangled_name;
      DAH.create_external ~is_visible_externally:true ]
    @ location_attributes
  in
  let attribute_values =
    attributes
    @ [ (* We assume every function might potentially be inlined (and possibly
           in the future), so we choose [DW_INL_inlined] as the most appropriate
           setting for [DW_AT_inline], even if it doesn't seem exactly correct.
           We must set something here to ensure that the subprogram is marked as
           an abstract instance root. *)
        DAH.create_inline Inlined ]
  in
  let abstract_instance_proto_die_symbol =
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  Format.eprintf "add_root: fun_symbol=%a\n" Asm_symbol.print fun_symbol;
  let abstract_instance_proto_die =
    match
      Asm_symbol.Tbl.find (DS.function_abstract_instances state) fun_symbol
    with
    | proto_die, _symbol ->
      (* See below in [find] *)
      Proto_die.replace_all_attribute_values proto_die attribute_values
    | exception Not_found ->
      (* DWARF-5 specification section 3.3.8.1, page 82. *)
      Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values
        ()
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Asm_symbol.Tbl.add (* or replace *)
    (DS.function_abstract_instances state)
    fun_symbol
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let find state ~function_proto_die (dbg : Debuginfo.t) =
  let orig_dbg = dbg in
  let fun_symbol, dbg_comp_unit, _item =
    match List.rev (Debuginfo.to_items dbg) with
    | [({ dinfo_scopes; dinfo_function_symbol; _ } as item)] -> (
      let module S = Debuginfo.Scoped_location in
      let compilation_unit = S.compilation_unit dinfo_scopes in
      let dbg = Debuginfo.of_items [item] in
      let function_symbol =
        match dinfo_function_symbol with
        | Some dinfo_function_symbol -> Asm_symbol.create dinfo_function_symbol
        | None ->
          Misc.fatal_errorf
            "No function symbol in Debuginfo.t: orig_dbg=%a dbg=%a"
            Debuginfo.print_compact orig_dbg Debuginfo.print_compact dbg
      in
      match compilation_unit with
      | Some compilation_unit -> function_symbol, compilation_unit, item
      | None ->
        Misc.fatal_errorf "No compilation unit extracted from: %a"
          Debuginfo.print_compact dbg)
    | [] -> Misc.fatal_error "Empty Debuginfo.t"
    | _ :: _ ->
      Misc.fatal_errorf "Non-singleton Debuginfo.t: %a" Debuginfo.print_compact
        dbg
  in
  (* CR mshinwell: think more about fabrication of DIEs for other units in the
     event that cross-unit references are not permitted *)
  Format.eprintf "found comp unit %a\n%!" Compilation_unit.print dbg_comp_unit;
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if Compilation_unit.equal dbg_comp_unit this_comp_unit
     || not (DS.can_reference_dies_across_units state)
  then (
    Format.eprintf "looking in function_abstract_instances for %a\n%!"
      Asm_symbol.print fun_symbol;
    match
      Asm_symbol.Tbl.find (DS.function_abstract_instances state) fun_symbol
    with
    | existing_instance ->
      Format.eprintf "...successfully found existing absint DIE\n%!";
      existing_instance
    | exception Not_found ->
      (* Fabricate an empty abstract instance DIE to fill in later. *)
      Format.eprintf "...making empty absint DIE for %a\n" Asm_symbol.print
        fun_symbol;
      add_empty state ~parent:function_proto_die ~fun_symbol)
  else
    (* CR mshinwell: use Dwarf_name_laundry.abstract_instance_root_die_name *)
    Misc.fatal_errorf
      "Abstract instance DIE references across units not supported yet: %a"
      Debuginfo.print_compact dbg
