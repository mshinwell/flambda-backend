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

let encode_name demangled_name (loc : Location.t) =
  (* XXX this isn't sufficient, we should find a way of recovering the code ID
     from the Debuginfo.t. The column numbers are missed out at present to give
     a higher chance of matches. *)
  let file, line, _chr = Location.get_pos_info loc.loc_start in
  Format.asprintf "%s.%s.%d" demangled_name file line

let add_empty state ~parent loc ~demangled_name =
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values:[]
      ()
  in
  let encoded_name = encode_name demangled_name loc in
  let fun_symbol = encoded_name |> Asm_symbol.create in
  let abstract_instance_proto_die_symbol =
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Misc.Stdlib.String.Tbl.add
    (DS.function_abstract_instances state)
    (encode_name demangled_name loc)
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let add_root state ~function_proto_die:parent loc ~demangled_name fun_symbol
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
    let encoded_name = encode_name demangled_name loc in
    let fun_symbol = encoded_name |> Asm_symbol.create in
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  let mangled_name = encode_name demangled_name loc in
  Format.eprintf "add_root: mangled_name=%s\n" mangled_name;
  let abstract_instance_proto_die =
    match
      Misc.Stdlib.String.Tbl.find
        (DS.function_abstract_instances state)
        mangled_name
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
  (* CR mshinwell maybe this should actually use the symbol name as the key, but
     then the following problem arises: we would need to be able to get the code
     ID or symbol name from the [Debuginfo.t] scopes info, e.g. during
     processing of inlined frames. *)
  Misc.Stdlib.String.Tbl.add (* or replace *)
    (DS.function_abstract_instances state)
    (encode_name demangled_name loc)
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let find state ~function_proto_die (dbg : Debuginfo.t) =
  let demangled_name, dbg_comp_unit, loc, _item =
    match List.rev (Debuginfo.to_items dbg) with
    | [({ dinfo_scopes; _ } as item)] -> (
      let module S = Debuginfo.Scoped_location in
      let demangled_name = S.string_of_scopes dinfo_scopes in
      let compilation_unit = S.compilation_unit dinfo_scopes in
      let dbg = Debuginfo.of_items [item] in
      let loc = Debuginfo.to_location dbg in
      match compilation_unit with
      | Some compilation_unit -> demangled_name, compilation_unit, loc, item
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
    let mangled_name = encode_name demangled_name loc in
    Format.eprintf "looking in function_abstract_instances for %s\n%!"
      mangled_name;
    match
      Misc.Stdlib.String.Tbl.find
        (DS.function_abstract_instances state)
        mangled_name
    with
    | existing_instance ->
      Format.eprintf "...successfully found existing absint DIE\n%!";
      existing_instance
    | exception Not_found ->
      (* Fabricate an empty abstract instance DIE to fill in later. *)
      Format.eprintf "...making empty absint DIE for %s\n" mangled_name;
      add_empty state ~parent:function_proto_die loc ~demangled_name)
  else
    (* CR mshinwell: use Dwarf_name_laundry.abstract_instance_root_die_name *)
    Misc.fatal_errorf
      "Abstract instance DIE references across units not supported yet: %a"
      Debuginfo.print_compact dbg
