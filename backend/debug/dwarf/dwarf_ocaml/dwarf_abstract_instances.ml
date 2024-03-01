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

let add_root state ~function_proto_die:parent ~demangled_name fun_symbol
    ~location_attributes =
  let attributes =
    [ DAH.create_name (Asm_symbol.encode fun_symbol);
      DAH.create_linkage_name ~linkage_name:demangled_name;
      DAH.create_external ~is_visible_externally:true ]
    @ location_attributes
  in
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram
      ~attribute_values:
        (attributes
        @ [ (* We assume every function might potentially be inlined (and
               possibly in the future), so we choose [DW_INL_inlined] as the
               most appropriate setting for [DW_AT_inline], even if it doesn't
               seem exactly correct. We must set something here to ensure that
               the subprogram is marked as an abstract instance root. *)
            DAH.create_inline Inlined ])
      ()
  in
  let abstract_instance_proto_die_symbol =
    Asm_symbol.create (Asm_symbol.to_raw_string fun_symbol ^ "_absinst")
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  (* CR mshinwell maybe this should actually use the symbol name as the key, but
     then the following problem arises: we would need to be able to get the code
     ID or symbol name from the [Debuginfo.t] scopes info, e.g. during
     processing of inlined frames. *)
  Misc.Stdlib.String.Tbl.add
    (DS.function_abstract_instances state)
    demangled_name
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let find state ~function_proto_die (dbg : Debuginfo.t) =
  let demangled_name =
    match List.rev dbg with
    | [] -> Misc.fatal_error "Empty Debuginfo.t"
    | { dinfo_scopes; _ } :: _ ->
      Debuginfo.Scoped_location.string_of_scopes dinfo_scopes
  in
  match
    Misc.Stdlib.String.Tbl.find
      (DS.function_abstract_instances state)
      demangled_name
  with
  | exception Not_found ->
    Misc.fatal_errorf "No abstract instance found for function %s"
      demangled_name
  | existing_instance -> existing_instance
(* let find_maybe_in_another_unit_or_add state ~function_proto_die
   (fundecl:Linear.fundecl)= (* if not (Debuginfo.Function.dwarf_die_present
   fun_dbg) then None else *) let dbg_comp_unit =
   Debuginfo.Function.Id.compilation_unit id in let this_comp_unit =
   Compilation_unit.get_current_exn () in if Compilation_unit.equal
   dbg_comp_unit this_comp_unit then let _abstract_instance_proto_die,
   abstract_instance_proto_die_symbol = find_or_add state ~function_proto_die
   fun_dbg in Some abstract_instance_proto_die_symbol else if
   DS.can_reference_dies_across_units state then Some
   (Dwarf_name_laundry.abstract_instance_root_die_name id) else None *)
