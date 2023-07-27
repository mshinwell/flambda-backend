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
module ARV = Available_ranges_all_vars
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear
module SLDL = Simple_location_description_lang
module V = Backend_var

type is_variable_phantom = Non_phantom

type proto_dies_for_var =
  { is_variable_phantom : is_variable_phantom;
    value_die_lvalue : Proto_die.reference;
    value_die_rvalue : Proto_die.reference;
    type_die : Proto_die.reference
  }

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found -> None
  | result -> Some result

let normal_type_for_var ?reference ~parent ident_for_type is_parameter =
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
          is_parameter
      in
      [DAH.create_name name]
  in
  (* CR mshinwell: This should not create duplicates when the name is missing *)
  Proto_die.create ?reference ~parent ~tag:Base_type
    ~attribute_values:
      (name_attribute
      @ [ DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr ])
    ()

let type_die_reference_for_var var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some dies -> Some dies.type_die

(* Build a new DWARF type for [var]. Each variable has its own type, which is
   basically its stamped name, and is nothing to do with its inferred OCaml
   type. The inferred type may be recovered by the debugger by extracting the
   stamped name and then using that as a key for lookup into the .cmt file for
   the appropriate module.

   We emit the parameter index into the type if the variable in question is a
   function parameter. This is used in the debugger support library. It would be
   nice not to have to have this hack, but it avoids changes in the main gdb
   code to pass parameter indexes to the printing function. It is arguably more
   robust, too. *)
(* CR mshinwell: Add proper type for [ident_for_type] *)
let construct_type_of_value_description state ~parent ident_for_type
    is_parameter ~proto_dies_for_vars ~reference =
  (* XXX let normal_case () = *)
  let (_ : Proto_die.t) =
    normal_type_for_var ~reference ~parent ident_for_type is_parameter
  in
  ()
(* in let name_attribute = match ident_for_type with | None -> [] | Some
   (compilation_unit, var) -> let name =
   Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
   is_parameter in [DAH.create_name name] in normal_case () *)

let is_variable_phantom var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { is_variable_phantom; _ } -> Some is_variable_phantom

(* let die_location_of_variable_lvalue state var ~proto_dies_for_vars = (* We
   may need to reference the locations of other values in order to describe the
   location of some particular value. This is done by using the "call"
   functionality of DWARF location descriptions. (DWARF-4 specification section
   2.5.1.5, page 24.) This avoids any need to transitively resolve phantom lets
   (to constants, symbols or non-phantom variables) in the compiler. *) match
   proto_dies_for_variable var ~proto_dies_for_vars with | None -> None | Some {
   value_die_lvalue; _ } -> let location = SLDL.Lvalue.location_from_another_die
   ~die_label:value_die_lvalue
   ~compilation_unit_header_label:(DS.compilation_unit_header_label state) in
   Some location

   let die_location_of_variable_rvalue state var ~proto_dies_for_vars = match
   proto_dies_for_variable var ~proto_dies_for_vars with | None -> None | Some {
   value_die_rvalue; _ } -> DS.set_rvalue_dies_required_for state (V.Set.add var
   (DS.rvalue_dies_required_for state)); let location =
   SLDL.Rvalue.location_from_another_die ~die_label:value_die_rvalue
   ~compilation_unit_header_label:(DS.compilation_unit_header_label state) in
   Some location *)

type location_description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let reg_location_description reg ~offset_from_cfa_in_bytes ~need_rvalue :
    location_description option =
  match
    Dwarf_reg_locations.reg_location_description reg ~offset_from_cfa_in_bytes
      ~need_rvalue
  with
  | None -> None
  | Some simple_loc_desc -> Some (Simple simple_loc_desc)

let single_location_description state ~parent ~subrange ~proto_dies_for_vars
    ~need_rvalue =
  let location_description =
    match ARV.Subrange.info subrange with
    | Non_phantom { reg; offset_from_cfa_in_bytes } ->
      reg_location_description reg ~offset_from_cfa_in_bytes ~need_rvalue
  in
  match location_description with
  | None -> None
  | Some (Simple simple) ->
    Some (Single_location_description.of_simple_location_description simple)
  | Some (Composite composite) ->
    Some
      (Single_location_description.of_composite_location_description composite)

type location_list_entry =
  | Dwarf_4 of Dwarf_4_location_list_entry.t
  | Dwarf_5 of Location_list_entry.t

let location_list_entry state ~subrange single_location_description :
    location_list_entry =
  let start_pos = Asm_label.create_int Text (ARV.Subrange.start_pos subrange) in
  let start_pos_offset = ARV.Subrange.start_pos_offset subrange in
  let end_pos = Asm_label.create_int Text (ARV.Subrange.end_pos subrange) in
  let end_pos_offset = ARV.Subrange.end_pos_offset subrange in
  match Dwarf_version.four (* XXX !Clflags.gdwarf_version *) with
  | Four ->
    let location_list_entry =
      Dwarf_4_location_list_entry.create_location_list_entry
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
        ~first_address_when_in_scope:start_pos
        ~first_address_when_in_scope_offset:(Some start_pos_offset)
        ~first_address_when_not_in_scope:end_pos
        ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
        ~single_location_description
    in
    Dwarf_4 location_list_entry
  | Five ->
    let start_inclusive =
      Address_table.add (DS.address_table state) start_pos
        ~adjustment:start_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let end_exclusive =
      Address_table.add (DS.address_table state) end_pos
        ~adjustment:end_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let loc_desc =
      Counted_location_description.create single_location_description
    in
    let location_list_entry : Location_list_entry.entry =
      (* DWARF-5 spec page 45 line 1. *)
      Startx_endx { start_inclusive; end_exclusive; payload = loc_desc }
    in
    Dwarf_5
      (Location_list_entry.create location_list_entry
         ~start_of_code_symbol:(DS.start_of_code_symbol state))

let dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars
    ~need_rvalue (var : Backend_var.t) ~phantom:_ ~hidden ~ident_for_type ~range
    =
  let range_info = ARV.Range.info range in
  let provenance = ARV.Range_info.provenance range_info in
  let var_is_a_parameter_of_fundecl_itself =
    match ARV.Range_info.is_parameter range_info with
    | Local -> false
    | Parameter _ -> true
  in
  let is_static =
    match provenance with
    | None -> false
    | Some provenance -> false (* XXX V.Provenance.is_static provenance *)
  in
  let phantom_defining_expr = ARV.Range_info.phantom_defining_expr range_info in
  let (parent_proto_die : Proto_die.t), hidden =
    if var_is_a_parameter_of_fundecl_itself
    then
      function_proto_die, hidden
      (* XXX else if is_static then match provenance with | None ->
         DS.compilation_unit_proto_die state, hidden | Some provenance -> let
         module_path = V.Provenance.module_path provenance in
         Dwarf_modules.dwarf state ~module_path, hidden *)
    else
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function. It is important to generate accurate
         lexical block information to avoid large numbers of variables, many of
         which may be out of scope, being visible in the debugger at the same
         time. *)
      match provenance with
      | None ->
        (* Any variable without provenance gets hidden. *)
        function_proto_die, true
      | Some _provenance -> function_proto_die, false
    (* old code: (

       let dbg = Backend_var.Provenance.debuginfo provenance in let block_die =
       Dwarf_lexical_blocks_and_inlined_frames.find_scope_die_from_debuginfo dbg
       ~function_proto_die ~scope_proto_dies in match block_die with | Some
       block_die -> block_die, hidden | None -> (* There are be no instructions
       marked with the block in which [var] was defined. For the moment, just
       hide [var]. *) function_proto_die, true) *)
  in
  let location_attribute_value, location_list_in_debug_loc_table =
    if is_static
    then
      (* The location of a static (toplevel) variable is invariant under changes
         to the program counter. As such a location list is not needed. *)
      (* CR-someday mshinwell: In the future we should work out how to make
         static variables be properly scoped with respect to the interleaving
         function definitions. Then, we would use the actual calculated
         subranges here. This work requires not only changes in the OCaml middle
         end but also a determination as to whether GDB can support the
         necessary scoping. *)
      match phantom_defining_expr with
      | Non_phantom -> [], None (* Should have been caught below. *)
    else
      (* Build a location list that identifies where the value of [var] may be
         found at runtime, indexed by program counter range. The representations
         of location lists (and range lists, used below to describe lexical
         blocks) changed completely between DWARF-4 and DWARF-5. *)
      let dwarf_4_location_list_entries, location_list =
        ARV.Range.fold range
          ~init:([], Location_list.create ())
          ~f:(fun (dwarf_4_location_list_entries, location_list) subrange ->
            let single_location_description =
              single_location_description state
                ~parent:(Some function_proto_die) ~subrange ~proto_dies_for_vars
                ~need_rvalue
            in
            match single_location_description with
            | None -> dwarf_4_location_list_entries, location_list
            | Some single_location_description -> (
              let location_list_entry =
                location_list_entry state ~subrange single_location_description
              in
              match location_list_entry with
              | Dwarf_4 location_list_entry ->
                let dwarf_4_location_list_entries =
                  location_list_entry :: dwarf_4_location_list_entries
                in
                dwarf_4_location_list_entries, location_list
              | Dwarf_5 location_list_entry ->
                let location_list =
                  Location_list.add location_list location_list_entry
                in
                dwarf_4_location_list_entries, location_list))
      in
      match Dwarf_version.four with
      (* XXX !Clflags.gdwarf_version *)
      | Four ->
        let location_list_entries = dwarf_4_location_list_entries in
        let location_list =
          Dwarf_4_location_list.create ~location_list_entries
        in
        ( [Debug_loc_table.attribute_to_reference_location_list location_list],
          Some location_list )
      | Five ->
        let location_list_index =
          Location_list_table.add (DS.location_list_table state) location_list
        in
        [DAH.create_location location_list_index], None
  in
  let is_parameter =
    ARV.Range_info.is_parameter range_info
    (* XXX let is_parameter_from_provenance = match provenance with | None ->
       Is_parameter.local | Some provenance ->
       Backend_var.Provenance.is_parameter provenance in (* The two inputs here
       correspond to: 1. The normal case of parameters of function declarations,
       which are identified in [Selectgen]. 2. Parameters of inlined functions,
       which have to be tagged much earlier, on [let]-bindings when inlining is
       performed. *) Is_parameter.join (ARV.Range_info.is_parameter range_info)
       is_parameter_from_provenance *)
  in
  let type_and_name_attributes =
    match type_die_reference_for_var var ~proto_dies_for_vars with
    | None -> []
    | Some reference ->
      let name_is_unique, position_is_unique_given_name_is_unique =
        true, true (* CR mshinwell: to improve in future *)
      in
      let name_for_var =
        if name_is_unique
        then Backend_var.name_for_debugger var
        else if position_is_unique_given_name_is_unique
        then
          match provenance with
          | None -> Backend_var.unique_name_for_debugger var
          | Some _provenance -> Backend_var.unique_name_for_debugger var
          (* XXX let dbg = Backend_var.Provenance.debuginfo provenance in match
             Debuginfo.position dbg with | None ->
             Backend_var.unique_name_for_debugger var | Some position ->
             Format.asprintf "%s[%a]" (Backend_var.name_for_debugger var)
             Debuginfo.Code_range.print_compact_without_dirname position *)
        else Backend_var.unique_name_for_debugger var
      in
      (* CR-someday mshinwell: This should be tidied up. It's only correct by
         virtue of the fact we do the closure-env ones second below. *)
      (* CR mshinwell: re-check this CR-someday *)
      let type_attribute =
        if not need_rvalue
        then
          construct_type_of_value_description state
            ~parent:(Some (DS.compilation_unit_proto_die state))
            ident_for_type is_parameter ~proto_dies_for_vars ~reference;
        [DAH.create_type_from_reference ~proto_die_reference:reference]
      in
      let name_attribute =
        if hidden || need_rvalue then [] else [DAH.create_name name_for_var]
      in
      name_attribute @ type_attribute
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index ->
      (* The lvalue DIE is the "normal" one for variables and parameters; it is
         the one that is marked with a name, for example. To avoid erroneous
         display of, or confusion around, rvalue DIEs we always mark them as
         variables not parameters. *)
      if need_rvalue then Variable else Formal_parameter
    | Local -> Variable
  in
  let reference =
    match proto_dies_for_variable var ~proto_dies_for_vars with
    | None -> None
    | Some proto_dies ->
      if need_rvalue
      then Some proto_dies.value_die_rvalue
      else Some proto_dies.value_die_lvalue
  in
  let sort_priority =
    match is_parameter with
    | Local -> None
    | Parameter { index } ->
      (* Ensure that parameters appear in the correct order in the debugger. *)
      if need_rvalue then None else Some index
  in
  Proto_die.create_ignore ?reference ?sort_priority
    ?location_list_in_debug_loc_table ~parent:(Some parent_proto_die) ~tag
    ~attribute_values:(type_and_name_attributes @ location_attribute_value)
    ()

(* This function covers local variables, parameters, variables in closures and
   other "fun_var"s in the current mutually-recursive set. (The last two cases
   are handled by the explicit addition of phantom lets way back in
   [Flambda_to_clambda].) Phantom variables are also covered. *)
let iterate_over_variable_like_things state ~available_ranges_vars ~rvalues_only
    ~f =
  ARV.iter available_ranges_vars ~f:(fun var range ->
      Format.eprintf "processing range for %a\n%!" Ident.print var;
      let should_process =
        not rvalues_only
        (* XXX || V.Set.mem var (DS.rvalue_dies_required_for state) *)
      in
      if should_process
      then
        let range_info = ARV.Range.info range in
        let provenance = ARV.Range_info.provenance range_info in
        let phantom : is_variable_phantom =
          match ARV.Range_info.phantom_defining_expr range_info with
          | Non_phantom ->
            (match provenance with
            | None -> ()
            | Some provenance ->
              if false (* XXX V.Provenance.is_static provenance *)
              then
                Misc.fatal_errorf
                  "Variable %a marked as static, but it is not bound by a \
                   phantom let"
                  V.print var);
            Non_phantom
        in
        (* There are two variables in play here: 1. [var] is the "real" variable
           that is used for obtaining a value at runtime in the debugger. 2.
           [ident_for_type] is the corresponding identifier with the stamp as in
           the typed tree together with its original compilation unit. This is
           the one used for lookup in .cmt files to retrieve a type. We cannot
           conflate these since the multiple [vars] that might be associated
           with a given [ident_for_type] (due to inlining) may not all have the
           same value. *)
        (* CR-someday mshinwell: Introduce some flag on Backend_var.t to mark
           identifiers that were generated internally (or vice-versa)? We should
           probably also hide certain internally-generated identifiers that
           appear in .cmt files but did not come from the source code. *)
        let hidden = Backend_var.is_internal var in
        let ident_for_type =
          Some (Compilation_unit.get_current_exn (), var)
          (* XXX match provenance with | None -> (* In this case the variable
             won't be given a name in the DWARF, so as not to appear in the
             debugger; but we still need to emit a DIE for it, as it may be
             referenced as part of some chain of phantom lets. *) let
             in_startup_file = (* The startup file is generated from Cmm code
             and is therefore not expected to link back to any .cmt file via
             identifier names and stamps. *) false (* XXX Compilation_unit.equal
             (Compilation_unit.get_current_exn ()) Compilation_unit.startup *)
             in if (not hidden) && not in_startup_file then Misc.fatal_errorf
             "Variable %a is not hidden, but has no provenance\n%!"
             Backend_var.print var; None | Some provenance -> Some
             (Compilation_unit.get_current_exn (), var) (* XXX to be replaced by
             a new approach based on "uid"s *) (*
             Backend_var.Provenance.ident_for_type provenance*) *)
        in
        f var ~phantom ~hidden ~ident_for_type ~range)

let dwarf state ~function_proto_die available_ranges_vars =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  Format.eprintf "Dwarf_variables_and_parameters\n%!";
  iterate_over_variable_like_things state ~available_ranges_vars
    ~rvalues_only:false
    ~f:(fun var ~phantom ~hidden:_ ~ident_for_type:_ ~range:_ ->
      let value_die_lvalue = Proto_die.create_reference () in
      let value_die_rvalue = Proto_die.create_reference () in
      let type_die = Proto_die.create_reference () in
      assert (not (Backend_var.Tbl.mem proto_dies_for_vars var));
      Backend_var.Tbl.add proto_dies_for_vars var
        { is_variable_phantom = phantom;
          value_die_lvalue;
          value_die_rvalue;
          type_die
        });
  (* XXX DS.set_rvalue_dies_required_for state V.Set.empty; *)
  (* CR-someday mshinwell: Consider changing [need_rvalue] to use a variant type
     "lvalue or rvalue". *)
  iterate_over_variable_like_things state ~available_ranges_vars
    ~rvalues_only:false
    ~f:
      (dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars
         ~need_rvalue:false);
  iterate_over_variable_like_things state ~available_ranges_vars
    ~rvalues_only:true
    ~f:
      (dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars
         ~need_rvalue:true)
