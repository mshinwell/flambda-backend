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
module IF = Inlined_frame_ranges
module K = IF.Inlined_frames.Key
module L = Linear
module String = Misc.Stdlib.String

let _find_scope_die_from_debuginfo (dbg : Debuginfo.t) ~function_proto_die:_
    ~scope_proto_dies =
  match Debuginfo.to_items dbg with
  | [] -> None
  | { dinfo_uid = None; _ } :: _ -> None
  | { dinfo_uid = Some _; _ } :: _ -> (
    match K.Map.find dbg scope_proto_dies with
    | exception Not_found -> None
    | proto_die -> Some proto_die)

let magic_offset () =
  (* CR mshinwell: wtf? *)
  match Target_system.architecture () with
  | X86_64 -> 8
  | AArch64 -> 4
  | ARM | IA32 | POWER | Z | Riscv ->
    Misc.fatal_error "Architecture not supported"

type ranges =
  | Contiguous of
      { start_pos : Asm_label.t;
        start_pos_offset : int;
        end_pos : Asm_label.t;
        end_pos_offset : int
      }
  | Discontiguous of
      Dwarf_4_range_list_entry.t list * Range_list.t * Address_index.Pair.Set.t

let create_contiguous_range_list_and_summarise _state (_fundecl : L.fundecl)
    subrange =
  let start_pos = IF.Subrange.start_pos subrange in
  let start_pos_offset =
    IF.Subrange.start_pos_offset subrange - magic_offset ()
  in
  let end_pos = IF.Subrange.end_pos subrange in
  let end_pos_offset = IF.Subrange.end_pos_offset subrange in
  Contiguous
    { start_pos = Asm_label.create_int Text start_pos;
      start_pos_offset;
      end_pos = Asm_label.create_int Text end_pos;
      end_pos_offset
    }

let create_discontiguous_range_list_and_summarise state (_fundecl : L.fundecl)
    range =
  let dwarf_4_range_list_entries, range_list, summary =
    IF.Range.fold range
      ~init:([], Range_list.create (), Address_index.Pair.Set.empty)
      ~f:(fun (dwarf_4_range_list_entries, range_list, summary) subrange ->
        let start_pos = IF.Subrange.start_pos subrange in
        let start_pos_offset =
          IF.Subrange.start_pos_offset subrange - magic_offset ()
        in
        let end_pos = IF.Subrange.end_pos subrange in
        let end_pos_offset = IF.Subrange.end_pos_offset subrange in
        let start_inclusive =
          Address_table.add (DS.address_table state)
            (Asm_label.create_int Text start_pos)
            ~adjustment:start_pos_offset
            ~start_of_code_symbol:(DS.start_of_code_symbol state)
        in
        let end_exclusive =
          Address_table.add (DS.address_table state)
            (Asm_label.create_int Text end_pos)
            ~adjustment:end_pos_offset
            ~start_of_code_symbol:(DS.start_of_code_symbol state)
        in
        let range_list_entry : Range_list_entry.entry =
          (* DWARF-5 spec page 54 line 1. *)
          Startx_endx { start_inclusive; end_exclusive; payload = () }
        in
        let range_list_entry =
          Range_list_entry.create range_list_entry
            ~start_of_code_symbol:(DS.start_of_code_symbol state)
        in
        (* We still use the [Range_list] when emitting DWARF-4 (even though it
           is a DWARF-5 structure) for the purposes of de-duplicating ranges. *)
        let range_list = Range_list.add range_list range_list_entry in
        let summary =
          Address_index.Pair.Set.add (start_inclusive, end_exclusive) summary
        in
        let dwarf_4_range_list_entries =
          match !Dwarf_flags.gdwarf_version with
          | Four ->
            let range_list_entry =
              Dwarf_4_range_list_entry.create_range_list_entry
                ~start_of_code_symbol:(DS.start_of_code_symbol state)
                ~first_address_when_in_scope:
                  (Asm_label.create_int Text start_pos)
                ~first_address_when_not_in_scope:
                  (Asm_label.create_int Text end_pos)
                ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
            in
            Format.eprintf "range_list_entry: start=%d end=%d+%d\n%!" start_pos
              end_pos end_pos_offset;
            range_list_entry :: dwarf_4_range_list_entries
          | Five -> dwarf_4_range_list_entries
        in
        dwarf_4_range_list_entries, range_list, summary)
  in
  Discontiguous (dwarf_4_range_list_entries, range_list, summary)

let create_range_list_and_summarise state _fundecl range =
  match IF.Range.get_singleton range with
  | No_ranges -> None
  | One_subrange subrange ->
    Some (create_contiguous_range_list_and_summarise state _fundecl subrange)
  | More_than_one_subrange ->
    Some (create_discontiguous_range_list_and_summarise state _fundecl range)

(* "Summaries", sets of pairs of the starting and ending points of ranges, are
   used to dedup entries in the range list table. We do this for range lists but
   not yet for location lists since deduping entries in the latter would involve
   comparing DWARF location descriptions. *)
module All_summaries = Identifiable.Make (struct
  include Address_index.Pair.Set

  let hash t = Hashtbl.hash (elements t)
end)

(* XXX fundecl -> fundecl_being_inlined *)
let die_for_inlined_frame state parent fundecl range range_list_attributes block
    =
  let _, abstract_instance_symbol =
    Dwarf_abstract_instances.find (* find_maybe_in_another_unit_or_add *) state
      ~function_proto_die:parent block
  in
  let abstract_instance_symbol = Some abstract_instance_symbol in
  let _entry_pc =
    (* CR-someday mshinwell: The "entry PC" is supposed to be the address of the
       "temporally first" instruction of the inlined function. We assume here
       that we don't do transformations which might cause the first instruction
       of the inlined function to not be the one at the lowest address amongst
       all instructions of the inlined function. If this assumption is wrong the
       most likely outcome seems to be breakpoints being slightly in the wrong
       place, although still in the correct function. Making this completely
       accurate will necessitate more tracking of instruction ordering from
       earlier in the compiler. *)
    match IF.Range.estimate_lowest_address range with
    | None -> []
    | Some (lowest_address, offset) ->
      (* XXX this addition is wrong *)
      [DAH.create_entry_pc (Asm_label.create_int Text (lowest_address + offset))]
  in
  (* Note that with Flambda, this DIE may not be in the scope of the referenced
     abstract instance DIE, as inline expansions may be made out of the scope of
     the function declaration. *)
  let abstract_instance =
    match abstract_instance_symbol with
    | None ->
      (* If the abstract instance DIE cannot be referenced, reconstitute as many
         of its attributes as we can and put them directly into the DIE for the
         inlined frame, making use of DWARF-5 spec page 85, line 30 onwards. *)
      (* XXX unclear this is correct - look where [fundecl] comes from below *)
      Dwarf_abstract_instances.attributes fundecl.L.fun_name
    | Some abstract_instance_symbol ->
      (* This appears to be the source of a bogus error from "dwarfdump
         --verify" on macOS:

         error: <address>: DIE has tag Unknown DW_TAG constant: 0x4109 has
         DW_AT_abstract_origin that points to DIE <address> with incompatible
         tag TAG_subprogram

         The error seems to be bogus because:

         (a) 0x4109 is DW_TAG_GNU_call_site which has no DW_AT_abstract_origin
         attribute (it does in a child, but not in itself); and

         (b) When DW_AT_abstract_origin is used to reference an abstract
         instance then it is expected that the tags of the referring and
         referred-to DIEs differ. DWARF-5 spec page 85, lines 4--8.

         Since this complaint does not appear during a normal build process we
         do not attempt to work around it. *)
      [DAH.create_abstract_origin ~die_symbol:abstract_instance_symbol]
  in
  let block : Debuginfo.item = List.hd (Debuginfo.to_items block) in
  (* let code_range = Debuginfo.Call_site.position call_site in let file_name =
     Debuginfo.Code_range.file code_range in *)
  Proto_die.create ~parent:(Some parent) ~tag:Inlined_subroutine
    ~attribute_values:
      (abstract_instance @ range_list_attributes
      @ [ DAH.create_call_file (Dwarf_state.get_file_num state block.dinfo_file);
          DAH.create_call_line block.dinfo_line;
          DAH.create_call_column block.dinfo_char_start ])
    ()

let rec create_up_to_root fundecl state block (block_with_parents : Debuginfo.t)
    scope_proto_dies all_summaries function_proto_die lexical_block_ranges =
  Format.eprintf "... %a\n%!" Debuginfo.print_compact block;
  match K.Map.find block_with_parents scope_proto_dies with
  | proto_die ->
    Format.eprintf "block already has a proto DIE\n%!";
    proto_die, scope_proto_dies, all_summaries
  | exception Not_found -> (
    let parent =
      Option.map Debuginfo.of_items
        (K.parent (Debuginfo.to_items block_with_parents))
    in
    match parent with
    | None ->
      Format.eprintf "no parent\n%!";
      function_proto_die, scope_proto_dies, all_summaries
    | Some parent ->
      let parent, scope_proto_dies, all_summaries =
        Format.eprintf "parent is: %a\n%!" K.print parent;
        create_up_to_root fundecl state block parent scope_proto_dies
          all_summaries function_proto_die lexical_block_ranges
      in
      let () =
        Format.eprintf "finding ranges for block: %a\n%!" K.print
          block_with_parents
      in
      let range = IF.find lexical_block_ranges block_with_parents in
      let range_list_attributes, all_summaries =
        match create_range_list_and_summarise state fundecl range with
        | None -> [], all_summaries
        | Some
            (Contiguous
              { start_pos; start_pos_offset; end_pos; end_pos_offset }) ->
          let start_pos_offset = Targetint.of_int start_pos_offset in
          let end_pos_offset = Targetint.of_int end_pos_offset in
          let low_pc =
            DAH.create_low_pc_with_offset start_pos
              ~offset_in_bytes:start_pos_offset
          in
          let high_pc =
            DAH.create_high_pc_offset ~low_pc:start_pos
              ~low_pc_offset_in_bytes:start_pos_offset ~high_pc:end_pos
              ~high_pc_offset_in_bytes:end_pos_offset
          in
          [low_pc; high_pc], all_summaries
        | Some
            (Discontiguous (dwarf_4_range_list_entries, _range_list, summary))
          -> (
          match All_summaries.Map.find summary all_summaries with
          | exception Not_found ->
            let range_list_attributes =
              match !Dwarf_flags.gdwarf_version with
              | Four ->
                let range_list =
                  Dwarf_4_range_list.create
                    ~range_list_entries:dwarf_4_range_list_entries
                in
                let range_list_attribute =
                  Debug_ranges_table.insert
                    (DS.debug_ranges_table state)
                    ~range_list
                in
                [range_list_attribute]
              | Five -> Misc.fatal_error "not yet implemented"
              (* let range_list_index = Range_list_table.add
                 (DS.range_list_table state) range_list in DAH.create_ranges
                 range_list_index *)
            in
            let all_summaries =
              All_summaries.Map.add summary range_list_attributes all_summaries
            in
            range_list_attributes, all_summaries
          | range_list_attributes -> range_list_attributes, all_summaries)
      in
      let proto_die =
        die_for_inlined_frame state parent fundecl range range_list_attributes
          block
      in
      let scope_proto_dies = K.Map.add block proto_die scope_proto_dies in
      proto_die, scope_proto_dies, all_summaries)

let dwarf state (fundecl : L.fundecl) lexical_block_ranges ~function_proto_die =
  let all_blocks = IF.all_indexes lexical_block_ranges in
  let scope_proto_dies, _all_summaries =
    IF.Inlined_frames.Index.Set.fold
      (fun (block_with_parents : Debuginfo.t) (scope_proto_dies, all_summaries) ->
        let block =
          let block_with_parents = Debuginfo.to_items block_with_parents in
          match List.rev block_with_parents with
          | [] ->
            Misc.fatal_errorf "Empty debuginfo in function %s" fundecl.fun_name
          | block :: _ -> Debuginfo.of_items [block]
        in
        let parent =
          Option.map Debuginfo.of_items
            (K.parent (Debuginfo.to_items block_with_parents))
        in
        Format.eprintf ">> %a (parents are: %a)\n%!" Debuginfo.print_compact
          block
          (Misc.Stdlib.Option.print Debuginfo.print_compact)
          parent;
        let _proto_die, scope_proto_dies, all_summaries =
          create_up_to_root fundecl state block block_with_parents
            scope_proto_dies all_summaries function_proto_die
            lexical_block_ranges
        in
        scope_proto_dies, all_summaries)
      all_blocks
      (K.Map.empty, All_summaries.Map.empty)
  in
  scope_proto_dies
