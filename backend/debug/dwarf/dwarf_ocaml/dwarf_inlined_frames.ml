(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2024 Jane Street Group LLC                           *)
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
  let start_pos_offset = IF.Subrange.start_pos_offset subrange in
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
        let start_pos_offset = IF.Subrange.start_pos_offset subrange in
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

let die_for_inlined_frame state ~compilation_unit_proto_die ~parent
    range_list_attributes block =
  let abstract_instance_symbol =
    Dwarf_abstract_instances.find state ~compilation_unit_proto_die block
  in
  (* Note that with Flambda, this DIE may not be in the scope of the referenced
     abstract instance DIE, as inline expansions may be made out of the scope of
     the function declaration. *)
  let abstract_instance =
    match abstract_instance_symbol with
    | Ok abstract_instance_symbol ->
      [DAH.create_abstract_origin ~die_symbol:abstract_instance_symbol]
    | External_unit { demangled_name; fun_symbol } ->
      (* For references to DIEs in other units, we reconstitute as many of their
         attributes as we can and put them directly into the DIE for the inlined
         frame, making use of DWARF-5 spec page 85, line 30 onwards. This won't
         provide parameter information for the functions concerned, but will do
         for now, until we sort out how to properly reference DIEs across units
         (in a way which will also work on macOS). In particular it should
         otherwise suffice for backtraces. *)
      [ DAH.create_name (Asm_symbol.encode fun_symbol);
        DAH.create_linkage_name ~linkage_name:demangled_name;
        DAH.create_external ~is_visible_externally:true ]
  in
  let block : Debuginfo.item = List.hd (Debuginfo.to_items block) in
  Proto_die.create ~parent:(Some parent) ~tag:Inlined_subroutine
    ~attribute_values:
      (abstract_instance @ range_list_attributes
      @ [ DAH.create_call_file (Dwarf_state.get_file_num state block.dinfo_file);
          DAH.create_call_line block.dinfo_line;
          DAH.create_call_column block.dinfo_char_start ])
    ()

let create_range_list_attributes_and_summarise state fundecl range all_summaries
    =
  match create_range_list_and_summarise state fundecl range with
  | None -> [], all_summaries
  | Some (Contiguous { start_pos; start_pos_offset; end_pos; end_pos_offset })
    ->
    (* Save space by avoiding the emission of a range list. *)
    let start_pos_offset = Targetint.of_int start_pos_offset in
    let end_pos_offset = Targetint.of_int end_pos_offset in
    let low_pc =
      DAH.create_low_pc_with_offset start_pos ~offset_in_bytes:start_pos_offset
    in
    let high_pc =
      DAH.create_high_pc_offset ~low_pc:start_pos
        ~low_pc_offset_in_bytes:start_pos_offset ~high_pc:end_pos
        ~high_pc_offset_in_bytes:end_pos_offset
    in
    [low_pc; high_pc], all_summaries
  | Some (Discontiguous (dwarf_4_range_list_entries, _range_list, summary)) -> (
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
            Debug_ranges_table.insert (DS.debug_ranges_table state) ~range_list
          in
          [range_list_attribute]
        | Five -> Misc.fatal_error "not yet implemented"
        (* let range_list_index = Range_list_table.add (DS.range_list_table
           state) range_list in DAH.create_ranges range_list_index *)
      in
      let all_summaries =
        All_summaries.Map.add summary range_list_attributes all_summaries
      in
      range_list_attributes, all_summaries
    | range_list_attributes -> range_list_attributes, all_summaries)

let rec create_up_to_root fundecl state ~compilation_unit_proto_die
    ~(prefix : Debuginfo.item list) ~(blocks_outermost_first : Debuginfo.t)
    scope_proto_dies all_summaries ~parent_die lexical_block_ranges =
  Format.eprintf ">> create_up_to_root: %a || %a\n%!" Debuginfo.print_compact
    (Debuginfo.of_items prefix)
    Debuginfo.print_compact blocks_outermost_first;
  match Debuginfo.to_items blocks_outermost_first with
  | [] ->
    Format.eprintf "Deepest inlined frame reached\n%!";
    (* We have now gone past the deepest inlined frame. *)
    scope_proto_dies, all_summaries
  | block_item :: deeper_blocks ->
    let block = Debuginfo.of_items [block_item] in
    Format.eprintf "...the current block is %a\n%!" Debuginfo.print_compact
      block;
    let inlined_subroutine_die, scope_proto_dies, all_summaries =
      match K.Map.find block scope_proto_dies with
      | existing_die ->
        Format.eprintf "block already has a proto DIE (ref %a)\n%!"
          Asm_label.print
          (Proto_die.reference existing_die);
        existing_die, scope_proto_dies, all_summaries
      | exception Not_found ->
        Format.eprintf "New DIE will be needed, parent DIE ref is %a\n%!"
          Asm_label.print
          (Proto_die.reference parent_die);
        let range_key =
          Debuginfo.of_items (prefix @ Debuginfo.to_items blocks_outermost_first)
        in
        let () =
          Format.eprintf
            "finding ranges for key (current block + all parents): %a\n%!"
            K.print range_key
        in
        let range = IF.find lexical_block_ranges range_key in
        let range_list_attributes, all_summaries =
          create_range_list_attributes_and_summarise state fundecl range
            all_summaries
        in
        let inlined_subroutine_die =
          die_for_inlined_frame state ~compilation_unit_proto_die
            ~parent:parent_die range_list_attributes block
        in
        Format.eprintf
          "Our DIE ref (DW_TAG_inlined_subroutine) for %a is %a\n%!"
          Debuginfo.print_compact block Asm_label.print
          (Proto_die.reference inlined_subroutine_die);
        let scope_proto_dies =
          K.Map.add blocks_outermost_first inlined_subroutine_die
            scope_proto_dies
        in
        inlined_subroutine_die, scope_proto_dies, all_summaries
    in
    create_up_to_root fundecl state ~compilation_unit_proto_die
      ~prefix:(prefix @ [block_item])
      ~blocks_outermost_first:(Debuginfo.of_items deeper_blocks)
      scope_proto_dies all_summaries ~parent_die:inlined_subroutine_die
      lexical_block_ranges

let dwarf state (fundecl : L.fundecl) lexical_block_ranges ~function_proto_die =
  Format.eprintf "\n\nDwarf_inlined_frames.dwarf: function proto DIE is %a\n%!"
    Asm_label.print
    (Proto_die.reference function_proto_die);
  let all_blocks = IF.all_indexes lexical_block_ranges in
  let scope_proto_dies, _all_summaries =
    IF.Inlined_frames.Index.Set.fold
      (fun (block_with_parents : Debuginfo.t) (scope_proto_dies, all_summaries) ->
        Format.eprintf "--------------------------------------------------\n";
        Format.eprintf "START: %a\n%!" Debuginfo.print_compact
          block_with_parents;
        (* The head of [block_with_parents] always corresponds to [fundecl] and
           thus will be associated with [function_proto_die]. As such we don't
           need to create any DW_TAG_inlined_subroutine DIEs for it. *)
        let first_item, parents_outermost_first =
          (* "Outermost" = less deep inlining *)
          let block_with_parents = Debuginfo.to_items block_with_parents in
          match block_with_parents with
          | [] ->
            Misc.fatal_errorf "Empty debuginfo in function %s" fundecl.fun_name
          | first_item :: parents -> first_item, Debuginfo.of_items parents
        in
        Format.eprintf "Having removed fundecl item: %a\n%!"
          Debuginfo.print_compact parents_outermost_first;
        let compilation_unit_proto_die = DS.compilation_unit_proto_die state in
        let scope_proto_dies, all_summaries =
          (* XXX hack for error in printf.ml in Base *)
          try
            create_up_to_root fundecl state ~compilation_unit_proto_die
              ~prefix:[first_item]
              ~blocks_outermost_first:parents_outermost_first scope_proto_dies
              all_summaries ~parent_die:function_proto_die lexical_block_ranges
          with Not_found -> scope_proto_dies, all_summaries
        in
        scope_proto_dies, all_summaries)
      all_blocks
      (K.Map.empty, All_summaries.Map.empty)
  in
  scope_proto_dies
