(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Asm_section = Asm_targets.Asm_section
module D = Asm_targets.Asm_directives
module Const = D.Directive.Constant

let extract_label_this_offset = function[@warning "-4"]
  | Const.Add
      (Const.Sub (Const.Named_thing name, Const.This), Const.Signed_int offset)
    ->
    Some (name, offset)
  | Const.Sub (Const.Named_thing name, Const.This) -> Some (name, 0L)
  | _ -> None

let eval_constant state ~all_sections const =
  (* For same-section relative expressions like (Label - This), the offset
     within the section is all that matters. Cross-section references are
     detected and handled via relocations before this function is called. *)
  let this () = Int64.of_int (Section_state.offset_in_bytes state) in
  let lookup name =
    (* When generating PIC code (dlcode=true), global symbols should not be
       resolved as they may be interposed at runtime. We must emit zeros and let
       the linker handle them via relocations. Note: global symbols appear both
       in the symbol table (via Global directive) AND in the label table (via
       the New_label for the : definition). So we check if the name is a
       declared global symbol first. *)
    let is_global_symbol =
      Option.is_some (Section_state.find_symbol_offset_in_bytes state name)
    in
    if !Clflags.dlcode && is_global_symbol
    then None
    else
      (* First try current section, then fall back to global lookup. *)
      match Section_state.find_label_offset_in_bytes state name with
      | Some offset -> Some (Int64.of_int offset)
      | None -> (
        match Section_state.find_symbol_offset_in_bytes state name with
        | Some offset -> Some (Int64.of_int offset)
        | None -> (
          match All_section_states.find_in_any_section all_sections name with
          | Some (offset, _section) -> Some (Int64.of_int offset)
          | None -> None))
  in
  D.Directive.Constant.eval ~this ~lookup const

(* Handle cross-section (Label - This) + offset pattern. This occurs in
   frametable entries where a DATA section location references a TEXT section
   label (return address). We emit a PREL32_PAIR relocation using existing
   global symbols (matching assembler behavior). *)
let try_cross_section_relative_reference state ~all_sections ~current_section c
    =
  match extract_label_this_offset c with
  | None -> None
  | Some (label_name, offset_upper) -> (
    (* First check if label is in current section *)
    match Section_state.find_label_offset_in_bytes state label_name with
    | Some _ -> None (* Same section, use normal eval *)
    | None -> (
      (* Try cross-section lookup *)
      match All_section_states.find_in_any_section all_sections label_name with
      | None -> None (* Not found at all *)
      | Some (_, label_section) -> (
        if Asm_section.equal label_section current_section
        then None (* Same section after all *)
        else if not (Asm_section.equal current_section Asm_section.Data)
        then
          Misc.fatal_errorf
            "Cross-section (Label - This) from non-DATA section %s to %s not \
             supported"
            (Asm_section.to_string current_section)
            (Asm_section.to_string label_section)
        else if not (Asm_section.equal label_section Asm_section.Text)
        then
          Misc.fatal_errorf
            "Cross-section (Label - This) from DATA to non-TEXT section %s not \
             supported"
            (Asm_section.to_string label_section)
        else
          (* Cross-section: TEXT label referenced from DATA. The linker
             computes: plus_sym - minus_sym + addend So: addend = (target -
             plus_sym) - (current - minus_sym) *)
          let current_pos = Section_state.offset_in_bytes state in
          (* Find nearest symbol in DATA for SUBTRACTOR *)
          match Section_state.find_nearest_symbol_before state current_pos with
          | None ->
            Misc.fatal_error
              "No symbol in DATA section for cross-section relocation"
          | Some (minus_symbol, minus_sym_offset) -> (
            (* Find nearest symbol in TEXT for UNSIGNED *)
            let text_state =
              All_section_states.find_exn all_sections Asm_section.Text
            in
            (* Get target label offset in TEXT *)
            match
              Section_state.find_label_offset_in_bytes text_state label_name
            with
            | None ->
              Misc.fatal_errorf "Label %s not found in TEXT section" label_name
            | Some target_offset -> (
              match
                Section_state.find_nearest_symbol_before text_state
                  target_offset
              with
              | None ->
                Misc.fatal_error
                  "No symbol in TEXT section for cross-section relocation"
              | Some (plus_symbol, plus_sym_offset) ->
                let addend =
                  Int64.add offset_upper
                    (Int64.sub
                       (Int64.of_int (target_offset - plus_sym_offset))
                       (Int64.of_int (current_pos - minus_sym_offset)))
                in
                Section_state.add_relocation_at_current_offset state
                  ~symbol_name:plus_symbol
                  ~reloc_kind:
                    (Relocation.Kind.R_AARCH64_PREL32_PAIR
                       { plus_symbol; minus_symbol });
                Some addend)))))

(* Handle absolute cross-section symbol reference. For .8byte symbol where
   symbol is in a different section, we must emit a relocation. *)
let try_cross_section_absolute_reference state ~all_sections ~current_section
    ~width_bytes c =
  match[@warning "-4"] c with
  | Const.Named_thing name when width_bytes = 8 -> (
    (* Check if symbol is in a different section *)
    match Section_state.find_label_offset_in_bytes state name with
    | Some _ -> None (* Same section, can resolve *)
    | None -> (
      match Section_state.find_symbol_offset_in_bytes state name with
      | Some _ -> None (* Same section symbol *)
      | None -> (
        (* Try cross-section lookup *)
        match All_section_states.find_in_any_section all_sections name with
        | None -> None (* Not found, will fall through to relocation *)
        | Some (_, sym_section) ->
          if Asm_section.equal sym_section current_section
          then None (* Same section *)
          else (
            (* Cross-section absolute reference - needs relocation *)
            Section_state.add_relocation_at_current_offset state
              ~symbol_name:name
              ~reloc_kind:(Relocation.Kind.R_AARCH64_ABS64 name);
            Some 0L (* Emit zero, relocation will patch *)))))
  | _ -> None

let rec extract_symbol_name = function
  | Const.Named_thing name -> Some name
  | Const.Add (a, _) -> extract_symbol_name a
  | Const.Sub (a, _) -> extract_symbol_name a
  | Const.Signed_int _ | Const.Unsigned_int _ | Const.This -> None

(* Handle unresolved symbol reference by emitting zeros and recording a
   relocation for the linker to patch. *)
let emit_unresolved_symbol_relocation state ~width_bytes c =
  let buf = Section_state.buffer state in
  (match extract_symbol_name c with
  | Some symbol_name when width_bytes = 8 ->
    Section_state.add_relocation_at_current_offset state ~symbol_name
      ~reloc_kind:(Relocation.Kind.R_AARCH64_ABS64 symbol_name)
  | Some symbol_name ->
    Misc.fatal_errorf
      "Unresolved %d-byte reference to symbol %s (only 8-byte relocations \
       supported)"
      width_bytes symbol_name
  | None ->
    Misc.fatal_errorf
      "Unresolved %d-byte constant expression (only 8-byte relocations \
       supported)"
      width_bytes);
  for _ = 1 to width_bytes do
    Buffer.add_char buf '\x00'
  done

(* Emit a constant value, handling cross-section references and relocations. *)
let emit_constant state ~all_sections ~current_section constant =
  let buf = Section_state.buffer state in
  let module C = D.Directive.Constant_with_width in
  let c = C.constant constant in
  let width = C.width_in_bytes constant in
  let width_bytes = C.width_in_bytes_int width in
  let value_opt =
    match
      try_cross_section_relative_reference state ~all_sections ~current_section
        c
    with
    | Some addend -> Some addend
    | None -> (
      match
        try_cross_section_absolute_reference state ~all_sections
          ~current_section ~width_bytes c
      with
      | Some v -> Some v
      | None -> eval_constant state ~all_sections c)
  in
  match value_opt with
  | Some value -> D.Directive.emit_int_le buf ~width_bytes value
  | None -> emit_unresolved_symbol_relocation state ~width_bytes c

let emit_directive state ~current_section ~all_sections
    (directive : D.Directive.t) =
  let buf = Section_state.buffer state in
  (* Update current section when we see a Section directive *)
  (match[@warning "-4"] directive with
  | D.Directive.Section { names; _ } -> (
    match Asm_section.of_names names with
    | Some section -> current_section := section
    | None -> ())
  | _ -> ());
  match directive with
  | Bytes { str; _ } -> Buffer.add_string buf str
  | Space { bytes } ->
    for _ = 1 to bytes do
      Buffer.add_char buf '\x00'
    done
  | Align { bytes; fill } -> (
    let offset = Section_state.offset_in_bytes state in
    let remainder = offset mod bytes in
    if remainder <> 0
    then
      let padding = bytes - remainder in
      match fill with
      | D.Nop ->
        (* Emit NOP instructions (4 bytes each) for code alignment *)
        let nop_count = padding / 4 in
        let zero_count = padding mod 4 in
        let nop = Int64.of_int32 (Nop_helpers.encode_nop ()) in
        for _ = 1 to nop_count do
          D.Directive.emit_int_le buf ~width_bytes:4 nop
        done;
        for _ = 1 to zero_count do
          Buffer.add_char buf '\x00'
        done
      | D.Zero ->
        for _ = 1 to padding do
          Buffer.add_char buf '\x00'
        done)
  | Const { constant; _ } ->
    emit_constant state ~all_sections ~current_section:!current_section constant
  | Sleb128 { constant; _ } -> (
    match eval_constant state ~all_sections constant with
    | Some value -> D.Directive.emit_sleb128 buf value
    | None -> Misc.fatal_error "Cannot emit SLEB128 for external symbol")
  | Uleb128 { constant; _ } -> (
    match eval_constant state ~all_sections constant with
    | Some value -> D.Directive.emit_uleb128 buf value
    | None -> Misc.fatal_error "Cannot emit ULEB128 for external symbol")
  (* Directives that don't emit data *)
  | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_endproc | Cfi_offset _
  | Cfi_startproc | Cfi_remember_state | Cfi_restore_state
  | Cfi_def_cfa_register _ | Comment _ | Direct_assignment _ | File _ | Global _
  | Indirect_symbol _ | Loc _ | New_label _ | New_line | Private_extern _
  | Section _ | Size _ | Type _ | Protected _ | Hidden _ | Weak _ | External _
  | Reloc _ ->
    ()
