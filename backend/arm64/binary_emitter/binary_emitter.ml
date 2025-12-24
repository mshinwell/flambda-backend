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

open Arm64_ast.Ast
module Asm_section = Asm_targets.Asm_section
module D = Asm_targets.Asm_directives

(* Re-export sub-modules for the library interface *)
module Relocation = Relocation
module Section_state = Section_state

type instruction_or_directive =
  | Instruction of Instruction.t
  | Directive of D.Directive.t

type t = { mutable enqueued_rev : instruction_or_directive list }

let create () = { enqueued_rev = [] }

let enqueue t insn_or_directive =
  t.enqueued_rev <- insn_or_directive :: t.enqueued_rev

let enqueued t = List.rev t.enqueued_rev

let add_instruction t i = enqueue t (Instruction i)

let add_directive t d = enqueue t (Directive d)

let iter emitter ~state_for_section ~on_insn ~on_directive =
  let current_state = ref (state_for_section Asm_section.Text) in
  List.iter
    (fun insn_or_directive ->
      match insn_or_directive with
      | Instruction i ->
        on_insn !current_state i;
        let offset = Section_state.offset_in_bytes !current_state in
        Section_state.set_offset_in_bytes !current_state (offset + 4)
      | Directive d ->
        (match d with
        | D.Directive.Section { names; _ } -> (
          match Asm_section.of_names names with
          | Some section -> current_state := state_for_section section
          | None ->
            Misc.fatal_errorf "Unknown section: %s" (String.concat ", " names))
        | _ -> ());
        on_directive !current_state d;
        let offset_in_bytes = Section_state.offset_in_bytes !current_state in
        let new_offset =
          D.Directive.increment_offset_in_bytes d ~offset_in_bytes
        in
        Section_state.set_offset_in_bytes !current_state new_offset)
    (enqueued emitter)

(* First pass: compute offsets of local symbol and label definitions *)
let compute_label_offsets emitter ~state_for_section =
  iter emitter ~state_for_section
    ~on_insn:(fun _state _insn -> ())
    ~on_directive:(fun state directive ->
      match directive with
      | New_label (name, _) -> Section_state.define_label state name
      | Global name -> Section_state.define_symbol state name
      (* Directives that don't define labels or symbols *)
      | Align _ | Bytes _ | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _
      | Cfi_endproc | Cfi_offset _ | Cfi_startproc | Cfi_remember_state
      | Cfi_restore_state | Cfi_def_cfa_register _ | Comment _ | Const _
      | Direct_assignment _ | File _ | Indirect_symbol _ | Loc _ | New_line
      | Private_extern _ | Section _ | Size _ | Sleb128 _ | Space _ | Type _
      | Uleb128 _ | Protected _ | Hidden _ | Weak _ | External _ | Reloc _ ->
        ())

(* Second pass: emit machine code and data *)
let emit_code_and_data emitter ~state_for_section ~section_base ~global_lookup
    ~global_lookup_with_section ~section_tbl =
  (* Track current section to get the right base offset *)
  let current_section = ref Asm_section.Text in
  (* For cross-section (Label - This) expressions, we need to emit a relocation
     pair (SUBTRACTOR + UNSIGNED) and store the addend. The linker will compute:
     final_value = plus_symbol_addr - minus_symbol_addr + addend We create
     symbols at the exact positions needed so the addend is just the
     offset_upper from the original expression. *)
  iter emitter ~state_for_section
    ~on_insn:(fun state (Instruction.I { name; operands }) ->
      let encoded = Encode_instruction.encode_instruction state name operands in
      let buf = Section_state.buffer state in
      (* Emit as little-endian 32-bit *)
      Buffer.add_char buf (Char.chr (Int32.to_int encoded land 0xff));
      Buffer.add_char buf
        (Char.chr
           (Int32.to_int (Int32.shift_right_logical encoded 8) land 0xff));
      Buffer.add_char buf
        (Char.chr
           (Int32.to_int (Int32.shift_right_logical encoded 16) land 0xff));
      Buffer.add_char buf
        (Char.chr
           (Int32.to_int (Int32.shift_right_logical encoded 24) land 0xff)))
    ~on_directive:
      (Encode_directive.emit_directive ~current_section ~section_base
         ~global_lookup ~global_lookup_with_section ~section_tbl)

let emit emitter =
  let section_tbl = Asm_section.Tbl.create 10 in
  let state_for_section section =
    match Asm_section.Tbl.find_opt section_tbl section with
    | Some state -> state
    | None ->
      let state = Section_state.create () in
      Asm_section.Tbl.add section_tbl section state;
      state
  in
  compute_label_offsets emitter ~state_for_section;
  (* For same-section relative expressions like (Label - This), the section
     base cancels out, so we use 0 for all sections. Cross-section references
     are handled via relocations (R_AARCH64_PREL32_PAIR for relative refs,
     R_AARCH64_ABS64 for absolute refs) which the linker resolves. *)
  let section_base (_ : Asm_section.t) = 0 in
  (* Global lookup for cross-section references. Returns the section containing
     the symbol if found. For global/external symbols in PIC mode, we return
     None to trigger the relocation path. *)
  let global_lookup_with_section name =
    (* Search all sections for this label *)
    let result = ref None in
    Asm_section.Tbl.iter
      (fun section state ->
        if Option.is_none !result
        then
          match Section_state.find_label_offset_in_bytes state name with
          | Some offset -> result := Some (offset, section)
          | None -> (
            match Section_state.find_symbol_offset_in_bytes state name with
            | Some offset -> result := Some (offset, section)
            | None -> ()))
      section_tbl;
    !result
  in
  (* Global lookup returning offset within the object file. Since section_base
     is 0 for all sections, this is just the offset within the section. For
     same-section expressions this works correctly; cross-section expressions
     are handled via relocations. *)
  let global_lookup name =
    match global_lookup_with_section name with
    | Some (offset, _section) -> Some (Int64.of_int offset)
    | None -> None
  in
  (* Reset offsets for second pass *)
  Asm_section.Tbl.iter
    (fun _section state -> Section_state.set_offset_in_bytes state 0)
    section_tbl;
  emit_code_and_data emitter ~state_for_section ~section_base ~global_lookup
    ~global_lookup_with_section ~section_tbl;
  section_tbl

module For_jit = For_jit
