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
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol

(* TODO: Asm_directives uses strings for labels and symbols. We should change it
   to use Asm_label.t and Asm_symbol.t, and update this module accordingly. *)
type patch_size =
  | P8
  | P16
  | P32
  | P64

(* Helper to extract Q (vector width) and size (element size) from vector
   type *)
let vector_q_size (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int * int =
  match vec with
  | V8B -> 0, 0b00 (* 64-bit, B elements *)
  | V16B -> 1, 0b00 (* 128-bit, B elements *)
  | V4H -> 0, 0b01 (* 64-bit, H elements *)
  | V8H -> 1, 0b01 (* 128-bit, H elements *)
  | V2S -> 0, 0b10 (* 64-bit, S elements *)
  | V4S -> 1, 0b10 (* 128-bit, S elements *)
  | V1D -> 0, 0b11 (* 64-bit, D elements *)
  | V2D -> 1, 0b11 (* 128-bit, D elements *)

(* Helper for FP vector operations - returns Q and the FP precision bit (sz) *)
let vector_q_fp_sz (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int * int
    =
  match vec with
  | V2S -> 0, 0 (* 64-bit, single-precision *)
  | V4S -> 1, 0 (* 128-bit, single-precision *)
  | V2D -> 1, 1 (* 128-bit, double-precision *)
  | V8B | V16B | V4H | V8H | V1D ->
    Misc.fatal_error "FP vector operations only support S and D element types"

let split_21bit_immediate (imm21 : int) : int * int =
  if imm21 < -0x100000 || imm21 > 0xfffff
  then
    Misc.fatal_errorf
      "Immediate value %d (0x%x) out of range for 21-bit signed immediate (max \
       ±0x100000)"
      imm21 imm21;
  let immlo = imm21 land 0b11 in
  let immhi = (imm21 lsr 2) land 0x7ffff in
  immlo, immhi

let encode_six_bit_shift (shift_opt : [`Shift of _ * [`Six]] Operand.t option) =
  match shift_opt with
  | Some (Shift shift) -> (match shift.amount with Six n -> n) / 16
  | None -> 0

(* Decode shift type and amount for add/sub shifted register instructions.
   Returns (shift_type, amount) where shift_type is 00=LSL, 01=LSR, 10=ASR. *)
let decode_shift_kind_int : type a. a Operand.Shift.Kind.t -> int =
 fun kind ->
  match kind with Operand.Shift.Kind.LSL -> 0b00 | LSR -> 0b01 | ASR -> 0b10

let decode_shift_amount_six : type a. a Operand.Imm.t -> int =
 fun amount -> match amount with Six n -> n | _ -> assert false

(* Check if a register is SP (stack pointer) by examining its name *)
let is_sp_reg : type a. a Reg.t -> bool =
 fun r ->
  match r.reg_name with
  | GP GP_reg_name.SP -> true
  | GP GP_reg_name.WSP -> true
  | _ -> false

(* Helper to extract ftype from scalar precision: S=0, D=1 *)
let scalar_ftype (type a) (s : [`Scalar of a] Neon_reg_name.t) : int =
  match s with Scalar S -> 0 | Scalar D -> 1 | _ -> assert false

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
      (Encode_directive.emit_directive ~current_section ~section_base ~global_lookup
         ~global_lookup_with_section ~section_tbl)

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
  (* Compute section sizes (final offset after first pass) and bases. Sections
     are laid out contiguously: TEXT at 0, DATA after TEXT, etc. *)
  let section_sizes = Asm_section.Tbl.create 10 in
  Asm_section.Tbl.iter
    (fun section state ->
      Asm_section.Tbl.add section_sizes section
        (Section_state.offset_in_bytes state))
    section_tbl;
  (* Compute section bases. We use a simple layout: TEXT at 0, DATA after TEXT,
     other sections after DATA. The actual layout doesn't matter for correctness
     as long as it's consistent - we just need relative positions to work
     out. *)
  let text_size =
    Option.value ~default:0
      (Asm_section.Tbl.find_opt section_sizes Asm_section.Text)
  in
  let data_size =
    Option.value ~default:0
      (Asm_section.Tbl.find_opt section_sizes Asm_section.Data)
  in
  let section_base section =
    match section with
    | Asm_section.Text -> 0
    | Asm_section.Data -> text_size
    | _ -> text_size + data_size (* Other sections after data *)
  in
  (* Global lookup for cross-section references. Returns (offset_in_section,
     section, section_base) if found. For local labels (which start with L on
     macOS), we can resolve cross-section references at assembly time because
     the relative positions are fixed within the object file. For
     global/external symbols in PIC mode, we return None to trigger the
     relocation path. *)
  let global_lookup_with_section name =
    (* Search all sections for this label *)
    let result = ref None in
    Asm_section.Tbl.iter
      (fun section state ->
        if Option.is_none !result
        then
          match Section_state.find_label_offset_in_bytes state name with
          | Some offset -> result := Some (offset, section, section_base section)
          | None -> (
            match Section_state.find_symbol_offset_in_bytes state name with
            | Some offset ->
              result := Some (offset, section, section_base section)
            | None -> ()))
      section_tbl;
    !result
  in
  (* Simple global_lookup returning absolute offset for backward compat *)
  let global_lookup name =
    match global_lookup_with_section name with
    | Some (offset, _section, base) -> Some (Int64.of_int (base + offset))
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
