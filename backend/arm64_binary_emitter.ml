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

open Arm64_ast
module Asm_section = Asm_targets.Asm_section
module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol

module Relocation = struct
  module Kind = struct
    type t =
      | R_AARCH64_ADR_PREL_LO21 of string
      | R_AARCH64_ADR_PREL_PG_HI21 of string
      | R_AARCH64_LD64_GOT_LO12_NC of string
      | R_AARCH64_ADD_ABS_LO12_NC of string
      | R_AARCH64_CALL26 of string
      | R_AARCH64_JUMP26 of string
      (* Absolute 64-bit data reference (ARM64_RELOC_UNSIGNED on macOS) *)
      | R_AARCH64_ABS64 of string
      (* Cross-section relative reference (SUBTRACTOR + UNSIGNED pair on macOS)
         Used for expressions like (Label - This) where Label and This are in
         different sections. The addend is stored in the data, and the linker
         will compute: addend + plus_symbol - minus_symbol *)
      | R_AARCH64_PREL32_PAIR of { plus_symbol : string; minus_symbol : string }
  end

  type t =
    { offset_from_section_beginning : int;
      kind : Kind.t
    }
end

(* TODO: Asm_directives uses strings for labels and symbols. We should change it
   to use Asm_label.t and Asm_symbol.t, and update this module accordingly. *)
type patch_size =
  | P8
  | P16
  | P32
  | P64

module Section_state = struct
  type t =
    { buffer : Buffer.t;
      mutable offset_in_bytes : int;
      symbol_offset_tbl : (string, int) Hashtbl.t;
      label_offset_tbl : (string, int) Hashtbl.t;
      mutable relocations : Relocation.t list;
      mutable patches : (int * patch_size * int64) list
    }

  let create () =
    { buffer = Buffer.create 1024;
      offset_in_bytes = 0;
      symbol_offset_tbl = Hashtbl.create 16;
      label_offset_tbl = Hashtbl.create 16;
      relocations = [];
      patches = []
    }

  let buffer t = t.buffer

  let offset_in_bytes t = t.offset_in_bytes

  let set_offset_in_bytes t offset = t.offset_in_bytes <- offset

  let add_relocation_at_current_offset t ~symbol_name:_ ~reloc_kind =
    t.relocations
      <- { Relocation.offset_from_section_beginning = t.offset_in_bytes;
           kind = reloc_kind
         }
         :: t.relocations

  let define_symbol t name =
    Hashtbl.replace t.symbol_offset_tbl name t.offset_in_bytes

  let define_label t name =
    Hashtbl.replace t.label_offset_tbl name t.offset_in_bytes

  let find_symbol_offset_in_bytes t name =
    Hashtbl.find_opt t.symbol_offset_tbl name

  let find_label_offset_in_bytes t name =
    Hashtbl.find_opt t.label_offset_tbl name

  (* Find the nearest global symbol at or before a given offset.
     Returns (symbol_name, symbol_offset) or None.

     NOTE: This is a somewhat surprising design choice. For cross-section
     relocations (e.g., frame table entries in DATA referencing labels in TEXT),
     we could create local symbols at exact positions and use addend=0. However,
     the system assembler instead uses existing global symbols (like
     _camlFoo__frametable and _camlFoo__fib_code) and computes a non-zero addend.
     We match this behavior to produce byte-identical output for testing purposes.
     Both approaches are correct for linking - the linker computes the same
     final value either way. *)
  let find_nearest_symbol_at_or_before t offset =
    let best = ref None in
    Hashtbl.iter
      (fun name sym_offset ->
        if sym_offset <= offset then begin
          match !best with
          | None -> best := Some (name, sym_offset)
          | Some (_, best_offset) when sym_offset > best_offset ->
            best := Some (name, sym_offset)
          | Some _ -> ()
        end)
      t.symbol_offset_tbl;
    !best

  (* Look up both symbols and labels - used for branch targets which can be
     either *)
  let find_symbol_or_label_offset_in_bytes t name =
    match Hashtbl.find_opt t.symbol_offset_tbl name with
    | Some _ as result -> result
    | None -> Hashtbl.find_opt t.label_offset_tbl name

  let relocations t = List.rev t.relocations

  let symbols t = t.symbol_offset_tbl

  let labels t = t.label_offset_tbl

  let add_patch t ~offset ~size ~data =
    t.patches <- (offset, size, data) :: t.patches

  let contents_mut t =
    let buf = Buffer.to_bytes t.buffer in
    let set_int8 pos v =
      Bytes.set buf pos (Char.chr (Int64.to_int v land 0xFF))
    in
    List.iter
      (fun (pos, size, v) ->
        match size with
        | P8 -> set_int8 pos v
        | P16 ->
          set_int8 pos v;
          set_int8 (pos + 1) (Int64.shift_right_logical v 8)
        | P32 ->
          set_int8 pos v;
          set_int8 (pos + 1) (Int64.shift_right_logical v 8);
          set_int8 (pos + 2) (Int64.shift_right_logical v 16);
          set_int8 (pos + 3) (Int64.shift_right_logical v 24)
        | P64 ->
          set_int8 pos v;
          set_int8 (pos + 1) (Int64.shift_right_logical v 8);
          set_int8 (pos + 2) (Int64.shift_right_logical v 16);
          set_int8 (pos + 3) (Int64.shift_right_logical v 24);
          set_int8 (pos + 4) (Int64.shift_right_logical v 32);
          set_int8 (pos + 5) (Int64.shift_right_logical v 40);
          set_int8 (pos + 6) (Int64.shift_right_logical v 48);
          set_int8 (pos + 7) (Int64.shift_right_logical v 56))
      t.patches;
    buf

  let contents t = Bytes.to_string (contents_mut t)
end

let encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let max_shift = if sf = 1 then 63 else 31 in
  if imm6 < 0 || imm6 > max_shift
  then Misc.fatal_errorf "ADD/SUB shift amount out of range: %d" imm6 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b01011) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Add/subtract (extended register) encoding - C4.1.92.2 Encoding: sf op S 01011
   00 1 Rm option imm3 Rn Rd Used when Rn is SP or when an extend operation is
   needed *)
let encode_add_sub_extended_register ~sf ~op ~s ~rm ~option ~imm3 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b01011) 24) in
  let result = logor result (shift_left (of_int 0b00) 22) in
  let result = logor result (shift_left (of_int 1) 21) in
  (* bit 21 = 1 for extended *)
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int option) 13) in
  let result = logor result (shift_left (of_int imm3) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Move wide (immediate) encoding - C4.1.92.7 Used for MOVN, MOVZ, MOVK *)
let encode_move_wide ~sf ~opc ~hw ~imm16 ~rd =
  let open Int32 in
  if imm16 < 0 || imm16 > 0xFFFF
  then Misc.fatal_errorf "MOVZ/MOVN/MOVK immediate out of range: %d" imm16 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100101) 23) in
  let result = logor result (shift_left (of_int hw) 21) in
  let result = logor result (shift_left (of_int imm16) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Bitfield encoding - C4.1.92.8 Used for SBFM, BFM, UBFM *)
let encode_bitfield ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100110) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (2 source) - C4.1.94.1 *)
let encode_data_proc_2_source ~sf ~s ~opcode ~rm ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Unconditional branch (register) - C4.1.93.13

   Encoding: 1101011 | opc[3:0] | op2[4:0] | op3[5:0] | Rn | op4[4:0] *)
let encode_branch_register ~opc ~rn =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b1101011) 25) in
  let result = logor result (shift_left (of_int opc) 21) in
  let result = logor result (shift_left (of_int 0b11111) 16) in
  (* op2 = 11111 *)
  let result = logor result (shift_left (of_int 0b000000) 10) in
  (* op3 = 000000 *)
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int 0b00000) in
  (* op4 = 00000 *)
  result

(* Unconditional branch (immediate) - C4.1.93.14 Encoding: op | 00101 | imm26 *)
let encode_branch_immediate ~op ~imm26 =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int 0b00101) 26) in
  let result = logor result (of_int (imm26 land 0x3FFFFFF)) in
  result

(* Compare and branch (immediate) - C4.1.93.15

   Encoding: sf | 011010 | op | imm19 | Rt *)
let encode_compare_branch ~sf ~op ~imm19 ~rt =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b011010) 25) in
  let result = logor result (shift_left (of_int op) 24) in
  let result = logor result (shift_left (of_int (imm19 land 0x7FFFF)) 5) in
  let result = logor result (of_int rt) in
  result

(* Test and branch (immediate) - C4.1.93.16

   Encoding: b5 | 011011 | op | b40 | imm14 | Rt *)
let encode_test_branch ~b5 ~op ~b40 ~imm14 ~rt =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int b5) 31) in
  let result = logor result (shift_left (of_int 0b011011) 25) in
  let result = logor result (shift_left (of_int op) 24) in
  let result = logor result (shift_left (of_int b40) 19) in
  let result = logor result (shift_left (of_int (imm14 land 0x3FFF)) 5) in
  let result = logor result (of_int rt) in
  result

(* Conditional select - C4.1.94.12

   Encoding: sf | op | S | 11010100 | Rm | cond | op2 | Rn | Rd *)
let encode_conditional_select ~sf ~op ~op2 ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int 0b0) 29) in
  (* S = 0 *)
  let result = logor result (shift_left (of_int 0b11010100) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int op2) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (3 source) - C4.1.94.13 *)
let encode_data_proc_3_source ~sf ~op54 ~op31 ~o0 ~rm ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op54) 29) in
  let result = logor result (shift_left (of_int 0b11011) 24) in
  let result = logor result (shift_left (of_int op31) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding ra)) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (1 source) - C4.1.94.2 *)
let encode_data_proc_1_source ~sf ~s ~opcode2 ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b1) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int opcode2) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

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

(* Helper for widening operations - returns source element size from dest type *)
(* For SMULL/UMULL: dest has 2x wider elements than source *)
let vector_widening_size (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int
    =
  match vec with
  | V8H -> 0b00 (* 16-bit dest -> 8-bit source *)
  | V4S -> 0b01 (* 32-bit dest -> 16-bit source *)
  | V2D -> 0b10 (* 64-bit dest -> 32-bit source *)
  | V8B | V16B | V4H | V2S | V1D ->
    Misc.fatal_error
      "Widening operations require H, S, or D destination elements"

(* Advanced SIMD two-register miscellaneous - C4.1.95.21 *)
let encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b10000) 17) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD three same - C4.1.95.24 *)
(* Encoding: 0 Q U 01110 size 1 Rm opcode 1 Rn Rd *)
let encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* Bit 31 is always 0, Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD three different - C4.1.95.23 *)
(* Encoding: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd *)
let encode_simd_three_different ~q ~u ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 12) in
  (* bits 11-10 = 00 *)
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD shift by immediate - C4.1.95.26 *)
(* Encoding: 0 Q U 01111 0 immh immb opcode 1 Rn Rd *)
(* immh encodes element size: 0001=8b, 001x=16b, 01xx=32b, 1xxx=64b *)
let encode_simd_shift_imm ~q ~u ~immh ~immb ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01111) 24) in
  (* bit 23 = 0 *)
  let result = logor result (shift_left (of_int immh) 19) in
  let result = logor result (shift_left (of_int immb) 16) in
  let result = logor result (shift_left (of_int opcode) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Helper to compute immh and immb for SHL instruction *)
(* For SHL: shift = (immh:immb) - element_width *)
(* So (immh:immb) = shift + element_width *)
let shl_immh_immb (type v s) (vec : (v, s) Neon_reg_name.Vector.t) shift =
  let esize, immh_base =
    match vec with
    | V8B | V16B -> 8, 0b0001
    | V4H | V8H -> 16, 0b0010
    | V2S | V4S -> 32, 0b0100
    | V1D | V2D -> 64, 0b1000
  in
  let immh_immb = shift + esize in
  let immh = (immh_immb lsr 3) lor immh_base in
  let immb = immh_immb land 0b111 in
  immh, immb

(* Helper to compute immh and immb for SSHR/USHR instructions *)
(* For SSHR/USHR: shift = element_width*2 - (immh:immb) *)
(* So (immh:immb) = element_width*2 - shift *)
let shr_immh_immb (type v s) (vec : (v, s) Neon_reg_name.Vector.t) shift =
  let esize =
    match vec with
    | V8B | V16B -> 8
    | V4H | V8H -> 16
    | V2S | V4S -> 32
    | V1D | V2D -> 64
  in
  let immh_immb = (esize * 2) - shift in
  let immh = immh_immb lsr 3 in
  let immb = immh_immb land 0b111 in
  immh, immb

(* Helper to compute immh for SXTL/UXTL (SSHLL/USHLL with shift=0) *)
(* immh encodes source element size: 0001=8b, 0010=16b, 0100=32b *)
(* Destination has 2x wider elements than source *)
let sxtl_immh (type v s) (vec : (v, s) Neon_reg_name.Vector.t) =
  match vec with
  | V8H -> 0b0001 (* 16-bit dest -> 8-bit source *)
  | V4S -> 0b0010 (* 32-bit dest -> 16-bit source *)
  | V2D -> 0b0100 (* 64-bit dest -> 32-bit source *)
  | V8B | V16B | V4H | V2S | V1D ->
    Misc.fatal_error "SXTL/UXTL requires H, S, or D destination elements"

(* Helper to compute imm5 for SIMD copy instructions (DUP, INS, SMOV, UMOV).
   imm5 encodes element size and lane index:

   - B: xxxx1 (index in bits 4:1)

   - H: xxx10 (index in bits 4:2)

   - S: xx100 (index in bits 4:3)

   - D: x1000 (index in bit 4) *)
let simd_copy_imm5 (type v s) (vec : (v, s) Neon_reg_name.Vector.t) lane_idx =
  match vec with
  | V8B | V16B -> (lane_idx lsl 1) lor 0b00001
  | V4H | V8H -> (lane_idx lsl 2) lor 0b00010
  | V2S | V4S -> (lane_idx lsl 3) lor 0b00100
  | V1D | V2D -> (lane_idx lsl 4) lor 0b01000

(* Advanced SIMD copy - C4.1.95.17 *)
(* Encoding: 0 Q op 01110 00 imm5 0 imm4 1 Rn Rd *)
let encode_simd_copy ~q ~op ~imm5 ~imm4 ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0, Q at bit 30, op at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int op) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  (* bits 23:22 = 00 *)
  let result = logor result (shift_left (of_int imm5) 16) in
  (* bit 15 = 0 *)
  let result = logor result (shift_left (of_int imm4) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD extract - C4.1.95.16 *)
(* Encoding: 0 Q 101110 op2 0 Rm 0 imm4 0 Rn Rd *)
let encode_simd_extract ~q ~rm ~imm4 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int 0b101110) 24) in
  (* op2=00, bit 21=0 *)
  let result = logor result (shift_left (of_int rm) 16) in
  (* bit 15=0 *)
  let result = logor result (shift_left (of_int imm4) 11) in
  (* bit 10=0 *)
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD modified immediate - C4.1.95.19 *)
(* Encoding: 0 Q op 0111100000 a b c cmode 01 d e f g h Rd *)
(* For MOVI: op=0, cmode determines the variant *)
let encode_simd_modified_imm ~q ~op ~abc ~cmode ~defgh ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int op) 29) in
  let result = logor result (shift_left (of_int 0b0111100000) 19) in
  let result = logor result (shift_left (of_int abc) 16) in
  let result = logor result (shift_left (of_int cmode) 12) in
  let result = logor result (shift_left (of_int 0b01) 10) in
  let result = logor result (shift_left (of_int defgh) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD across lanes - C4.1.95.22 *)
(* Encoding: 0 Q U 01110 size 11000 opcode 10 Rn Rd *)
let encode_simd_across_lanes ~q ~u ~size ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b11000) 17) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD permute *)
(* Encoding: 0 Q 0 01110 size 0 Rm 0 opcode 10 Rn Rd *)
let encode_simd_permute ~q ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, bit 29 = 0 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  (* bit 21 = 0 *)
  let result = logor result (shift_left (of_int rm) 16) in
  (* bit 15 = 0 *)
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (1 source) - C4.1.95.34

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | opcode | 10000 | Rn | Rd

   opcode: 000001=FABS, 000010=FNEG, 000011=FSQRT *)
let encode_fp_1_source ~ftype ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int opcode) 15) in
  let result = logor result (shift_left (of_int 0b10000) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (2 source) - C4.1.95.38 *)
let encode_fp_2_source ~ftype ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point conditional select - C4.1.95.39 *)
let encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (3 source) - C4.1.95.40 *)
let encode_fp_3_source ~ftype ~o1 ~rm ~o0 ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11111) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int o1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int ra) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point compare - C4.1.95.35

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | Rm | op=00 | 1000 | Rn | opcode2

   opcode2: 00000=FCMP(reg), 01000=FCMP(zero), 10000=FCMPE(reg),
   11000=FCMPE(zero) *)
let encode_fp_compare ~ftype ~rm ~opc2 ~rn =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int 0b00) 14) in
  let result = logor result (shift_left (of_int 0b1000) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int opc2) in
  result

(* Floating-point <-> integer conversion - C4.1.95.33

   Format: sf | 0 | S=0 | 11110 | ftype | 1 | rmode | opcode | 000000 | Rn | Rd

   Common opcodes:

   - SCVTF (int->FP): rmode=00, opcode=010

   - UCVTF (int->FP): rmode=00, opcode=011

   - FCVTNS (FP->int, nearest): rmode=00, opcode=000

   - FCVTPS (FP->int, +inf): rmode=01, opcode=000

   - FCVTMS (FP->int, -inf): rmode=10, opcode=000

   - FCVTZS (FP->int, zero): rmode=11, opcode=000

   - FCVTNU (FP->int, nearest unsigned): rmode=00, opcode=001

   - FCVTZU (FP->int, zero unsigned): rmode=11, opcode=001

   - FMOV (FP->GP or GP->FP): rmode=00, opcode=110/111 *)
let encode_fp_int_conv ~sf ~ftype ~rmode ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rmode) 19) in
  let result = logor result (shift_left (of_int opcode) 16) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point immediate - C4.1.95.36

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | imm8 | 100 | imm5=00000 | Rd *)
let encode_fp_immediate ~ftype ~imm8 ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int imm8) 13) in
  let result = logor result (shift_left (of_int 0b100) 10) in
  let result = logor result (of_int rd) in
  result

let encode_condition (cond : Cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | MI -> 0b0100
  | PL -> 0b0101
  | VS -> 0b0110
  | VC -> 0b0111
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101

(* Floating-point condition codes use the same encoding as integer conditions.
   The difference is semantic: after FCMP, the flags have different meanings. *)
let encode_float_condition (cond : Float_cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101

(* Conditional branch (immediate) - C4.1.93.1

   Encoding: 0101010 | 0 | imm19 | o0 | cond

   o0=0 for B.cond *)
let encode_conditional_branch ~imm19 ~cond =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b01010100) 24) in
  let result = logor result (shift_left (of_int (imm19 land 0x7FFFF)) 5) in
  let result = logor result (shift_left (of_int 0) 4) in
  (* o0 = 0 for B.cond *)
  let result = logor result (of_int cond) in
  result

(* Logical (shifted register) - C4.1.94.3 *)
let encode_logical_shifted_register ~sf ~opc ~shift ~n ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b01010) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int n) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Load register (literal) - C4.1.96.19 *)
let encode_load_literal ~opc ~v ~imm19 ~rt =
  assert (imm19 >= 0 && imm19 <= 0x7ffff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b011) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int imm19) 5) in
  let result = logor result (of_int rt) in
  result

(* PC-relative addressing - C4.1.92.2 *)
let encode_adr ~op ~immlo ~immhi ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int immlo) 29) in
  let result = logor result (shift_left (of_int 0b10000) 24) in
  let result = logor result (shift_left (of_int immhi) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

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

(* Logical (immediate) - C4.1.92.6 *)
let encode_logical_immediate ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100100) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Add/subtract (immediate) - C4.1.92.3 *)
let encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b100010) 23) in
  let result = logor result (shift_left (of_int sh) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Helper to encode add/sub immediate with automatic shift detection.
   When imm12 > 0xFFF but is a multiple of 4096 and the quotient fits in 12 bits,
   use sh=1 and encode imm12 / 4096. This matches the assembler's behavior when
   given large immediate values like #0x6000 which become #0x6, lsl #12. *)
let encode_add_sub_imm_auto_shift ~op ~s ~imm12 ~shift_opt ~rn ~rd =
  let sh_from_opt = match shift_opt with Some _ -> 1 | None -> 0 in
  let sh, actual_imm12 =
    if imm12 <= 0xFFF
    then sh_from_opt, imm12
    else if sh_from_opt = 1
    then
      Misc.fatal_errorf
        "Cannot encode add/sub immediate with explicit shift and value > 0xFFF"
    else if imm12 land 0xFFF = 0 && imm12 lsr 12 <= 0xFFF
    then 1, imm12 lsr 12
    else
      Misc.fatal_errorf "Cannot encode add/sub immediate %d (0x%x)" imm12 imm12
  in
  encode_add_sub_immediate ~sf:1 ~op ~s ~sh ~imm12:actual_imm12 ~rn ~rd

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

(* Helper to encode add/sub shifted register instructions.

   - op: 0=ADD, 1=SUB - s: 0=no flags, 1=set flags

   When Rn is SP, we must use extended register encoding because the shifted
   register form interprets register 31 as XZR, not SP. *)
let encode_add_sub_shifted_reg ~op ~s ~shift ~imm6 ~rd ~rn ~rm =
  let sf = Reg.gp_sf rd in
  let rd_enc = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let rm_enc = Reg.gp_encoding rm in
  (* Check if Rn is SP - if so, use extended register encoding *)
  if is_sp_reg rn && shift = 0 && imm6 = 0
  then
    (* Use extended register form with option=011 (UXTX/LSL for 64-bit) *)
    encode_add_sub_extended_register ~sf ~op ~s ~rm:rm_enc ~option:0b011 ~imm3:0
      ~rn:rn_enc ~rd:rd_enc
  else
    encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm:rm_enc ~imm6
      ~rn:rn_enc ~rd:rd_enc

(* Helper to encode logical shifted register instructions.

   - opc: 00=AND, 01=ORR, 10=EOR, 11=ANDS *)
let encode_logical_shifted_reg ~opc ~shift ~imm6 ~rd ~rn ~rm =
  let sf = Reg.gp_sf rd in
  let rd_enc = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let rm_enc = Reg.gp_encoding rm in
  encode_logical_shifted_register ~sf ~opc ~shift ~n:0 ~rm:rm_enc ~imm6
    ~rn:rn_enc ~rd:rd_enc

(* Load/store register (unscaled immediate) - C4.1.96.25 *)
let encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate post-indexed) - C4.1.96.26 *)
let encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b01) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate pre-indexed) - C4.1.96.28 *)
let encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (unsigned immediate) - C4.1.96.27 *)
let encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt =
  assert (imm12 >= 0 && imm12 <= 0xfff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int 0b01) 24) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (post-indexed) - C4.1.96.15

   Encoding: opc[1:0] | 101 | V | 001 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_post_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b001) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (pre-indexed) - C4.1.96.17

   Encoding: opc[1:0] | 101 | V | 011 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_pre_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b011) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (signed offset) - C4.1.96.16

   Encoding: opc[1:0] | 101 | V | 010 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_signed_offset ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b010) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Generalized load/store encoding for byte/halfword/word/doubleword operations.
   size: 00=byte, 01=halfword, 10=word, 11=doubleword opc encodes the operation
   (load/store and signed extension) *)
let encode_load_store_gp_sized :
    type a.
    Section_state.t ->
    instr_name:string ->
    size:int ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun state ~instr_name ~size ~opc ~rd addressing ->
  let vr = 0 in
  let rt = Reg.gp_encoding rd in
  match addressing with
  | Reg rn ->
    (* Use unsigned offset encoding with imm12=0 to match assembler behavior *)
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:0 ~rn ~rt
  | Offset_unscaled (rn, Nine_signed_unscaled imm) ->
    (* Prefer unsigned offset encoding when the offset is non-negative and
       properly aligned for the access size. This matches assembler behavior
       and provides a larger range for positive offsets.
       scale = 1 << size (1 for byte, 2 for half, 4 for word, 8 for double) *)
    let scale = 1 lsl size in
    let rn = Reg.gp_encoding rn in
    if imm >= 0 && imm mod scale = 0 && imm / scale <= 0xFFF
    then
      let imm12 = imm / scale in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt
    else
      let imm9 = imm land 0x1FF in
      encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt
  | Literal (_rn, sym) -> (
    (* This is encoded as "LDR (literal)" - only valid for word/doubleword *)
    if size < 0b10
    then Misc.fatal_errorf "%s does not support literal addressing" instr_name;
    match Section_state.find_symbol_offset_in_bytes state sym.name with
    | None ->
      Misc.fatal_errorf "%s (literal) references undefined symbol '%s' (rd=%s)"
        instr_name sym.name (Reg.name rd)
    | Some target_offset ->
      let pc_relative_offset =
        target_offset - Section_state.offset_in_bytes state
      in
      assert (pc_relative_offset > 0);
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' must be 4-byte aligned"
          instr_name pc_relative_offset sym.name;
      let imm19_unmasked = pc_relative_offset / 4 in
      if imm19_unmasked < -0x40000 || imm19_unmasked > 0x3ffff
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' out of range (max ±1MB)"
          instr_name pc_relative_offset sym.name;
      let imm19 = imm19_unmasked land 0x7FFFF in
      let v = 0 in
      encode_load_literal ~opc ~v ~imm19 ~rt)
  | Offset_imm (rn, Twelve_unsigned_scaled imm12) ->
    let max_imm12 = 0xfff in
    (* Scale depends on size: byte=1, half=2, word=4, double=8 *)
    let scale = 1 lsl size in
    if imm12 mod scale <> 0
    then
      Misc.fatal_errorf
        "%s offset %d must be aligned to %d-byte access size (rd=%s, rn=%s)"
        instr_name imm12 scale (Reg.name rd) (Reg.name rn);
    let rn = Reg.gp_encoding rn in
    let imm12_scaled = imm12 / scale in
    if imm12_scaled < 0 || imm12_scaled > max_imm12
    then
      Misc.fatal_errorf
        "%s offset %d (scaled: %d) out of range (max 0x%x * %d = 0x%x bytes)"
        instr_name imm12 imm12_scaled max_imm12 scale (max_imm12 * scale);
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:imm12_scaled ~rn ~rt
  | Offset_sym (rn, sym) -> (
    match sym.reloc with
    | Needs_reloc reloc ->
      let max_imm12 = 0xfff in
      let reloc_kind : Relocation.Kind.t =
        match reloc with
        | GOT_PAGE_OFF | GOT_LOWER_TWELVE -> R_AARCH64_LD64_GOT_LO12_NC sym.name
        | PAGE_OFF | LOWER_TWELVE -> R_AARCH64_ADD_ABS_LO12_NC sym.name
      in
      Section_state.add_relocation_at_current_offset state ~symbol_name:sym.name
        ~reloc_kind;
      let rn = Reg.gp_encoding rn in
      let imm12_unmasked = sym.offset lsr size in
      if imm12_unmasked < 0 || imm12_unmasked > max_imm12
      then
        Misc.fatal_errorf
          "%s symbol offset %d (shifted by %d) out of range (max 0x%x)"
          instr_name sym.offset size max_imm12;
      let imm12 = imm12_unmasked land 0xFFF in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt)
  | Pre (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Post (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt

let encode_load_store_gp :
    type a.
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun state ~instr_name ~opc ~rd addressing ->
  let size =
    match rd.reg_name with
    | GP W | GP WZR | GP WSP -> 0b10
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b11
  in
  encode_load_store_gp_sized state ~instr_name ~size ~opc ~rd addressing

(* Byte load/store - size=00 *)
let encode_load_store_byte :
    type a.
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun state ~instr_name ~opc ~rd addressing ->
  encode_load_store_gp_sized state ~instr_name ~size:0b00 ~opc ~rd addressing

(* Halfword load/store - size=01 *)
let encode_load_store_halfword :
    type a.
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun state ~instr_name ~opc ~rd addressing ->
  encode_load_store_gp_sized state ~instr_name ~size:0b01 ~opc ~rd addressing

(* Load-Acquire (LDAR) encoding.
   Format: size[31:30] | 001000 | o2[23] | L[22] | o1[21] | Rs[20:16] |
           o0[15] | Rt2[14:10] | Rn[9:5] | Rt[4:0]
   For LDAR: o2=1, L=1, o1=0, Rs=11111, o0=1, Rt2=11111
   size: 10 for 32-bit, 11 for 64-bit *)
let encode_load_acquire :
    type a. rd:[`GP of a] Reg.t -> rn:[`GP of _] Reg.t -> int32 =
 fun ~rd ~rn ->
  let size =
    match rd.reg_name with
    | GP W | GP WZR | GP WSP -> 0b10
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b11
  in
  let rt = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let o2 = 1 in
  let l = 1 in
  let o1 = 0 in
  let o0 = 1 in
  let rs = 0b11111 in
  let rt2 = 0b11111 in
  let open Int32 in
  let result = shift_left (of_int size) 30 in
  let result = logor result (shift_left (of_int 0b001000) 24) in
  let result = logor result (shift_left (of_int o2) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int o1) 21) in
  let result = logor result (shift_left (of_int rs) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn_enc) 5) in
  let result = logor result (of_int rt) in
  result

(* Memory barrier encoding. Format: 1101 0101 0000 0011 0011 | CRm[11:8] |
   op2[7:5] | 11111

   - DMB: op2=101

   - DSB: op2=100 *)
let encode_memory_barrier ~op2 (barrier : Memory_barrier.t) =
  let crm =
    match barrier with
    | SY -> 0b1111
    | ST -> 0b1110
    | LD -> 0b1101
    | ISH -> 0b1011
    | ISHST -> 0b1010
    | ISHLD -> 0b1001
    | NSH -> 0b0111
    | NSHST -> 0b0110
    | NSHLD -> 0b0101
    | OSH -> 0b0011
    | OSHST -> 0b0010
    | OSHLD -> 0b0001
  in
  let open Int32 in
  (* 1101 0101 0000 0011 0011 = 0xD503_30 shifted appropriately *)
  let result = of_int 0b11010101000000110011 in
  let result = shift_left result 12 in
  let result = logor result (shift_left (of_int crm) 8) in
  let result = logor result (shift_left (of_int op2) 5) in
  let result = logor result (of_int 0b11111) in
  result

(* NOP encoding: 1101 0101 0000 0011 0010 0000 000 11111 = 0xD503201F *)
let encode_nop () = Int32.of_int 0xD503201F

(* YIELD encoding: 1101 0101 0000 0011 0010 0000 001 11111 = 0xD503203F *)
let encode_yield () = Int32.of_int 0xD503203F

(* Encode LDP/STP instructions for GP registers. l=1 for load (LDP), l=0 for
   store (STP). opc: 00 for 32-bit (W), 10 for 64-bit (X) *)
let encode_load_store_pair_gp :
    type a b.
    instr_name:string ->
    l:int ->
    rt1:[`GP of a] Reg.t ->
    rt2:[`GP of b] Reg.t ->
    [< `Offset_pair | `Pre_pair | `Post_pair] Operand.Addressing_mode.t ->
    int32 =
 fun ~instr_name ~l ~rt1 ~rt2 addressing ->
  let opc =
    match rt1.reg_name with
    | GP W | GP WZR | GP WSP -> 0b00
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b10
  in
  let scale = if opc = 0b10 then 8 else 4 in
  let v = 0 in
  let rt1_enc = Reg.gp_encoding rt1 in
  let rt2_enc = Reg.gp_encoding rt2 in
  let encode_with_alignment_check encode_fn rn imm =
    if imm mod scale <> 0
    then
      Misc.fatal_errorf "%s offset %d must be aligned to %d bytes" instr_name
        imm scale;
    let imm7 = imm / scale in
    let rn = Reg.gp_encoding rn in
    encode_fn ~opc ~v ~l ~imm7 ~rt2:rt2_enc ~rn ~rt:rt1_enc
  in
  match addressing with
  | Offset_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_signed_offset rn imm
  | Pre_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_pre_indexed rn imm
  | Post_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_post_indexed rn imm

(* Encode load/store for SIMD&FP registers.

   For LDR: S->opc=01, D->opc=01, Q->opc=11

   For STR: S->opc=00, D->opc=00, Q->opc=10

   size: S->10, D->11, Q->00 *)
let encode_load_store_simd_fp :
    type s.
    Section_state.t ->
    instr_name:string ->
    is_load:bool ->
    rd:[`Neon of [`Scalar of s]] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun state ~instr_name ~is_load ~rd addressing ->
  let vr = 1 in
  let size, opc, scale =
    match rd.reg_name with
    | Neon (Scalar S) -> 0b10, (if is_load then 0b01 else 0b00), 4
    | Neon (Scalar D) -> 0b11, (if is_load then 0b01 else 0b00), 8
    | Neon (Scalar Q) -> 0b00, (if is_load then 0b11 else 0b10), 16
    | Neon (Scalar B) -> 0b00, (if is_load then 0b01 else 0b00), 1
    | Neon (Scalar H) -> 0b01, (if is_load then 0b01 else 0b00), 2
  in
  let rt = rd.index in
  match addressing with
  | Reg rn ->
    (* Use unsigned offset encoding with imm12=0 to match assembler behavior *)
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:0 ~rn ~rt
  | Offset_unscaled (rn, Nine_signed_unscaled imm) ->
    (* Prefer unsigned offset encoding when the offset is non-negative and
       properly aligned for the access size. This matches assembler behavior. *)
    let rn = Reg.gp_encoding rn in
    if imm >= 0 && imm mod scale = 0 && imm / scale <= 0xFFF
    then
      let imm12 = imm / scale in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt
    else
      let imm9 = imm land 0x1FF in
      encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt
  | Literal (_rn, sym) -> (
    match Section_state.find_symbol_offset_in_bytes state sym.name with
    | None ->
      Misc.fatal_errorf "%s (literal) references undefined symbol '%s'"
        instr_name sym.name
    | Some target_offset ->
      let pc_relative_offset =
        target_offset - Section_state.offset_in_bytes state
      in
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' must be 4-byte aligned"
          instr_name pc_relative_offset sym.name;
      let imm19_unmasked = pc_relative_offset / 4 in
      if imm19_unmasked < -0x40000 || imm19_unmasked > 0x3ffff
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' out of range (max ±1MB)"
          instr_name pc_relative_offset sym.name;
      let imm19 = imm19_unmasked land 0x7FFFF in
      let opc_lit =
        match rd.reg_name with
        | Neon (Scalar S) -> 0b00
        | Neon (Scalar D) -> 0b01
        | Neon (Scalar Q) -> 0b10
        | Neon (Scalar B) | Neon (Scalar H) ->
          Misc.fatal_errorf "%s (literal) not supported for B/H registers"
            instr_name
      in
      encode_load_literal ~opc:opc_lit ~v:1 ~imm19 ~rt)
  | Offset_imm (rn, Twelve_unsigned_scaled imm12) ->
    if imm12 mod scale <> 0
    then
      Misc.fatal_errorf "%s offset %d must be aligned to %d-byte access size"
        instr_name imm12 scale;
    let imm12_scaled = imm12 / scale in
    if imm12_scaled < 0 || imm12_scaled > 0xfff
    then
      Misc.fatal_errorf "%s offset %d (scaled: %d) out of range" instr_name
        imm12 imm12_scaled;
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:imm12_scaled ~rn ~rt
  | Offset_sym (rn, sym) -> (
    match sym.reloc with
    | Needs_reloc reloc ->
      let max_imm12 = 0xfff in
      let reloc_kind : Relocation.Kind.t =
        match reloc with
        | GOT_PAGE_OFF | GOT_LOWER_TWELVE -> R_AARCH64_LD64_GOT_LO12_NC sym.name
        | PAGE_OFF | LOWER_TWELVE -> R_AARCH64_ADD_ABS_LO12_NC sym.name
      in
      Section_state.add_relocation_at_current_offset state ~symbol_name:sym.name
        ~reloc_kind;
      let rn = Reg.gp_encoding rn in
      let imm12_unmasked = sym.offset / scale in
      if imm12_unmasked < 0 || imm12_unmasked > max_imm12
      then
        Misc.fatal_errorf
          "%s symbol offset %d (scaled by %d) out of range (max 0x%x)"
          instr_name sym.offset scale max_imm12;
      let imm12 = imm12_unmasked land 0xFFF in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt)
  | Pre (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Post (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt

(* Helper to compute a 26-bit PC-relative offset for B/BL instructions. If the
   symbol is undefined, creates a relocation and returns 0. *)
let compute_branch_imm26 state ~instr_name ~reloc_kind (sym : _ Symbol.t) =
  let symbol_name = sym.name in
  match
    Section_state.find_symbol_or_label_offset_in_bytes state symbol_name
  with
  | None ->
    (* Symbol is undefined - create a relocation and use 0 as placeholder *)
    Section_state.add_relocation_at_current_offset state ~symbol_name
      ~reloc_kind:(reloc_kind symbol_name);
    0
  | Some target_offset ->
    let pc_relative_offset =
      target_offset - Section_state.offset_in_bytes state
    in
    if pc_relative_offset mod 4 <> 0
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
        instr_name pc_relative_offset symbol_name;
    let imm26 = pc_relative_offset / 4 in
    if imm26 < -0x2000000 || imm26 > 0x1FFFFFF
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' out of range (max ±128MB)"
        instr_name pc_relative_offset symbol_name;
    imm26

(* Helper to compute a 19-bit PC-relative offset for CBZ/CBNZ instructions *)
let compute_branch_imm19 state ~instr_name (sym : _ Symbol.t) =
  let symbol_name = sym.name in
  match
    Section_state.find_symbol_or_label_offset_in_bytes state symbol_name
  with
  | None ->
    Misc.fatal_errorf "%s references undefined symbol '%s'" instr_name
      symbol_name
  | Some target_offset ->
    let pc_relative_offset =
      target_offset - Section_state.offset_in_bytes state
    in
    if pc_relative_offset mod 4 <> 0
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
        instr_name pc_relative_offset symbol_name;
    let imm19 = pc_relative_offset / 4 in
    if imm19 < -0x40000 || imm19 > 0x3FFFF
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' out of range (max ±1MB)"
        instr_name pc_relative_offset symbol_name;
    imm19

(* Helper to compute a 14-bit PC-relative offset for TBZ/TBNZ instructions *)
let compute_branch_imm14 state ~instr_name (sym : _ Symbol.t) =
  let symbol_name = sym.name in
  match
    Section_state.find_symbol_or_label_offset_in_bytes state symbol_name
  with
  | None ->
    Misc.fatal_errorf "%s references undefined symbol '%s'" instr_name
      symbol_name
  | Some target_offset ->
    let pc_relative_offset =
      target_offset - Section_state.offset_in_bytes state
    in
    if pc_relative_offset mod 4 <> 0
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
        instr_name pc_relative_offset symbol_name;
    let imm14 = pc_relative_offset / 4 in
    if imm14 < -0x2000 || imm14 > 0x1FFF
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' out of range (max ±32KB)"
        instr_name pc_relative_offset symbol_name;
    imm14

(* Helper to extract ftype from scalar precision: S=0, D=1 *)
let scalar_ftype (type a) (s : [`Scalar of a] Neon_reg_name.t) : int =
  match s with Scalar S -> 0 | Scalar D -> 1 | _ -> assert false

let encode_instruction :
    type num operands.
    Section_state.t ->
    (num, operands) Instruction_name.t ->
    (num, operands) many ->
    int32 =
 fun state instr operands ->
  match operands, instr with
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      ABS_vector ) ->
    let q, size = vector_q_size vec in
    (* ABS: U=0, opcode=01011 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size ~opcode:0b01011 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADD_immediate ->
    encode_add_sub_imm_auto_shift ~op:0 ~s:0 ~imm12 ~shift_opt:shift ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Sym sym), Optional shift), ADD_immediate -> (
    let sh = match shift with Some _ -> 1 | None -> 0 in
    match sym.reloc with
    | Needs_reloc reloc ->
      let reloc_kind : Relocation.Kind.t =
        match reloc with
        | LOWER_TWELVE | PAGE_OFF -> R_AARCH64_ADD_ABS_LO12_NC sym.name
        | GOT_LOWER_TWELVE | GOT_PAGE_OFF -> R_AARCH64_LD64_GOT_LO12_NC sym.name
      in
      Section_state.add_relocation_at_current_offset state ~symbol_name:sym.name
        ~reloc_kind;
      encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh ~imm12:sym.offset ~rn ~rd)
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      ADD_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_add_sub_shifted_reg ~op:0 ~s:0 ~shift ~imm6 ~rd ~rn ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ADDP_vector ) ->
    let q, size = vector_q_size vec in
    (* ADDP: U=0, opcode=10111 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10111 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADDS ->
    encode_add_sub_imm_auto_shift ~op:0 ~s:1 ~imm12 ~shift_opt:shift ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ADD_vector ) ->
    let q, size = vector_q_size vec in
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10000 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      ADDV ) ->
    let q, size = vector_q_size vec in
    (* ADDV: U=0, opcode=11011 *)
    encode_simd_across_lanes ~q ~u:0 ~size ~opcode:0b11011 ~rn ~rd
  | Pair (Reg rd, Imm (Sym sym)), ADR ->
    (* ADR only accepts Same_section_and_unit symbols (local labels) *)
    let Same_section_and_unit = sym.reloc in
    (* Compute PC-relative offset at assembly time *)
    let target_offset =
      match Section_state.find_label_offset_in_bytes state sym.name with
      | Some off -> off
      | None ->
        Misc.fatal_errorf "ADR: label %s not found in current section" sym.name
    in
    let current_offset = Section_state.offset_in_bytes state in
    let pc_rel = target_offset - current_offset + sym.offset in
    let immlo, immhi = split_21bit_immediate pc_rel in
    encode_adr ~op:0 ~immlo ~immhi ~rd
  | Pair (Reg rd, Imm (Sym sym)), ADRP ->
    Section_state.add_relocation_at_current_offset state ~symbol_name:sym.name
      ~reloc_kind:(R_AARCH64_ADR_PREL_PG_HI21 sym.name);
    let immlo, immhi = split_21bit_immediate sym.offset in
    encode_adr ~op:1 ~immlo ~immhi ~rd
  | Triple (Reg rd, Reg rn, Bitmask bitmask), AND_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b00 ~n ~immr ~imms ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      AND_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_logical_shifted_reg ~opc:0b00 ~shift ~imm6 ~rd ~rn ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      AND_vector ) ->
    let q, _ = vector_q_size vec in
    (* AND: U=0, size=00, opcode=00011 *)
    encode_simd_three_same ~q ~u:0 ~size:0b00 ~rm ~opcode:0b00011 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), ASRV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001010 ~rm ~rn ~rd
  | Singleton (Imm (Sym sym)), B ->
    let imm26 =
      compute_branch_imm26 state ~instr_name:"B"
        ~reloc_kind:(fun s -> R_AARCH64_JUMP26 s)
        sym
    in
    encode_branch_immediate ~op:0 ~imm26
  | Singleton (Imm (Sym sym)), B_cond cond ->
    let imm19 = compute_branch_imm19 state ~instr_name:"B.cond" sym in
    let cond = encode_condition cond in
    encode_conditional_branch ~imm19 ~cond
  | Singleton (Imm (Sym sym)), B_cond_float cond ->
    let imm19 = compute_branch_imm19 state ~instr_name:"B.cond" sym in
    let cond = encode_float_condition cond in
    encode_conditional_branch ~imm19 ~cond
  | Singleton (Imm (Sym sym)), BL ->
    let imm26 =
      compute_branch_imm26 state ~instr_name:"BL"
        ~reloc_kind:(fun s -> R_AARCH64_CALL26 s)
        sym
    in
    encode_branch_immediate ~op:1 ~imm26
  | Singleton (Reg rn), BLR -> encode_branch_register ~opc:0b0001 ~rn
  | Singleton (Reg rn), BR -> encode_branch_register ~opc:0b0000 ~rn
  | Pair (Reg rt, Imm (Sym sym)), CBNZ ->
    let imm19 = compute_branch_imm19 state ~instr_name:"CBNZ" sym in
    let sf = Reg.gp_sf rt in
    let rt = Reg.gp_encoding rt in
    encode_compare_branch ~sf ~op:1 ~imm19 ~rt
  | Pair (Reg rt, Imm (Sym sym)), CBZ ->
    let imm19 = compute_branch_imm19 state ~instr_name:"CBZ" sym in
    let sf = Reg.gp_sf rt in
    let rt = Reg.gp_encoding rt in
    encode_compare_branch ~sf ~op:0 ~imm19 ~rt
  | Pair (Reg rd, Reg rn), CLZ ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      CM_register cond ) ->
    let q, size = vector_q_size vec in
    (* Integer vector compares (register):

       - CMGT: U=0, opcode=00110

       - CMGE: U=0, opcode=00111

       - CMEQ: U=1, opcode=10001

       - CMHI: U=1, opcode=00110

       - CMHS: U=1, opcode=00111 *)
    let u, opcode =
      match cond with
      | Cond.GT -> 0, 0b00110
      | Cond.GE -> 0, 0b00111
      | Cond.EQ -> 1, 0b10001
      | Cond.HI -> 1, 0b00110
      | Cond.CS -> 1, 0b00111 (* HS/CS: unsigned greater or equal *)
      | _ -> Misc.fatal_error "Unsupported CM_register condition"
    in
    encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      CM_zero cond ) ->
    let q, size = vector_q_size vec in
    (* Integer vector compares (zero):

       - CMGT (zero): U=0, opcode=01000

       - CMEQ (zero): U=0, opcode=01001

       - CMLT (zero): U=0, opcode=01010

       - CMGE (zero): U=1, opcode=01000

       - CMLE (zero): U=1, opcode=01001 *)
    let u, opcode =
      match cond with
      | Cond.GT -> 0, 0b01000
      | Cond.EQ -> 0, 0b01001
      | Cond.LT -> 0, 0b01010
      | Cond.GE -> 1, 0b01000
      | Cond.LE -> 1, 0b01001
      | _ -> Misc.fatal_error "Unsupported CM_zero condition"
    in
    encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | Pair (Reg rd, Reg rn), CNT ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000111 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      CNT_vector ) ->
    let q, _ = vector_q_size vec in
    (* CNT: U=0, size=00, opcode=00101 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size:0b00 ~opcode:0b00101 ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Cond cond), CSEL ->
    let sf = Reg.gp_sf rd in
    let cond = encode_condition cond in
    encode_conditional_select ~sf ~op:0 ~op2:0b00 ~rm ~cond ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Cond cond), CSINC ->
    let sf = Reg.gp_sf rd in
    let cond = encode_condition cond in
    encode_conditional_select ~sf ~op:0 ~op2:0b01 ~rm ~cond ~rn ~rd
  | Pair (Reg rd, Reg rn), CTZ ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000110 ~rn ~rd
  | _, DMB barrier -> encode_memory_barrier ~op2:0b101 barrier
  | _, DSB barrier -> encode_memory_barrier ~op2:0b100 barrier
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      DUP lane_idx ) ->
    let q, _ = vector_q_size vec in
    let imm5 = simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx) in
    (* DUP (element): op=0, imm4=0000 *)
    encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0000 ~rn ~rd
  | Triple (Reg rd, Reg rn, Bitmask bitmask), EOR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b10 ~n ~immr ~imms ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      EOR_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_logical_shifted_reg ~opc:0b10 ~shift ~imm6 ~rd ~rn ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      EOR_vector ) ->
    let q, _ = vector_q_size vec in
    (* EOR: U=1, size=00, opcode=00011 *)
    encode_simd_three_same ~q ~u:1 ~size:0b00 ~rm ~opcode:0b00011 ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Imm (Six imm4)), EXT ->
    (* EXT is 128-bit only (V16B), so Q=1 *)
    encode_simd_extract ~q:1 ~rm:rm.index ~imm4 ~rn:rn.index ~rd:rd.index
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FABS ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_1_source ~ftype ~opcode:0b000001 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADD ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADDP_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FADDP: U=1, size=0x, opcode=11010 *)
    encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADD_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FADD: U=0, size=0x, opcode=11010 *)
    encode_simd_three_same ~q ~u:0 ~size:sz ~rm ~opcode:0b11010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FCM_register cond ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FP vector compares (register):

       - FCMEQ: U=0, size=0x, opcode=11100

       - FCMGE: U=1, size=0x, opcode=11100

       - FCMGT: U=1, size=1x, opcode=11100 *)
    let u, size_hi =
      match cond with
      | Float_cond.EQ -> 0, 0
      | Float_cond.GE -> 1, 0
      | Float_cond.GT -> 1, 1
      | _ -> Misc.fatal_error "Unsupported FCM_register condition"
    in
    let size = (size_hi lsl 1) lor sz in
    encode_simd_three_same ~q ~u ~size ~rm ~opcode:0b11100 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCM_zero cond ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FP vector compares (zero):

       - FCMGT (zero): U=0, size=1x, opcode=01100

       - FCMEQ (zero): U=0, size=1x, opcode=01101

       - FCMLT (zero): U=0, size=1x, opcode=01110

       - FCMGE (zero): U=1, size=1x, opcode=01100

       - FCMLE (zero): U=1, size=1x, opcode=01101 *)
    let u, opcode =
      match cond with
      | Float_cond.GT -> 0, 0b01100
      | Float_cond.EQ -> 0, 0b01101
      | Float_cond.LT -> 0, 0b01110
      | Float_cond.GE -> 1, 0b01100
      | Float_cond.LE -> 1, 0b01101
      | _ -> Misc.fatal_error "Unsupported FCM_zero condition"
    in
    let size = (1 lsl 1) lor sz in
    (* size=1x *)
    encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rn },
          Reg { index = rm; _ } ),
      FCMP ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_compare ~ftype ~rm ~opc2:0b00000 ~rn
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Cond cond ),
      FCSEL ) ->
    let ftype = scalar_ftype scalar in
    let cond = encode_condition cond in
    encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd
  (* FCVT: convert between single and double precision *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = Neon (Scalar S); index = rn } ),
      FCVT ) ->
    (* FCVT Dd, Sn: ftype=00 (source=single), opcode=000101 (to double) *)
    encode_fp_1_source ~ftype:0 ~opcode:0b000101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = Neon (Scalar D); index = rn } ),
      FCVT ) ->
    (* FCVT Sd, Dn: ftype=01 (source=double), opcode=000100 (to single) *)
    encode_fp_1_source ~ftype:1 ~opcode:0b000100 ~rn ~rd
  (* FCVT same-precision conversions: use FMOV instead *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FCVT ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_1_source ~ftype ~opcode:0b000000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTL_vector ) ->
    (* FCVTL converts from narrower to wider FP (e.g., V2S->V2D) U=0,
       opcode=10111, size encodes source element size (0=32-bit->64-bit) *)
    let size =
      match vec with
      | V2D -> 0 (* 32-bit source -> 64-bit dest *)
      | _ -> Misc.fatal_error "FCVTL_vector: unsupported destination type"
    in
    (* Q=0 for lower half (FCVTL), Q=1 for upper half (FCVTL2) *)
    encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10111 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTN_vector ) ->
    (* FCVTN converts from wider to narrower FP (e.g., V2D->V2S) U=0,
       opcode=10110, size encodes destination element size *)
    let size =
      match vec with
      | V2S -> 0 (* 64-bit source -> 32-bit dest *)
      | _ -> Misc.fatal_error "FCVTN_vector: unsupported destination type"
    in
    (* Q=0 for lower half (FCVTN), Q=1 for upper half (FCVTN2) *)
    encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10110 ~rn ~rd
  (* FCVTNS: FP to signed int, round to nearest with ties to even *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar _ as scalar); index = rn } ),
      FCVTNS ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b00 ~opcode:0b000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTNS_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FCVTNS (vector): U=0, size=0x, opcode=11010 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size:sz ~opcode:0b11010 ~rn ~rd
  (* FCVTZS: FP to signed int, round toward zero *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar _ as scalar); index = rn } ),
      FCVTZS ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b11 ~opcode:0b000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTZS_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FCVTZS (vector, integer): U=0, size=1x, opcode=11011 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size:(0b10 lor sz) ~opcode:0b11011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FDIV ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0001 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FDIV_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FDIV: U=1, size=0x, opcode=11111 *)
    encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11111 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FMADD ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_3_source ~ftype ~o1:0 ~rm ~o0:0 ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMAX ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMAX_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FMAX: U=0, size=0x, opcode=11110 *)
    encode_simd_three_same ~q ~u:0 ~size:sz ~rm ~opcode:0b11110 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMIN ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0101 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMIN_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FMIN: U=0, size=1x, opcode=11110 *)
    encode_simd_three_same ~q ~u:0 ~size:(0b10 lor sz) ~rm ~opcode:0b11110 ~rn
      ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FMSUB ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_3_source ~ftype ~o1:0 ~rm ~o0:1 ~ra ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FMOV_fp ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_1_source ~ftype ~opcode:0b000000 ~rn ~rd
  (* FMOV_gp_to_fp_32: GP-to-FP (W to S) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = GP W; index = rn } ),
      FMOV_gp_to_fp_32 ) ->
    (* sf=0, ftype=00, rmode=00, opcode=111 for GP->FP single *)
    encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b111 ~rn ~rd
  (* FMOV_gp_to_fp_32: GP-to-FP (WZR to S) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = GP WZR; index = rn } ),
      FMOV_gp_to_fp_32 ) ->
    encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b111 ~rn ~rd
  (* FMOV_gp_to_fp_64: GP-to-FP (X to D) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = GP X; index = rn } ),
      FMOV_gp_to_fp_64 ) ->
    (* sf=1, ftype=01, rmode=00, opcode=111 for GP->FP double *)
    encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b111 ~rn ~rd
  (* FMOV_gp_to_fp_64: GP-to-FP (XZR to D) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = GP XZR; index = rn } ),
      FMOV_gp_to_fp_64 ) ->
    encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b111 ~rn ~rd
  (* FMOV_fp_to_gp_32: FP-to-GP (S to W) *)
  | ( Pair
        ( Reg { reg_name = GP W; index = rd },
          Reg { reg_name = Neon (Scalar S); index = rn } ),
      FMOV_fp_to_gp_32 ) ->
    (* sf=0, ftype=00, rmode=00, opcode=110 for FP->GP single *)
    encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b110 ~rn ~rd
  (* FMOV_fp_to_gp_64: FP-to-GP (D to X) *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar D); index = rn } ),
      FMOV_fp_to_gp_64 ) ->
    (* sf=1, ftype=01, rmode=00, opcode=110 for FP->GP double *)
    encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b110 ~rn ~rd
  (* FMOV scalar immediate - Float case *)
  | ( Pair
        (Reg { reg_name = Neon (Scalar _ as scalar); index = rd }, Imm (Float f)),
      FMOV_scalar_immediate ) ->
    let ftype = scalar_ftype scalar in
    let bits = Int64.bits_of_float f in
    (* Extract imm8 from double-precision IEEE bits using VFPExpandImm inverse:
       VFPExpandImm expands imm8 to: sign:NOT(imm8[6]):Replicate(imm8[6],8):imm8[5:0]:Zeros(48)
       So to encode, we extract:
       imm8[7] = sign (bit 63)
       imm8[6] = NOT(bit 62) - inverted MSB of exponent
       imm8[5:4] = bits 53:52 - lower exponent bits after the replicated part
       imm8[3:0] = bits 51:48 - top 4 mantissa bits
       For single, we convert from double representation. *)
    let sign = Int64.(to_int (logand (shift_right_logical bits 63) 1L)) in
    let exp10 = Int64.(to_int (logand (shift_right_logical bits 62) 1L)) in
    let imm8_5_4 = Int64.(to_int (logand (shift_right_logical bits 52) 3L)) in
    let frac = Int64.(to_int (logand (shift_right_logical bits 48) 0xFL)) in
    let imm8 =
      (sign lsl 7) lor ((1 - exp10) lsl 6) lor (imm8_5_4 lsl 4) lor frac
    in
    encode_fp_immediate ~ftype ~imm8 ~rd
  (* FMOV scalar immediate - Nativeint case (raw bits) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Imm (Nativeint n) ),
      FMOV_scalar_immediate ) ->
    let ftype = scalar_ftype scalar in
    let imm8 = Nativeint.to_int n land 0xFF in
    encode_fp_immediate ~ftype ~imm8 ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMUL ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0000 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMUL_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FMUL: U=1, size=0x, opcode=11011 *)
    encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11011 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FNEG ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_1_source ~ftype ~opcode:0b000010 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FNEG_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FNEG: U=1, size=1x, opcode=01111 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz) ~opcode:0b01111 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FNMADD ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_3_source ~ftype ~o1:1 ~rm ~o0:0 ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FNMUL ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b1000 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FNMSUB ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_3_source ~ftype ~o1:1 ~rm ~o0:1 ~ra ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRECPE_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FRECPE: U=0, size=1x, opcode=11101 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size:(0b10 lor sz) ~opcode:0b11101 ~rn ~rd
  (* FRINT: round FP to integer in FP format.

     Opcodes: N=001000, P=001001, M=001010, Z=001011, A=001100, X=001110 *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FRINT rmode ) ->
    let ftype = scalar_ftype scalar in
    let opcode =
      match rmode with
      | Rounding_mode.N -> 0b001000
      | Rounding_mode.P -> 0b001001
      | Rounding_mode.M -> 0b001010
      | Rounding_mode.Z -> 0b001011
      | Rounding_mode.X -> 0b001110
    in
    encode_fp_1_source ~ftype ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRINT_vector rm ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FRINT(mode) vector - encoding depends on rounding mode:

       - N: U=0, size=0x, opcode=11000

       - M: U=0, size=0x, opcode=11001

       - P: U=0, size=1x, opcode=11000

       - Z: U=0, size=1x, opcode=11001

       - X: U=1, size=0x, opcode=11001 *)
    let u, size_hi, opcode =
      match rm with
      | Rounding_mode.N -> 0, 0, 0b11000
      | Rounding_mode.M -> 0, 0, 0b11001
      | Rounding_mode.P -> 0, 1, 0b11000
      | Rounding_mode.Z -> 0, 1, 0b11001
      | Rounding_mode.X -> 1, 0, 0b11001
    in
    let size = (size_hi lsl 1) lor sz in
    encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRSQRTE_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FRSQRTE: U=1, size=1x, opcode=11101 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz) ~opcode:0b11101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FSQRT ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_1_source ~ftype ~opcode:0b000011 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FSQRT_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FSQRT: U=1, size=1x, opcode=11111 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz) ~opcode:0b11111 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FSUB ) ->
    let ftype = scalar_ftype scalar in
    encode_fp_2_source ~ftype ~rm ~opcode:0b0011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FSUB_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* FSUB: U=0, size=1x, opcode=11010 *)
    encode_simd_three_same ~q ~u:0 ~size:(0b10 lor sz) ~rm ~opcode:0b11010 ~rn
      ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { reg_name = GP _; index = rn } ),
      INS lane_idx ) ->
    let imm5 = simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx) in
    (* INS (general): Q=1, op=0, imm4=0011 *)
    encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0011 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { reg_name = Neon (Scalar _); index = rn } ),
      INS lane_idx ) ->
    (* INS from scalar D register - same as from GP X register *)
    let imm5 = simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx) in
    (* INS (general): Q=1, op=0, imm4=0011 *)
    encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0011 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      INS_V lanes ) ->
    let dest_idx =
      Neon_reg_name.Lane_index.to_int
        (Neon_reg_name.Lane_index.Src_and_dest.dest_index lanes)
    in
    let src_idx =
      Neon_reg_name.Lane_index.to_int
        (Neon_reg_name.Lane_index.Src_and_dest.src_index lanes)
    in
    let imm5 = simd_copy_imm5 vec dest_idx in
    (* INS (element): Q=1, op=1, imm4 encodes source index *)
    let imm4 = src_idx in
    encode_simd_copy ~q:1 ~op:1 ~imm5 ~imm4 ~rn ~rd
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem (Reg rn)), LDAR ->
    encode_load_acquire ~rd ~rn
  | Triple (Reg rt1, Reg rt2, Mem addressing), LDP ->
    encode_load_store_pair_gp ~instr_name:"LDP" ~l:1 ~rt1 ~rt2 addressing
  | Pair (Reg rd, Mem addressing), LDR ->
    encode_load_store_gp state ~instr_name:"LDR" ~opc:0b01 ~rd addressing
  | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as rd), Mem addressing),
      LDR_simd_and_fp ) ->
    encode_load_store_simd_fp state ~instr_name:"LDR" ~is_load:true ~rd
      addressing
  | Pair (Reg rd, Mem addressing), LDRB ->
    (* LDRB: size=00, opc=01 *)
    encode_load_store_byte state ~instr_name:"LDRB" ~opc:0b01 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRH ->
    (* LDRH: size=01, opc=01 *)
    encode_load_store_halfword state ~instr_name:"LDRH" ~opc:0b01 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRSB ->
    (* LDRSB (sign-extend byte to 64-bit): size=00, opc=10 *)
    encode_load_store_byte state ~instr_name:"LDRSB" ~opc:0b10 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRSH ->
    (* LDRSH (sign-extend halfword to 64-bit): size=01, opc=10 *)
    encode_load_store_halfword state ~instr_name:"LDRSH" ~opc:0b10 ~rd
      addressing
  | Pair (Reg rd, Mem addressing), LDRSW ->
    (* LDRSW (sign-extend word to 64-bit): size=10, opc=10 *)
    encode_load_store_gp_sized state ~instr_name:"LDRSW" ~size:0b10 ~opc:0b10
      ~rd addressing
  | Triple (Reg rd, Reg rn, Reg rm), LSLV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001000 ~rm ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), LSRV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001001 ~rm ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MADD ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:0 ~rm ~ra ~rn ~rd
  | ( Pair (Reg { reg_name = Neon (Vector vec); index = rd }, Imm (Twelve imm)),
      MOVI ) ->
    let q, size = vector_q_size vec in
    (* MOVI encoding depends on element size:
       - For byte elements (size=00): op=0, cmode=1110, byte replication
       - For 64-bit elements (size=11): op=1, cmode=1110, 64-bit immediate
       For zeroing, imm=0, abc=000, defgh=00000 *)
    let imm8 = imm land 0xFF in
    let abc = (imm8 lsr 5) land 0b111 in
    let defgh = imm8 land 0b11111 in
    let op = if size = 0b11 then 1 else 0 in
    encode_simd_modified_imm ~q ~op ~abc ~cmode:0b1110 ~defgh ~rd
  | ( Pair (Reg { reg_name = Neon (Scalar _); index = rd }, Imm (Twelve imm)),
      MOVI ) ->
    (* MOVI to scalar (D register): Q=0, op=1, cmode=1110 for 64-bit immediate *)
    let imm8 = imm land 0xFF in
    let abc = (imm8 lsr 5) land 0b111 in
    let defgh = imm8 land 0b11111 in
    encode_simd_modified_imm ~q:0 ~op:1 ~abc ~cmode:0b1110 ~defgh ~rd
  | Triple (Reg rd, Imm imm, Shift shift), MOVK ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = (match shift.amount with Six n -> n) / 16 in
    encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVN ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = encode_six_bit_shift shift_opt in
    encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVZ ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = encode_six_bit_shift shift_opt in
    encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MSUB ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:1 ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      MUL_vector ) ->
    let q, size = vector_q_size vec in
    (* MUL: U=0, opcode=10011 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10011 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      MVN_vector ) ->
    let q, _ = vector_q_size vec in
    (* NOT/MVN: U=1, size=00, opcode=00101 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size:0b00 ~opcode:0b00101 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      NEG_vector ) ->
    let q, size = vector_q_size vec in
    (* NEG: U=1, opcode=01011 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size ~opcode:0b01011 ~rn ~rd
  | _, NOP -> encode_nop ()
  | Triple (Reg rd, Reg rn, Bitmask bitmask), ORR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b01 ~n ~immr ~imms ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      ORR_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_logical_shifted_reg ~opc:0b01 ~shift ~imm6 ~rd ~rn ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ORR_vector ) ->
    let q, _ = vector_q_size vec in
    (* ORR: U=0, size=10, opcode=00011 *)
    encode_simd_three_same ~q ~u:0 ~size:0b10 ~rm ~opcode:0b00011 ~rn ~rd
  | Pair (Reg rd, Reg rn), RBIT ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000000 ~rn ~rd
  | _, RET ->
    (* RET defaults to X30 (LR). Encoding is same as BR/BLR but with opc=0010
       and Rn=11111 (X30) encoded in bits 9:5 *)
    let open Int32 in
    let result = zero in
    let result = logor result (shift_left (of_int 0b1101011) 25) in
    let result = logor result (shift_left (of_int 0b0010) 21) in
    (* opc = 0010 *)
    let result = logor result (shift_left (of_int 0b11111) 16) in
    (* op2 = 11111 *)
    let result = logor result (shift_left (of_int 0b000000) 10) in
    (* op3 = 000000 *)
    let result = logor result (shift_left (of_int 30) 5) in
    (* Rn = X30 = 11110 *)
    let result = logor result (of_int 0b00000) in
    (* op4 = 00000 *)
    result
  | Pair (Reg rd, Reg rn), REV ->
    let sf = Reg.gp_sf rd in
    let opcode = if sf = 1 then 0b000011 else 0b000010 in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode ~rn ~rd
  | Pair (Reg rd, Reg rn), REV16 ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000001 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), SBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    encode_bitfield ~sf ~opc:0b00 ~n ~immr ~imms ~rn ~rd
  (* SCVTF: signed integer to FP conversion *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { reg_name = GP X; index = rn } ),
      SCVTF ) ->
    let ftype = scalar_ftype scalar in
    (* sf=1 (64-bit int), rmode=00, opcode=010 *)
    encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b00 ~opcode:0b010 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SCVTF_vector ) ->
    let q, sz = vector_q_fp_sz vec in
    (* SCVTF (vector, integer): U=0, size=0x, opcode=11101 *)
    encode_simd_two_reg_misc ~q ~u:0 ~size:sz ~opcode:0b11101 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), SDIV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b000011 ~rm ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      SHL ) ->
    let q, _ = vector_q_size vec in
    let immh, immb = shl_immh_immb vec shift in
    (* SHL: U=0, opcode=01010 *)
    encode_simd_shift_imm ~q ~u:0 ~immh ~immb ~opcode:0b01010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMAX_vector ) ->
    let q, size = vector_q_size vec in
    (* SMAX: U=0, opcode=01100 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMIN_vector ) ->
    let q, size = vector_q_size vec in
    (* SMIN: U=0, opcode=01101 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = GP gp; index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      SMOV lane_idx ) ->
    let q = match gp with W -> 0 | X -> 1 in
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = simd_copy_imm5 vec lane_int in
    (* SMOV: op=0, imm4=0101 *)
    encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      SMOV lane_idx ) ->
    (* SMOV to scalar D - 64-bit, so Q=1 *)
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = simd_copy_imm5 vec lane_int in
    encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0101 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), SMULH ->
    (* SMULH is 64-bit only. Ra is encoded as 11111 (ignored for multiply-high) *)
    (* Encoding: sf=1 op54=00 11011 op31=010 Rm o0=0 Ra=11111 Rn Rd *)
    let ra = Arm64_ast.Reg.xzr () in
    encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b010 ~o0:0 ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMULL2_vector ) ->
    let size = vector_widening_size vec in
    (* SMULL2: U=0, opcode=1100, Q=1 for "2" variant *)
    encode_simd_three_different ~q:1 ~u:0 ~size ~rm ~opcode:0b1100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMULL_vector ) ->
    let size = vector_widening_size vec in
    (* SMULL: U=0, opcode=1100, Q=0 for basic variant *)
    encode_simd_three_different ~q:0 ~u:0 ~size ~rm ~opcode:0b1100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SQADD_vector ) ->
    let q, size = vector_q_size vec in
    (* SQADD: U=0, opcode=00001 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b00001 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SQXTN ) ->
    let _, size = vector_q_size vec in
    (* SQXTN: U=0, opcode=10100, Q=0 for SQXTN *)
    encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10100 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SQXTN2 ) ->
    let _, size = vector_q_size vec in
    (* SQXTN2: U=0, opcode=10100, Q=1 for SQXTN2 *)
    encode_simd_two_reg_misc ~q:1 ~u:0 ~size ~opcode:0b10100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SQSUB_vector ) ->
    let q, size = vector_q_size vec in
    (* SQSUB: U=0, opcode=00101 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b00101 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SSHL_vector ) ->
    let q, size = vector_q_size vec in
    (* SSHL: U=0, opcode=01000 *)
    encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01000 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      SSHR ) ->
    let q, _ = vector_q_size vec in
    let immh, immb = shr_immh_immb vec shift in
    (* SSHR: U=0, opcode=00000 *)
    encode_simd_shift_imm ~q ~u:0 ~immh ~immb ~opcode:0b00000 ~rn ~rd
  | Triple (Reg rt1, Reg rt2, Mem addressing), STP ->
    encode_load_store_pair_gp ~instr_name:"STP" ~l:0 ~rt1 ~rt2 addressing
  | Pair (Reg rd, Mem addressing), STR ->
    encode_load_store_gp state ~instr_name:"STR" ~opc:0b00 ~rd addressing
  | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as rd), Mem addressing),
      STR_simd_and_fp ) ->
    encode_load_store_simd_fp state ~instr_name:"STR" ~is_load:false ~rd
      addressing
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem addressing), STRB ->
    (* STRB: size=00, opc=00 *)
    encode_load_store_byte state ~instr_name:"STRB" ~opc:0b00 ~rd addressing
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem addressing), STRH ->
    (* STRH: size=01, opc=00 *)
    encode_load_store_halfword state ~instr_name:"STRH" ~opc:0b00 ~rd addressing
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUB_immediate ->
    encode_add_sub_imm_auto_shift ~op:1 ~s:0 ~imm12 ~shift_opt:shift ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      SUB_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_add_sub_shifted_reg ~op:1 ~s:0 ~shift ~imm6 ~rd ~rn ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SUB_vector ) ->
    let q, size = vector_q_size vec in
    (* SUB: U=1, opcode=10000 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b10000 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUBS_immediate ->
    encode_add_sub_imm_auto_shift ~op:1 ~s:1 ~imm12 ~shift_opt:shift ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      SUBS_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        decode_shift_kind_int kind, decode_shift_amount_six amount
    in
    encode_add_sub_shifted_reg ~op:1 ~s:1 ~shift ~imm6 ~rd ~rn ~rm
  (* TODO: SXTL is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SXTL ) ->
    let immh = sxtl_immh vec in
    (* SXTL is alias for SSHLL with shift=0: U=0, opcode=10100, Q=0 *)
    encode_simd_shift_imm ~q:0 ~u:0 ~immh ~immb:0 ~opcode:0b10100 ~rn ~rd
  | ( Triple (Reg ({ reg_name = GP _; _ } as rt), Imm (Six bit), Imm (Sym sym)),
      TBNZ ) ->
    let imm14 = compute_branch_imm14 state ~instr_name:"TBNZ" sym in
    let b5 = (bit lsr 5) land 1 in
    let b40 = bit land 0b11111 in
    let rt_enc = Reg.gp_encoding rt in
    encode_test_branch ~b5 ~op:1 ~b40 ~imm14 ~rt:rt_enc
  | ( Triple (Reg ({ reg_name = GP _; _ } as rt), Imm (Six bit), Imm (Sym sym)),
      TBZ ) ->
    let imm14 = compute_branch_imm14 state ~instr_name:"TBZ" sym in
    let b5 = (bit lsr 5) land 1 in
    let b40 = bit land 0b11111 in
    let rt_enc = Reg.gp_encoding rt in
    encode_test_branch ~b5 ~op:0 ~b40 ~imm14 ~rt:rt_enc
  (* TODO: TST is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | Pair (Reg ({ reg_name = GP _; _ } as rn), Bitmask bitmask), TST ->
    (* TST is an alias for ANDS with XZR/WZR as destination (rd=31) *)
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    let rn_enc = Reg.gp_encoding rn in
    let open Int32 in
    let result = zero in
    (* sf=1 for 64-bit, opc=11 for ANDS *)
    let result = logor result (shift_left (of_int 1) 31) in
    let result = logor result (shift_left (of_int 0b11) 29) in
    let result = logor result (shift_left (of_int 0b100100) 23) in
    let result = logor result (shift_left (of_int n) 22) in
    let result = logor result (shift_left (of_int immr) 16) in
    let result = logor result (shift_left (of_int imms) 10) in
    let result = logor result (shift_left (of_int rn_enc) 5) in
    let result = logor result (of_int 31) in
    result
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), UBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    encode_bitfield ~sf ~opc:0b10 ~n ~immr ~imms ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UADDLP_vector ) ->
    let q, size = vector_q_size vec in
    (* UADDLP: U=1, opcode=00010 *)
    encode_simd_two_reg_misc ~q ~u:1 ~size ~opcode:0b00010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMAX_vector ) ->
    let q, size = vector_q_size vec in
    (* UMAX: U=1, opcode=01100 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMIN_vector ) ->
    let q, size = vector_q_size vec in
    (* UMIN: U=1, opcode=01101 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = GP gp; index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      UMOV lane_idx ) ->
    let q = match gp with W -> 0 | X -> 1 in
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = simd_copy_imm5 vec lane_int in
    (* UMOV: op=0, imm4=0111 *)
    encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0111 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      UMOV lane_idx ) ->
    (* UMOV to scalar D - 64-bit, so Q=1 *)
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = simd_copy_imm5 vec lane_int in
    encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0111 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), UMULH ->
    (* UMULH is 64-bit only. Ra is encoded as 11111 (ignored for multiply-high) *)
    (* Encoding: sf=1 op54=00 11011 op31=110 Rm o0=0 Ra=11111 Rn Rd *)
    let ra = Arm64_ast.Reg.xzr () in
    encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b110 ~o0:0 ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMULL2_vector ) ->
    let size = vector_widening_size vec in
    (* UMULL2: U=1, opcode=1100, Q=1 for "2" variant *)
    encode_simd_three_different ~q:1 ~u:1 ~size ~rm ~opcode:0b1100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMULL_vector ) ->
    let size = vector_widening_size vec in
    (* UMULL: U=1, opcode=1100, Q=0 for basic variant *)
    encode_simd_three_different ~q:0 ~u:1 ~size ~rm ~opcode:0b1100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UQADD_vector ) ->
    let q, size = vector_q_size vec in
    (* UQADD: U=1, opcode=00001 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b00001 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UQXTN ) ->
    let _, size = vector_q_size vec in
    (* UQXTN: U=1, opcode=10100, Q=0 for UQXTN *)
    encode_simd_two_reg_misc ~q:0 ~u:1 ~size ~opcode:0b10100 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UQXTN2 ) ->
    let _, size = vector_q_size vec in
    (* UQXTN2: U=1, opcode=10100, Q=1 for UQXTN2 *)
    encode_simd_two_reg_misc ~q:1 ~u:1 ~size ~opcode:0b10100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UQSUB_vector ) ->
    let q, size = vector_q_size vec in
    (* UQSUB: U=1, opcode=00101 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b00101 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      USHL_vector ) ->
    let q, size = vector_q_size vec in
    (* USHL: U=1, opcode=01000 *)
    encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01000 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      USHR ) ->
    let q, _ = vector_q_size vec in
    let immh, immb = shr_immh_immb vec shift in
    (* USHR: U=1, opcode=00000 *)
    encode_simd_shift_imm ~q ~u:1 ~immh ~immb ~opcode:0b00000 ~rn ~rd
  (* TODO: UXTL is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UXTL ) ->
    let immh = sxtl_immh vec in
    (* UXTL is alias for USHLL with shift=0: U=1, opcode=10100, Q=0 *)
    encode_simd_shift_imm ~q:0 ~u:1 ~immh ~immb:0 ~opcode:0b10100 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      XTN ) ->
    let q, size = vector_q_size vec in
    (* XTN: U=0, opcode=10010, Q=0 for XTN *)
    encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10010 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      XTN2 ) ->
    let _, size = vector_q_size vec in
    (* XTN2: U=0, opcode=10010, Q=1 for XTN2 *)
    encode_simd_two_reg_misc ~q:1 ~u:0 ~size ~opcode:0b10010 ~rn ~rd
  | _, YIELD -> encode_yield ()
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ZIP1 ) ->
    let q, size = vector_q_size vec in
    (* ZIP1: opcode=011 *)
    encode_simd_permute ~q ~size ~rm ~opcode:0b011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ZIP2 ) ->
    let q, size = vector_q_size vec in
    (* ZIP2: opcode=111 *)
    encode_simd_permute ~q ~size ~rm ~opcode:0b111 ~rn ~rd

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

let eval_constant state ~current_section_base ~global_lookup c =
  (* For cross-section references, we use "absolute" offsets (section_base +
     offset_in_section) so that relative expressions like (Label - This) work
     correctly even when Label and This are in different sections. *)
  let this () =
    Int64.of_int (current_section_base + Section_state.offset_in_bytes state)
  in
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
      (* First try current section, then use global lookup for cross-section
         refs. For same-section lookups, add the current section base to get
         an "absolute" offset. *)
      match Section_state.find_label_offset_in_bytes state name with
      | Some offset -> Some (Int64.of_int (current_section_base + offset))
      | None -> (
        match Section_state.find_symbol_offset_in_bytes state name with
        | Some offset -> Some (Int64.of_int (current_section_base + offset))
        | None -> global_lookup name)
  in
  D.Directive.Constant.eval ~this ~lookup c

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
  let get_current_section_base () = section_base !current_section in
  (* For cross-section (Label - This) expressions, we need to emit a relocation
     pair (SUBTRACTOR + UNSIGNED) and store the addend. The linker will compute:
       final_value = plus_symbol_addr - minus_symbol_addr + addend
     We create symbols at the exact positions needed so the addend is just
     the offset_upper from the original expression. *)
  iter emitter ~state_for_section
    ~on_insn:(fun state (Instruction.I { name; operands }) ->
      let encoded = encode_instruction state name operands in
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
    ~on_directive:(fun state directive ->
      let buf = Section_state.buffer state in
      (* Update current section when we see a Section directive *)
      (match directive with
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
            for _ = 1 to nop_count do
              (* ARM64 NOP: 0xD503201F in little-endian *)
              Buffer.add_char buf '\x1f';
              Buffer.add_char buf '\x20';
              Buffer.add_char buf '\x03';
              Buffer.add_char buf '\xd5'
            done;
            for _ = 1 to zero_count do
              Buffer.add_char buf '\x00'
            done
          | D.Zero ->
            for _ = 1 to padding do
              Buffer.add_char buf '\x00'
            done)
      | Const { constant; _ } -> (
        let module C = D.Directive.Constant_with_width in
        let module Const = D.Directive.Constant in
        let c = C.constant constant in
        let width = C.width_in_bytes constant in
        let width_bytes = C.width_in_bytes_int width in
        let current_section_base = get_current_section_base () in
        (* Check for cross-section (Label - This) + offset pattern.
           This occurs in frametable entries where a DATA section location
           references a TEXT section label (return address). *)
        let try_cross_section_label_rel () =
          (* Pattern: Add(Sub(Named_thing label, This), offset) or
                      Sub(Named_thing label, This) *)
          let extract_label_this_offset = function
            | Const.Add (Const.Sub (Const.Named_thing name, Const.This),
                         Const.Signed_int offset) ->
              Some (name, offset)
            | Const.Sub (Const.Named_thing name, Const.This) ->
              Some (name, 0L)
            | _ -> None
          in
          match extract_label_this_offset c with
          | None -> None
          | Some (label_name, offset_upper) ->
            (* Check if this is a cross-section reference *)
            if not (Asm_section.equal !current_section Asm_section.Data)
            then None  (* Only handle DATA section for now *)
            else
              (* First check if label is in current section *)
              match Section_state.find_label_offset_in_bytes state label_name with
              | Some _ -> None  (* Same section, use normal eval *)
              | None ->
                (* Try cross-section lookup *)
                match global_lookup_with_section label_name with
                | None -> None  (* Not found at all *)
                | Some (_, label_section, _) ->
                  if Asm_section.equal label_section !current_section
                  then None  (* Same section after all *)
                  else if Asm_section.equal label_section Asm_section.Text
                  then begin
                    (* Cross-section: TEXT label referenced from DATA.
                       We need to emit a PREL32_PAIR relocation.
                       Use existing global symbols (matching assembler behavior):
                       - minus_symbol (SUBTRACTOR): nearest global symbol in DATA
                       - plus_symbol (UNSIGNED): nearest global symbol in TEXT
                       The linker computes: plus_sym - minus_sym + addend
                       So addend = (target - plus_sym) - (current - minus_sym) *)
                    let current_pos = Section_state.offset_in_bytes state in
                    (* Find nearest symbol in DATA for SUBTRACTOR *)
                    (match Section_state.find_nearest_symbol_at_or_before state
                             current_pos with
                    | None -> None  (* No symbol in DATA to use *)
                    | Some (minus_symbol, minus_sym_offset) ->
                      (* Find nearest symbol in TEXT for UNSIGNED *)
                      let text_state = Asm_section.Tbl.find section_tbl
                          Asm_section.Text in
                      (* Get target label offset in TEXT *)
                      (match Section_state.find_label_offset_in_bytes text_state
                               label_name with
                      | None -> None
                      | Some target_offset ->
                        (match Section_state.find_nearest_symbol_at_or_before
                                 text_state target_offset with
                        | None -> None  (* No symbol in TEXT to use *)
                        | Some (plus_symbol, plus_sym_offset) ->
                          let addend =
                            Int64.add offset_upper
                              (Int64.sub
                                (Int64.of_int (target_offset - plus_sym_offset))
                                (Int64.of_int (current_pos - minus_sym_offset)))
                          in
                          Section_state.add_relocation_at_current_offset state
                            ~symbol_name:plus_symbol
                            ~reloc_kind:(Relocation.Kind.R_AARCH64_PREL32_PAIR
                              { plus_symbol; minus_symbol });
                          Some addend)))
                  end
                  else None  (* Other cross-section cases not handled *)
        in
        (* Check for absolute cross-section symbol reference.
           For .8byte symbol where symbol is in a different section,
           we must emit a relocation. *)
        let try_cross_section_absolute () =
          match c with
          | Const.Named_thing name when width_bytes = 8 ->
            (* Check if symbol is in a different section *)
            (match Section_state.find_label_offset_in_bytes state name with
            | Some _ -> None  (* Same section, can resolve *)
            | None ->
              match Section_state.find_symbol_offset_in_bytes state name with
              | Some _ -> None  (* Same section symbol *)
              | None ->
                (* Try cross-section lookup *)
                match global_lookup_with_section name with
                | None -> None  (* Not found, will fall through to relocation *)
                | Some (_, sym_section, _) ->
                  if Asm_section.equal sym_section !current_section
                  then None  (* Same section *)
                  else begin
                    (* Cross-section absolute reference - needs relocation *)
                    Section_state.add_relocation_at_current_offset state
                      ~symbol_name:name
                      ~reloc_kind:(Relocation.Kind.R_AARCH64_ABS64 name);
                    Some 0L  (* Emit zero, relocation will patch *)
                  end)
          | _ -> None
        in
        let value_opt =
          match try_cross_section_label_rel () with
          | Some addend -> Some addend
          | None ->
            match try_cross_section_absolute () with
            | Some v -> Some v
            | None -> eval_constant state ~current_section_base ~global_lookup c
        in
        match value_opt with
        | Some value -> D.Directive.emit_int_le buf ~width_bytes value
        | None ->
          (* External/global reference - emit zeros and record relocation *)
          (* Extract the symbol name from the constant for the relocation *)
          let rec extract_symbol_name = function
            | Const.Named_thing name -> Some name
            | Const.Add (a, _) -> extract_symbol_name a
            | Const.Sub (a, _) -> extract_symbol_name a
            | Const.Signed_int _ | Const.Unsigned_int _ | Const.This -> None
          in
          (match extract_symbol_name c with
          | Some symbol_name when width_bytes = 8 ->
            Section_state.add_relocation_at_current_offset state ~symbol_name
              ~reloc_kind:(Relocation.Kind.R_AARCH64_ABS64 symbol_name)
          | _ -> ());
          for _ = 1 to width_bytes do
            Buffer.add_char buf '\x00'
          done)
      | Sleb128 { constant; _ } -> (
        let current_section_base = get_current_section_base () in
        match eval_constant state ~current_section_base ~global_lookup constant with
        | Some value -> D.Directive.emit_sleb128 buf value
        | None -> Misc.fatal_error "Cannot emit SLEB128 for external symbol")
      | Uleb128 { constant; _ } -> (
        let current_section_base = get_current_section_base () in
        match eval_constant state ~current_section_base ~global_lookup constant with
        | Some value -> D.Directive.emit_uleb128 buf value
        | None -> Misc.fatal_error "Cannot emit ULEB128 for external symbol")
      (* Directives that don't emit data *)
      | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_endproc
      | Cfi_offset _ | Cfi_startproc | Cfi_remember_state | Cfi_restore_state
      | Cfi_def_cfa_register _ | Comment _ | Direct_assignment _ | File _
      | Global _ | Indirect_symbol _ | Loc _ | New_label _ | New_line
      | Private_extern _ | Section _ | Size _ | Type _ | Protected _ | Hidden _
      | Weak _ | External _ | Reloc _ ->
        ())

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
  (* Compute section sizes (final offset after first pass) and bases.
     Sections are laid out contiguously: TEXT at 0, DATA after TEXT, etc. *)
  let section_sizes = Asm_section.Tbl.create 10 in
  Asm_section.Tbl.iter
    (fun section state ->
      Asm_section.Tbl.add section_sizes section
        (Section_state.offset_in_bytes state))
    section_tbl;
  (* Compute section bases. We use a simple layout: TEXT at 0, DATA after TEXT,
     other sections after DATA. The actual layout doesn't matter for correctness
     as long as it's consistent - we just need relative positions to work out. *)
  let text_size =
    Option.value ~default:0 (Asm_section.Tbl.find_opt section_sizes Asm_section.Text)
  in
  let data_size =
    Option.value ~default:0 (Asm_section.Tbl.find_opt section_sizes Asm_section.Data)
  in
  let section_base section =
    match section with
    | Asm_section.Text -> 0
    | Asm_section.Data -> text_size
    | _ -> text_size + data_size (* Other sections after data *)
  in
  (* Global lookup for cross-section references.
     Returns (offset_in_section, section, section_base) if found.
     For local labels (which start with L on macOS), we can resolve cross-section
     references at assembly time because the relative positions are fixed within
     the object file. For global/external symbols in PIC mode, we return None to
     trigger the relocation path. *)
  let global_lookup_with_section name =
    (* Search all sections for this label *)
    let result = ref None in
    Asm_section.Tbl.iter
      (fun section state ->
        if Option.is_none !result
        then
          match Section_state.find_label_offset_in_bytes state name with
          | Some offset ->
            result := Some (offset, section, section_base section)
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

(* For_jit module implementing Binary_emitter.S *)
module For_jit = struct
  module Relocation = struct
    type t = Relocation.t

    let offset_from_section_beginning (r : Relocation.t) =
      r.offset_from_section_beginning

    (* ARM64 code relocations are 32-bit patches within 32-bit instructions, but
       data relocations (ABS64) are 64-bit *)
    let size (r : t) : Binary_emitter.data_size =
      match r.kind with
      | R_AARCH64_ABS64 _ -> Binary_emitter.B64
      | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
      | R_AARCH64_LD64_GOT_LO12_NC _ | R_AARCH64_ADD_ABS_LO12_NC _
      | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _
      | R_AARCH64_PREL32_PAIR _ ->
        Binary_emitter.B32

    let target_symbol (r : Relocation.t) : string =
      match r.kind with
      | R_AARCH64_ADR_PREL_LO21 sym
      | R_AARCH64_ADR_PREL_PG_HI21 sym
      | R_AARCH64_LD64_GOT_LO12_NC sym
      | R_AARCH64_ADD_ABS_LO12_NC sym
      | R_AARCH64_CALL26 sym
      | R_AARCH64_JUMP26 sym
      | R_AARCH64_ABS64 sym ->
        sym
      | R_AARCH64_PREL32_PAIR { plus_symbol; _ } ->
        plus_symbol  (* Return the plus symbol as the primary target *)

    let is_got_reloc (r : Relocation.t) =
      match r.kind with
      | R_AARCH64_LD64_GOT_LO12_NC _ -> true
      | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
      | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _
      | R_AARCH64_ABS64 _ | R_AARCH64_PREL32_PAIR _ ->
        false

    let is_plt_reloc (_ : t) = false (* ARM64 doesn't use PLT in same way *)

    let compute_value (r : Relocation.t) ~place_address ~lookup_symbol =
      let sym = target_symbol r in
      match lookup_symbol sym with
      | None -> Error (Printf.sprintf "Symbol not found: %s" sym)
      | Some target_addr -> (
        match r.kind with
        | R_AARCH64_ADR_PREL_LO21 _ ->
          (* PC-relative offset for ADR instruction, low 21 bits *)
          let offset = Int64.sub target_addr place_address in
          Ok offset
        | R_AARCH64_ADR_PREL_PG_HI21 _ ->
          (* Page-relative offset for ADRP instruction Result = Page(target) -
             Page(place) *)
          let page_mask = Int64.lognot 0xFFF_L in
          let target_page = Int64.logand target_addr page_mask in
          let place_page = Int64.logand place_address page_mask in
          let offset = Int64.sub target_page place_page in
          Ok offset
        | R_AARCH64_ADD_ABS_LO12_NC _ ->
          (* Lower 12 bits of absolute address *)
          let low12 = Int64.logand target_addr 0xFFF_L in
          Ok low12
        | R_AARCH64_LD64_GOT_LO12_NC _ ->
          (* Lower 12 bits of GOT entry address, scaled by 8 *)
          let low12 = Int64.logand target_addr 0xFFF_L in
          Ok low12
        | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ ->
          (* PC-relative offset for B/BL instructions, divided by 4 *)
          let offset = Int64.sub target_addr place_address in
          Ok offset
        | R_AARCH64_ABS64 _ ->
          (* Absolute 64-bit address *)
          Ok target_addr
        | R_AARCH64_PREL32_PAIR { plus_symbol; minus_symbol } ->
          (* Cross-section relative: addend + plus_symbol - minus_symbol *)
          match lookup_symbol minus_symbol with
          | None -> Error (Printf.sprintf "Minus symbol not found: %s" minus_symbol)
          | Some minus_addr ->
            let _ = plus_symbol in
            (* target_addr is plus_symbol's address *)
            Ok (Int64.sub target_addr minus_addr))
  end

  module Assembled_section = struct
    type t = Section_state.t

    type relocation = Relocation.t

    let size t = Buffer.length (Section_state.buffer t)

    let contents t = Section_state.contents t

    let contents_mut t = Section_state.contents_mut t

    let relocations t = Section_state.relocations t

    let find_symbol_offset t name =
      Section_state.find_symbol_offset_in_bytes t name

    let find_label_offset t name =
      Section_state.find_label_offset_in_bytes t name

    let iter_symbols t ~f =
      Hashtbl.iter
        (fun name offset -> f ~name ~offset)
        (Section_state.symbols t)

    let add_patch t ~offset ~size:(sz : Binary_emitter.data_size) ~data =
      let sz =
        match sz with B8 -> P8 | B16 -> P16 | B32 -> P32 | B64 -> P64
      in
      Section_state.add_patch t ~offset ~size:sz ~data
  end

  module Plt = struct
    (* ARM64 PLT entry: ldr x16, .+8 ; 58000050 - load address from next 8 bytes
       br x16 ; d61f0200 - branch to x16 .quad <address> ; 8 bytes of address
       Total: 16 bytes *)
    let entry_size = 16

    let write_entry buf address =
      (* ldr x16, .+8 - PC-relative load from 8 bytes ahead *)
      Buffer.add_char buf '\x50';
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\x58';
      (* br x16 - branch to register *)
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\x02';
      Buffer.add_char buf '\x1f';
      Buffer.add_char buf '\xd6';
      (* 8-byte address (little-endian) *)
      for i = 0 to 7 do
        let byte =
          Int64.(to_int (logand (shift_right_logical address (i * 8)) 0xFFL))
        in
        Buffer.add_char buf (Char.chr byte)
      done
  end

  module Internal_assembler = struct
    type assembled_section = Assembled_section.t

    type hook = (string * assembled_section) list -> string -> unit

    let current_hook : hook option ref = ref None

    let register h = current_hook := Some h

    let unregister () = current_hook := None

    let get () = !current_hook
  end
end
