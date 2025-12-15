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

(* Each IPLT entry is 8 bytes: ff 25 XX XX XX XX - jmp [rip + disp32] (6 bytes)
   66 90 - 2-byte nop padding (operand size prefix + nop)

   The 4-byte displacement at offset +2 will be filled by a PC32 relocation. *)
let entry_size = 8

(* Delimiter for synthetic symbol names - same as IGOT *)
let delimiter = "\xe2\x9a\xa1" (* Unicode lightning bolt U+26A1 in UTF-8 *)

type entry =
  { index : int;
    original_symbol : string;
    iplt_symbol : string;
    igot_symbol : string
  }

type t =
  { entries : entry list;
    section_data : bytes
  }

let iplt_symbol_name ~prefix symbol =
  "iplt" ^ delimiter ^ prefix ^ delimiter ^ symbol

(* Build the PLT entry template using X86_binary_emitter. The JMP instruction
   is: jmp [rip + 0] which encodes as ff 25 00 00 00 00 We append a 2-byte nop
   (66 90) for padding to reach 8 bytes. *)
let plt_entry_template =
  (* Create a section with a single JMP instruction *)
  let jmp_instr = X86_ast.JMP (X86_ast.Mem64_RIP (X86_ast.QWORD, "dummy", 0)) in
  let section =
    { X86_binary_emitter.sec_name = ".text.iplt";
      sec_instrs = [| X86_ast.Ins jmp_instr |]
    }
  in
  let buffer = X86_binary_emitter.assemble_section X86_ast.X64 section in
  let jmp_bytes = X86_binary_emitter.contents buffer in
  (* The JMP should be 6 bytes, append 2-byte nop (66 90) for alignment *)
  assert (String.length jmp_bytes = 6);
  jmp_bytes ^ "\x66\x90"

let build ~prefix ~igot symbols =
  (* Remove duplicates while preserving order *)
  let seen = Hashtbl.create 16 in
  let unique_symbols =
    List.filter
      (fun sym ->
        if Hashtbl.mem seen sym
        then false
        else (
          Hashtbl.add seen sym ();
          true))
      symbols
  in
  let entries =
    List.mapi
      (fun index original_symbol ->
        let iplt_symbol = iplt_symbol_name ~prefix original_symbol in
        let igot_symbol = Igot.igot_symbol_name ~prefix original_symbol in
        (* Verify the IGOT entry exists *)
        (match Igot.find_entry igot original_symbol with
        | None ->
          Misc.fatal_errorf "IPLT: no IGOT entry for symbol %s" original_symbol
        | Some _ -> ());
        { index; original_symbol; iplt_symbol; igot_symbol })
      unique_symbols
  in
  (* Build section data *)
  let num_entries = List.length entries in
  let section_data = Bytes.make (num_entries * entry_size) '\x00' in
  for i = 0 to num_entries - 1 do
    Bytes.blit_string plt_entry_template 0 section_data (i * entry_size)
      entry_size
  done;
  { entries; section_data }

let entries t = t.entries

let section_data t = t.section_data

let section_size t = Bytes.length t.section_data

let find_entry t symbol =
  let rec find = function
    | [] -> None
    | entry :: rest ->
      if String.equal entry.original_symbol symbol
      then Some entry
      else find rest
  in
  find t.entries

type relocation =
  { offset : int;
    symbol : string;
    addend : int64
  }

let entry_offset entry = entry.index * entry_size

(* The displacement field is at offset +2 within each entry *)
let displacement_offset = 2

let relocations t =
  List.map
    (fun entry ->
      { offset = entry_offset entry + displacement_offset;
        symbol = entry.igot_symbol;
        addend = -4L
      })
    t.entries
