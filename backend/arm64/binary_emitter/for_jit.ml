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

(* For_jit module implementing Binary_emitter_intf.S *)

module Relocation = struct
  type t = Relocation.t

  let offset_from_section_beginning (r : Relocation.t) =
    r.offset_from_section_beginning

  (* ARM64 code relocations are 32-bit patches within 32-bit instructions, but
     data relocations (ABS64) are 64-bit *)
  let size (r : t) : Binary_emitter_intf.data_size =
    match r.kind with
    | R_AARCH64_ABS64 _ -> Binary_emitter_intf.B64
    | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
    | R_AARCH64_LD64_GOT_LO12_NC _ | R_AARCH64_ADD_ABS_LO12_NC _
    | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ | R_AARCH64_PREL32_PAIR _ ->
      Binary_emitter_intf.B32

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
      plus_symbol (* Return the plus symbol as the primary target *)

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
      | R_AARCH64_PREL32_PAIR { plus_symbol; minus_symbol } -> (
        (* Cross-section relative: addend + plus_symbol - minus_symbol *)
        match lookup_symbol minus_symbol with
        | None ->
          Error (Printf.sprintf "Minus symbol not found: %s" minus_symbol)
        | Some minus_addr ->
          let _ = plus_symbol in
          (* target_addr is plus_symbol's address *)
          Ok (Int64.sub target_addr minus_addr)))
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

  let find_label_offset t name = Section_state.find_label_offset_in_bytes t name

  let iter_symbols t ~f =
    Hashtbl.iter (fun name offset -> f ~name ~offset) (Section_state.symbols t)

  let add_patch t ~offset ~size:(sz : Binary_emitter_intf.data_size) ~data =
    let sz =
      match sz with
      | B8 -> Section_state.P8
      | B16 -> Section_state.P16
      | B32 -> Section_state.P32
      | B64 -> Section_state.P64
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
