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

module Kind : sig
  (** Relocation with symbol name and addend. On RELA platforms (Linux ELF),
      the addend is stored in the relocation entry. On REL platforms (macOS
      Mach-O), the addend is encoded in the instruction/data. *)
  type sym_with_addend =
    { symbol : string;
      addend : int
    }

  type t =
    | R_AARCH64_ADR_PREL_LO21 of sym_with_addend
    | R_AARCH64_ADR_PREL_PG_HI21 of sym_with_addend
    | R_AARCH64_ADR_GOT_PAGE of sym_with_addend
    | R_AARCH64_LD64_GOT_LO12_NC of sym_with_addend
    | R_AARCH64_ADD_ABS_LO12_NC of sym_with_addend
    | R_AARCH64_LDST64_ABS_LO12_NC of sym_with_addend
    | R_AARCH64_CALL26 of sym_with_addend
    | R_AARCH64_JUMP26 of sym_with_addend
    | R_AARCH64_ABS64 of sym_with_addend
    (* Cross-section relative reference (SUBTRACTOR + UNSIGNED pair on macOS)
       Used for expressions like (Label - This) where Label and This are in
       different sections. The addend is stored in the data, and the linker will
       compute: addend + plus_symbol - minus_symbol *)
    | R_AARCH64_PREL32_PAIR of
        { plus_symbol : string;
          minus_symbol : string
        }
    (* ELF section-relative 32-bit PC-relative reference. Used on ELF for
       cross-section references when function sections are enabled. The
       section_name is the target section (e.g., .text.caml.funcname) and addend
       is the offset within that section. The linker computes: section_address +
       addend - relocation_address *)
    | R_AARCH64_PREL32 of
        { section_name : string;
          addend : int
        }
end

type t =
  { offset_from_section_beginning : int;
    kind : Kind.t
  }

val offset_from_section_beginning : t -> int

val size : t -> Binary_emitter_intf.data_size

val target_symbol : t -> string

val target_symbols : t -> string list

val target_symbols_with_addends : t -> (string * int) list

val is_got_reloc : t -> bool

val is_plt_reloc : t -> bool

val get_addend : Kind.t -> int

val compute_value :
  t ->
  target_addr:int64 ->
  place_address:int64 ->
  read_instruction:(unit -> int32) ->
  lookup_symbol:(string -> int64 option) ->
  (int64, string) result
