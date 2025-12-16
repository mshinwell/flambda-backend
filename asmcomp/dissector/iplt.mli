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

(** Intermediate PLT (Procedure Linkage Table) for the dissector.

    The intermediate PLT provides local PLT entries that are within range of
    PC-relative call/jump instructions from the code in a partition. Each entry
    contains a jump instruction through the corresponding IGOT entry.

    Each PLT entry is 8 bytes:

    - ff 25 XX XX XX XX  : jmp [rip + displacement] (6 bytes)

    - 90 90              : 2-byte nop padding (two single-byte nops)

    The 4-byte displacement at offset +2 is filled by a PC32 relocation
    pointing to the corresponding IGOT entry. *)

(** Size of each IPLT entry in bytes. *)
val entry_size : int

(** An entry in the intermediate PLT. *)
type entry = private
  { index : int;  (** Index of this entry (0-based). *)
    original_symbol : string;
        (** The original external symbol this PLT entry calls. *)
    iplt_symbol : string;  (** The synthetic symbol for this PLT entry. *)
    igot_symbol : string  (** The IGOT symbol this entry jumps through. *)
  }

(** A built intermediate PLT section. *)
type t

(** [build ~prefix ~igot symbols] builds an intermediate PLT from a list of
    symbols that need PLT entries.

    Each PLT entry requires a corresponding IGOT entry, which must exist in
    the provided [igot].

    @param prefix A unique prefix for this partition (e.g., "0", "1")
    @param igot The intermediate GOT (must contain entries for all symbols)
    @param symbols List of original symbol names needing PLT entries *)
val build : prefix:string -> igot:Igot.t -> symbols:string list -> t

(** Returns the list of entries in the IPLT. *)
val entries : t -> entry list

(** Returns the section data (machine code). *)
val section_data : t -> bytes

(** Returns the size of the section in bytes. *)
val section_size : t -> int

(** [find_entry t ~symbol] returns the entry for [symbol], or [None] if not
    found. *)
val find_entry : t -> symbol:string -> entry option

(** [iplt_symbol_name ~prefix ~symbol] returns the IPLT symbol name for the
    given original symbol. *)
val iplt_symbol_name : prefix:string -> symbol:string -> string

(** A relocation for an IPLT entry. *)
type relocation =
  { offset : int;  (** Offset within the IPLT section (entry_offset + 2). *)
    symbol : string;  (** The IGOT symbol to relocate to. *)
    addend : int64
        (** Relocation addend (-4 to account for RIP pointing past displacement). *)
  }

(** [relocations t] returns the list of R_X86_64_PC32 relocations needed to
    fill the IPLT entries with displacements to their IGOT entries. *)
val relocations : t -> relocation list

(** [entry_offset entry] returns the byte offset of the entry within the IPLT
    section. *)
val entry_offset : entry -> int
