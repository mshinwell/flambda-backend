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

(** Extract relocations from partially-linked object files.

    This module reads ELF object files and extracts relocations that need
    to be converted to use an intermediate PLT or GOT when linking with
    the dissector code model. *)

(** Information about a single relocation that needs conversion. *)
type relocation_entry = private
  { symbol_name : string;
    offset : int64
  }

(** The result of extracting relocations from object files. *)
type t = private
  { convert_to_plt : relocation_entry list;
        (** Relocations with type R_X86_64_PLT32 that need PLT entries. *)
    convert_to_got : relocation_entry list
        (** Relocations with type R_X86_64_REX_GOTPCRELX that need GOT entries. *)
  }

(** [extract unix ~filename] reads the ELF object file at [filename] and
    extracts relocations from the .rela.text section that need to be
    converted for the medium code model.

    Returns the lists of PLT32 and REX_GOTPCRELX relocations found. *)
val extract : (module Compiler_owee.Unix_intf.S) -> filename:string -> t

(** [extract_from_linked_partitions unix linked_partitions] extracts
    relocations from all the partially-linked object files.

    Returns combined relocation information from all partitions. *)
val extract_from_linked_partitions :
  (module Compiler_owee.Unix_intf.S) -> Partition.linked list -> t
