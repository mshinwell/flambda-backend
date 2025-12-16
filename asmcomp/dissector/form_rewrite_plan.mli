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

(** Form a rewrite plan for ELF section rewriting.

    This module analyzes the input ELF file and builds a plan describing
    all the modifications needed: new sections, relocated symbols, and
    file layout. The plan can then be executed by [Rewrite_sections]. *)

type symbol_entry =
  { name : string;
    st_info : int;
    st_other : int;
    st_shndx : int;
    st_value : int64;
    st_size : int64
  }

type section_layout =
  { offset : int;
    size : int
  }

type layout =
  { igot : section_layout;
    rela_igot : section_layout;
    iplt : section_layout;
    rela_iplt : section_layout;
    symtab_layout : section_layout;
    strtab_layout : section_layout;
    rela_text : section_layout;
    shstrtab_layout : section_layout;
    section_headers_offset : int;
    total_size : int
  }

type t =
  { original_symbols : symbol_entry array;
    symbol_to_index : (string, int) Hashtbl.t;
    total_symbols : int;
    new_rela_text : Compiler_owee.Owee_elf_relocation.rela_entry list;
    strtab : Compiler_owee.Owee_elf_string_table.t;
    shstrtab : Compiler_owee.Owee_elf_string_table.t;
    igot_name_offset : int;
    rela_igot_name_offset : int;
    iplt_name_offset : int;
    rela_iplt_name_offset : int;
    igot_idx : int;
    rela_igot_idx : int;
    iplt_idx : int;
    rela_iplt_idx : int;
    num_sections : int;
    symtab_idx : int;
    layout : layout
  }

(** [compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_body
      ~igot_and_iplt ~relocations] analyzes the ELF structure and builds
    a rewrite plan. *)
val compute :
  header:Compiler_owee.Owee_elf.header ->
  sections:Compiler_owee.Owee_elf.section array ->
  symtab_body:Compiler_owee.Owee_buf.t ->
  strtab_body:Compiler_owee.Owee_buf.t ->
  rela_text_body:Compiler_owee.Owee_buf.t ->
  igot_and_iplt:Build_igot_and_iplt.t ->
  relocations:Extract_relocations.t ->
  t
