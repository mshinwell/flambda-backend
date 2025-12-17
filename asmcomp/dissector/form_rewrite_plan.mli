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

(** Information about an original symbol from the ELF symbol table. *)
type symbol_entry

val symbol_name : symbol_entry -> string

val symbol_st_info : symbol_entry -> int

val symbol_st_other : symbol_entry -> int

val symbol_st_shndx : symbol_entry -> int

val symbol_st_value : symbol_entry -> int64

val symbol_st_size : symbol_entry -> int64

(** Layout of a section in the output file. *)
type section_layout

val layout_offset : section_layout -> int

val layout_size : section_layout -> int

(** Layout of all sections in the output file. *)
type layout

val layout_igot : layout -> section_layout

val layout_rela_igot : layout -> section_layout

val layout_iplt : layout -> section_layout

val layout_rela_iplt : layout -> section_layout

val layout_symtab : layout -> section_layout

val layout_strtab : layout -> section_layout

val layout_rela_text : layout -> section_layout

val layout_shstrtab : layout -> section_layout

val layout_section_headers_offset : layout -> int

val layout_total_size : layout -> int

(** A rewrite plan for an ELF file. *)
type t

val original_symbols : t -> symbol_entry array

val symbol_to_index : t -> (string, int) Hashtbl.t

val total_symbols : t -> int

val new_rela_text : t -> Compiler_owee.Owee_elf_relocation.rela_entry list

val strtab : t -> Compiler_owee.Owee_elf_string_table.t

val shstrtab : t -> Compiler_owee.Owee_elf_string_table.t

(** Maps original section names to their offsets in shstrtab. For Large_code
    partitions, the stored offset points to the renamed name (e.g.,
    .caml.p1.text instead of .text). *)
val section_name_offsets : t -> (string, int) Hashtbl.t

val igot_name_offset : t -> int

val rela_igot_name_offset : t -> int

val iplt_name_offset : t -> int

val rela_iplt_name_offset : t -> int

val igot_idx : t -> int

val rela_igot_idx : t -> int

val iplt_idx : t -> int

val rela_iplt_idx : t -> int

val num_sections : t -> int

val symtab_idx : t -> int

val layout : t -> layout

(** [compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_body
      ~partition_kind ~igot_and_iplt ~relocations] analyzes the ELF structure
    and builds a rewrite plan.

    For [Large_code] partitions, section names are renamed with a prefix
    (e.g., .text -> .caml.p1.text). *)
val compute :
  header:Compiler_owee.Owee_elf.header ->
  sections:Compiler_owee.Owee_elf.section array ->
  symtab_body:Compiler_owee.Owee_buf.t ->
  strtab_body:Compiler_owee.Owee_buf.t ->
  rela_text_body:Compiler_owee.Owee_buf.t ->
  partition_kind:Partition.kind ->
  igot_and_iplt:Build_igot_and_iplt.t ->
  relocations:Extract_relocations.t ->
  t
