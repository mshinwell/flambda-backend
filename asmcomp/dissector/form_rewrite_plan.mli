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
module Symbol_entry : sig
  type t

  val name : t -> string

  val st_info : t -> int

  val st_other : t -> int

  val st_shndx : t -> int

  val st_value : t -> int64

  val st_size : t -> int64
end

(** Layout of a section in the output file. *)
module Section_layout : sig
  type t

  val offset : t -> int

  val size : t -> int
end

(** Layout of all sections in the output file. *)
module Layout : sig
  type t

  val igot : t -> Section_layout.t

  val rela_igot : t -> Section_layout.t

  val iplt : t -> Section_layout.t

  val rela_iplt : t -> Section_layout.t

  val symtab : t -> Section_layout.t

  (** Layout of SYMTAB_SHNDX section, if the input file has one. *)
  val symtab_shndx : t -> Section_layout.t option

  val strtab : t -> Section_layout.t

  val shstrtab : t -> Section_layout.t

  val section_headers_offset : t -> int

  val total_size : t -> int
end

(** Rewritten relocation entries for a single .rela.text* section. *)
module Rewritten_rela_section : sig
  type t

  (** Original file offset of this section. *)
  val section_offset : t -> int64

  (** The rewritten relocation entries. *)
  val entries : t -> Compiler_owee.Owee_elf_relocation.rela_entry list
end

(** A rewrite plan for an ELF file. *)
type t

val original_symbols : t -> Symbol_entry.t array

val symbol_to_index : t -> (string, int) Hashtbl.t

val total_symbols : t -> int

(** The list of rewritten .rela.text* sections, each with their original
    file offset and rewritten entries. *)
val rewritten_rela_sections : t -> Rewritten_rela_section.t list

val strtab : t -> Compiler_owee.Owee_elf_string_table.t

val shstrtab : t -> Compiler_owee.Owee_elf_string_table.t

(** Maps original section names to (offset, renamed_name) pairs in shstrtab.
    For Large_code partitions, the stored values point to the renamed name
    (e.g., .caml.p1.text instead of .text). The renamed_name is used to update
    sh_name_str in the section header. *)
val section_name_offsets : t -> (string, int * string) Hashtbl.t

val igot_name_offset : t -> int

val igot_name_str : t -> string

val rela_igot_name_offset : t -> int

val rela_igot_name_str : t -> string

val iplt_name_offset : t -> int

val iplt_name_str : t -> string

val rela_iplt_name_offset : t -> int

val rela_iplt_name_str : t -> string

val igot_idx : t -> int

val rela_igot_idx : t -> int

val iplt_idx : t -> int

val rela_iplt_idx : t -> int

val num_sections : t -> int

val symtab_idx : t -> int

(** Index of the SYMTAB_SHNDX section in the input file, if present. *)
val symtab_shndx_idx : t -> int option

(** Index of a newly created SYMTAB_SHNDX section, if one needs to be created
    because the input doesn't have one but new section indices >= SHN_LORESERVE. *)
val new_symtab_shndx_idx : t -> int option

(** Name offset in shstrtab for a newly created SYMTAB_SHNDX section. *)
val symtab_shndx_name_offset : t -> int option

val layout : t -> Layout.t

(** [compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_sections
      ~partition_kind ~igot_and_iplt ~relocations] analyzes the ELF structure
    and builds a rewrite plan.

    [rela_text_sections] is a list of (section, body) pairs for all .rela.text*
    sections in the input file. This handles both traditional single .rela.text
    sections and function sections (.rela.text.foo, .rela.text.bar, etc.).

    For [Large_code] partitions, section names are renamed with a prefix
    (e.g., .text -> .caml.p1.text). *)
val compute :
  header:Compiler_owee.Owee_elf.header ->
  sections:Compiler_owee.Owee_elf.section array ->
  symtab_body:Compiler_owee.Owee_buf.t ->
  strtab_body:Compiler_owee.Owee_buf.t ->
  rela_text_sections:(Compiler_owee.Owee_elf.section * Compiler_owee.Owee_buf.t)
                     list ->
  partition_kind:Partition.kind ->
  igot_and_iplt:Build_igot_and_iplt.t ->
  relocations:Extract_relocations.t ->
  t
