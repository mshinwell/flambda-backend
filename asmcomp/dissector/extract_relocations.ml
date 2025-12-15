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

(* ELF section type for RELA (relocations with addends) *)
let sht_rela = 4

let sht_symtab = 2

(* x86-64 relocation types we care about *)
let r_x86_64_plt32 = 4L

let r_x86_64_rex_gotpcrelx = 42L

(* Size of an Elf64_Rela entry in bytes *)
let rela_entry_size = 24

(* Size of an Elf64_Sym entry in bytes *)
let sym_entry_size = 24

type relocation_entry =
  { symbol_name : string;
    offset : int64
  }

type t =
  { convert_to_plt : relocation_entry list;
    convert_to_got : relocation_entry list
  }

let empty = { convert_to_plt = []; convert_to_got = [] }

let merge t1 t2 =
  { convert_to_plt = t1.convert_to_plt @ t2.convert_to_plt;
    convert_to_got = t1.convert_to_got @ t2.convert_to_got
  }

(* Extract symbol index from r_info (upper 32 bits) *)
let r_sym r_info = Int64.shift_right_logical r_info 32

(* Extract relocation type from r_info (lower 32 bits) *)
let r_type r_info = Int64.logand r_info 0xFFFFFFFFL

(* Read a symbol name from the symbol table *)
let read_symbol_name ~symtab_body ~strtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset >= Compiler_owee.Owee_buf.size symtab_body
  then None
  else
    (* Elf64_Sym: first 4 bytes are st_name (index into string table) *)
    let cursor = Compiler_owee.Owee_buf.cursor symtab_body ~at:sym_offset in
    let st_name = Compiler_owee.Owee_buf.Read.u32 cursor in
    (* Read null-terminated string from strtab *)
    if st_name >= Compiler_owee.Owee_buf.size strtab_body
    then None
    else
      let cursor = Compiler_owee.Owee_buf.cursor strtab_body ~at:st_name in
      Compiler_owee.Owee_buf.Read.zero_string cursor ()

(* Parse RELA entries from a section body *)
let parse_rela_section ~rela_body ~symtab_body ~strtab_body =
  let size = Compiler_owee.Owee_buf.size rela_body in
  let num_entries = size / rela_entry_size in
  let convert_to_plt = ref [] in
  let convert_to_got = ref [] in
  for i = 0 to num_entries - 1 do
    let entry_offset = i * rela_entry_size in
    let cursor = Compiler_owee.Owee_buf.cursor rela_body ~at:entry_offset in
    let r_offset = Compiler_owee.Owee_buf.Read.u64 cursor in
    let r_info = Compiler_owee.Owee_buf.Read.u64 cursor in
    (* r_addend is not needed for our purposes *)
    let reloc_type = r_type r_info in
    if Int64.equal reloc_type r_x86_64_plt32
       || Int64.equal reloc_type r_x86_64_rex_gotpcrelx
    then
      let sym_index = Int64.to_int (r_sym r_info) in
      match read_symbol_name ~symtab_body ~strtab_body ~sym_index with
      | None -> ()
      | Some symbol_name ->
        let entry = { symbol_name; offset = r_offset } in
        if Int64.equal reloc_type r_x86_64_plt32
        then convert_to_plt := entry :: !convert_to_plt
        else convert_to_got := entry :: !convert_to_got
  done;
  { convert_to_plt = List.rev !convert_to_plt;
    convert_to_got = List.rev !convert_to_got
  }

(* Find a section by name *)
let find_section sections name =
  Array.find_opt
    (fun (section : Compiler_owee.Owee_elf.section) ->
      String.equal section.sh_name_str name)
    sections

(* Find the symbol table section *)
let find_symtab_section sections =
  Array.find_opt
    (fun (section : Compiler_owee.Owee_elf.section) ->
      section.sh_type = sht_symtab)
    sections

let extract (unix : (module Compiler_owee.Unix_intf.S)) ~filename =
  let module Unix = (val unix) in
  let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
  let _header, sections = Compiler_owee.Owee_elf.read_elf buf in
  (* Find .rela.text section *)
  match find_section sections ".rela.text" with
  | None -> empty
  | Some rela_section -> (
    (* Find symbol table *)
    match find_symtab_section sections with
    | None -> empty
    | Some symtab_section ->
      (* Find string table (sh_link of symtab points to it) *)
      let strtab_index = symtab_section.sh_link in
      if strtab_index >= Array.length sections
      then empty
      else
        let strtab_section = sections.(strtab_index) in
        let rela_body = Compiler_owee.Owee_elf.section_body buf rela_section in
        let symtab_body =
          Compiler_owee.Owee_elf.section_body buf symtab_section
        in
        let strtab_body =
          Compiler_owee.Owee_elf.section_body buf strtab_section
        in
        parse_rela_section ~rela_body ~symtab_body ~strtab_body)

let extract_from_linked_partitions unix linked_partitions =
  List.fold_left
    (fun acc (linked : Partition.linked) ->
      let result = extract unix ~filename:linked.linked_object in
      merge acc result)
    empty linked_partitions
