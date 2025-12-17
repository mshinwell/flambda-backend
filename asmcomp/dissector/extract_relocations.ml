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

module Rela = Compiler_owee.Owee_elf_relocation

module Relocation_entry = struct
  type t =
    { symbol_name : string;
      offset : int64
    }

  let symbol_name t = t.symbol_name

  let offset t = t.offset
end

type t =
  { convert_to_plt : Relocation_entry.t list;
    convert_to_got : Relocation_entry.t list
  }

let convert_to_plt t = t.convert_to_plt

let convert_to_got t = t.convert_to_got

let empty = { convert_to_plt = []; convert_to_got = [] }

let merge t1 t2 =
  { convert_to_plt = t1.convert_to_plt @ t2.convert_to_plt;
    convert_to_got = t1.convert_to_got @ t2.convert_to_got
  }

(* Parse RELA entries and extract PLT32 and REX_GOTPCRELX relocations for
   undefined symbols (st_shndx = SHN_UNDEF). Only undefined symbols need PLT/GOT
   entries since defined symbols can be resolved directly. *)
let parse_rela_section ~rela_body ~symtab_body ~strtab_body =
  let convert_to_plt = ref [] in
  let convert_to_got = ref [] in
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      if Int64.equal entry.r_type Rela.r_x86_64_plt32
         || Int64.equal entry.r_type Rela.r_x86_64_rex_gotpcrelx
      then
        (* Only process relocations for undefined symbols *)
        match Rela.read_symbol_shndx ~symtab_body ~sym_index:entry.r_sym with
        | None -> ()
        | Some shndx when shndx <> Rela.shn_undef -> ()
        | Some _ -> (
          match
            Rela.read_symbol_name ~symtab_body ~strtab_body
              ~sym_index:entry.r_sym
          with
          | None -> ()
          | Some symbol_name ->
            let reloc_entry =
              { Relocation_entry.symbol_name; offset = entry.r_offset }
            in
            if Int64.equal entry.r_type Rela.r_x86_64_plt32
            then convert_to_plt := reloc_entry :: !convert_to_plt
            else convert_to_got := reloc_entry :: !convert_to_got));
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
      section.sh_type = Rela.sht_symtab)
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
    (fun acc linked ->
      let result =
        extract unix ~filename:(Partition.Linked.linked_object linked)
      in
      merge acc result)
    empty linked_partitions
