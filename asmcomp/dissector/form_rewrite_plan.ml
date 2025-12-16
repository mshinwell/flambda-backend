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

module Elf = Compiler_owee.Owee_elf
module Rela = Compiler_owee.Owee_elf_relocation
module Strtab = Compiler_owee.Owee_elf_string_table

let align_up value alignment =
  let mask = alignment - 1 in
  (value + mask) land lnot mask

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
    new_rela_text : Rela.rela_entry list;
    strtab : Strtab.t;
    shstrtab : Strtab.t;
    section_name_offsets : (string, int) Hashtbl.t;
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

let read_symbols ~symtab_body ~strtab_body =
  let symbols = ref [] in
  Elf.iter_symbols ~symtab_body ~strtab_body
    ~f:(fun ~name ~st_info ~st_other ~st_shndx ~st_value ~st_size ->
      symbols
        := { name; st_info; st_other; st_shndx; st_value; st_size } :: !symbols);
  Array.of_list (List.rev !symbols)

let build_symbol_index_map ~original_symbols ~igot_and_iplt strtab =
  let symbol_to_index = Hashtbl.create 256 in
  Array.iteri
    (fun index sym ->
      if not (Hashtbl.mem symbol_to_index sym.name)
      then Hashtbl.add symbol_to_index sym.name index;
      ignore (Strtab.add strtab sym.name))
    original_symbols;
  let next_index = ref (Array.length original_symbols) in
  List.iter
    (fun (entry : Igot.entry) ->
      Hashtbl.add symbol_to_index entry.igot_symbol !next_index;
      ignore (Strtab.add strtab entry.igot_symbol);
      incr next_index)
    (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot);
  List.iter
    (fun (entry : Iplt.entry) ->
      Hashtbl.add symbol_to_index entry.iplt_symbol !next_index;
      ignore (Strtab.add strtab entry.iplt_symbol);
      incr next_index)
    (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt);
  symbol_to_index, !next_index

let build_relocation_rewrite_map ~igot_and_iplt ~relocations =
  let map = Hashtbl.create 256 in
  List.iter
    (fun (entry : Extract_relocations.relocation_entry) ->
      match
        Build_igot_and_iplt.iplt_symbol_for_plt_reloc igot_and_iplt entry
      with
      | Some sym -> Hashtbl.add map (entry.offset, Rela.r_x86_64_plt32) sym
      | None -> ())
    relocations.Extract_relocations.convert_to_plt;
  List.iter
    (fun (entry : Extract_relocations.relocation_entry) ->
      match
        Build_igot_and_iplt.igot_symbol_for_got_reloc igot_and_iplt entry
      with
      | Some sym ->
        Hashtbl.add map (entry.offset, Rela.r_x86_64_rex_gotpcrelx) sym
      | None -> ())
    relocations.Extract_relocations.convert_to_got;
  map

let rewrite_rela_text ~rela_body ~symbol_to_index ~rewrite_map =
  let entries = ref [] in
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      let new_entry =
        match Hashtbl.find_opt rewrite_map (entry.r_offset, entry.r_type) with
        | Some new_sym_name -> (
          match Hashtbl.find_opt symbol_to_index new_sym_name with
          | Some idx -> { entry with r_sym = idx; r_type = Rela.r_x86_64_pc32 }
          | None -> entry)
        | None -> entry
      in
      entries := new_entry :: !entries);
  List.rev !entries

let compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
    ~strtab_size ~rela_text_count ~shstrtab_size ~num_sections ~shentsize =
  let current = ref (Int64.to_int original_data_end) in
  let alloc alignment size =
    current := align_up !current alignment;
    let offset = !current in
    current := offset + size;
    { offset; size }
  in
  let igot =
    alloc 16 (Igot.section_size igot_and_iplt.Build_igot_and_iplt.igot)
  in
  let rela_igot =
    let count =
      List.length (Igot.relocations igot_and_iplt.Build_igot_and_iplt.igot)
    in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let iplt =
    alloc 16 (Iplt.section_size igot_and_iplt.Build_igot_and_iplt.iplt)
  in
  let rela_iplt =
    let count =
      List.length (Iplt.relocations igot_and_iplt.Build_igot_and_iplt.iplt)
    in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let symtab_layout = alloc 8 (total_symbols * Rela.sym_entry_size) in
  let strtab_layout = alloc 1 strtab_size in
  let rela_text = alloc 8 (rela_text_count * Rela.rela_entry_size) in
  let shstrtab_layout = alloc 1 shstrtab_size in
  let section_headers_offset = align_up !current 8 in
  let total_size = section_headers_offset + (num_sections * shentsize) in
  { igot;
    rela_igot;
    iplt;
    rela_iplt;
    symtab_layout;
    strtab_layout;
    rela_text;
    shstrtab_layout;
    section_headers_offset;
    total_size
  }

(* Sections that should be renamed for Large_code partitions *)
let sections_to_rename = [".text"; ".rodata"; ".data"; ".bss"; ".eh_frame"]

(* Rename a section name based on partition kind. For Large_code partitions,
   .text -> .caml.p1.text, .rela.text -> .rela.caml.p1.text, etc. *)
let rename_section ~partition_kind name =
  match partition_kind with
  | Partition.Main -> name
  | Partition.Large_code _ ->
    let prefix = Partition.section_prefix partition_kind in
    (* Check if this is a section that needs renaming *)
    let needs_rename =
      List.exists
        (fun s -> String.equal name s || String.starts_with ~prefix:s name)
        sections_to_rename
    in
    let is_rela = String.starts_with ~prefix:".rela" name in
    if needs_rename
    then
      if is_rela
      then
        (* .rela.text -> .rela.caml.p1.text *)
        let base = String.sub name 5 (String.length name - 5) in
        ".rela" ^ prefix ^ base
      else prefix ^ name
    else name

let compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_body
    ~partition_kind ~igot_and_iplt ~relocations =
  let original_symbols = read_symbols ~symtab_body ~strtab_body in
  let strtab = Strtab.create () in
  let symbol_to_index, total_symbols =
    build_symbol_index_map ~original_symbols ~igot_and_iplt strtab
  in
  let rewrite_map = build_relocation_rewrite_map ~igot_and_iplt ~relocations in
  let new_rela_text =
    rewrite_rela_text ~rela_body:rela_text_body ~symbol_to_index ~rewrite_map
  in
  let shstrtab = Strtab.create () in
  let section_name_offsets = Hashtbl.create 64 in
  Array.iter
    (fun (s : Elf.section) ->
      let renamed = rename_section ~partition_kind s.sh_name_str in
      let offset = Strtab.add shstrtab renamed in
      Hashtbl.add section_name_offsets s.sh_name_str offset)
    sections;
  let igot_name_offset = Strtab.add shstrtab ".data.igot" in
  let rela_igot_name_offset = Strtab.add shstrtab ".rela.data.igot" in
  let iplt_name_offset = Strtab.add shstrtab ".text.iplt" in
  let rela_iplt_name_offset = Strtab.add shstrtab ".rela.text.iplt" in
  let num_original = Array.length sections in
  let igot_idx = num_original in
  let rela_igot_idx = num_original + 1 in
  let iplt_idx = num_original + 2 in
  let rela_iplt_idx = num_original + 3 in
  let num_sections = num_original + 4 in
  let symtab_idx =
    let idx = ref 0 in
    Array.iteri
      (fun i (s : Elf.section) ->
        if s.sh_type = Elf.Section_type.sht_symtab then idx := i)
      sections;
    !idx
  in
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  let layout =
    compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
      ~strtab_size:(Strtab.length strtab)
      ~rela_text_count:(List.length new_rela_text)
      ~shstrtab_size:(Strtab.length shstrtab) ~num_sections
      ~shentsize:header.Elf.e_shentsize
  in
  { original_symbols;
    symbol_to_index;
    total_symbols;
    new_rela_text;
    strtab;
    shstrtab;
    section_name_offsets;
    igot_name_offset;
    rela_igot_name_offset;
    iplt_name_offset;
    rela_iplt_name_offset;
    igot_idx;
    rela_igot_idx;
    iplt_idx;
    rela_iplt_idx;
    num_sections;
    symtab_idx;
    layout
  }
