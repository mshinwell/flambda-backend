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

module Symbol_entry = struct
  type t =
    { name : string;
      st_info : int;
      st_other : int;
      st_shndx : int;
      st_value : int64;
      st_size : int64
    }

  let name s = s.name

  let st_info s = s.st_info

  let st_other s = s.st_other

  let st_shndx s = s.st_shndx

  let st_value s = s.st_value

  let st_size s = s.st_size
end

module Section_layout = struct
  type t =
    { offset : int;
      size : int
    }

  let offset l = l.offset

  let size l = l.size
end

module Layout = struct
  type t =
    { igot : Section_layout.t;
      rela_igot : Section_layout.t;
      iplt : Section_layout.t;
      rela_iplt : Section_layout.t;
      symtab : Section_layout.t;
      strtab : Section_layout.t;
      rela_text : Section_layout.t;
      shstrtab : Section_layout.t;
      section_headers_offset : int;
      total_size : int
    }

  let igot l = l.igot

  let rela_igot l = l.rela_igot

  let iplt l = l.iplt

  let rela_iplt l = l.rela_iplt

  let symtab l = l.symtab

  let strtab l = l.strtab

  let rela_text l = l.rela_text

  let shstrtab l = l.shstrtab

  let section_headers_offset l = l.section_headers_offset

  let total_size l = l.total_size
end

type t =
  { original_symbols : Symbol_entry.t array;
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
    layout : Layout.t
  }

let original_symbols t = t.original_symbols

let symbol_to_index t = t.symbol_to_index

let total_symbols t = t.total_symbols

let new_rela_text t = t.new_rela_text

let strtab t = t.strtab

let shstrtab t = t.shstrtab

let section_name_offsets t = t.section_name_offsets

let igot_name_offset t = t.igot_name_offset

let rela_igot_name_offset t = t.rela_igot_name_offset

let iplt_name_offset t = t.iplt_name_offset

let rela_iplt_name_offset t = t.rela_iplt_name_offset

let igot_idx t = t.igot_idx

let rela_igot_idx t = t.rela_igot_idx

let iplt_idx t = t.iplt_idx

let rela_iplt_idx t = t.rela_iplt_idx

let num_sections t = t.num_sections

let symtab_idx t = t.symtab_idx

let layout t = t.layout

let read_symbols ~symtab_body ~strtab_body =
  let symbols = ref [] in
  Elf.iter_symbols ~symtab_body ~strtab_body
    ~f:(fun ~name ~st_info ~st_other ~st_shndx ~st_value ~st_size ->
      symbols
        := { Symbol_entry.name; st_info; st_other; st_shndx; st_value; st_size }
           :: !symbols);
  Array.of_list (List.rev !symbols)

let build_symbol_index_map ~original_symbols ~igot_and_iplt strtab =
  let symbol_to_index = Hashtbl.create 256 in
  Array.iteri
    (fun index sym ->
      let name = Symbol_entry.name sym in
      if not (Hashtbl.mem symbol_to_index name)
      then Hashtbl.add symbol_to_index name index;
      ignore (Strtab.add strtab name))
    original_symbols;
  let next_index = ref (Array.length original_symbols) in
  List.iter
    (fun entry ->
      let igot_sym = Igot.Entry.igot_symbol entry in
      Hashtbl.add symbol_to_index igot_sym !next_index;
      ignore (Strtab.add strtab igot_sym);
      incr next_index)
    (Igot.entries (Build_igot_and_iplt.igot igot_and_iplt));
  List.iter
    (fun entry ->
      let iplt_sym = Iplt.Entry.iplt_symbol entry in
      Hashtbl.add symbol_to_index iplt_sym !next_index;
      ignore (Strtab.add strtab iplt_sym);
      incr next_index)
    (Iplt.entries (Build_igot_and_iplt.iplt igot_and_iplt));
  symbol_to_index, !next_index

let build_relocation_rewrite_map ~igot_and_iplt ~relocations =
  let map = Hashtbl.create 256 in
  List.iter
    (fun entry ->
      match
        Build_igot_and_iplt.iplt_symbol_for_plt_reloc igot_and_iplt entry
      with
      | Some sym ->
        let offset = Extract_relocations.Relocation_entry.offset entry in
        Hashtbl.add map (offset, Rela.r_x86_64_plt32) sym
      | None -> ())
    (Extract_relocations.convert_to_plt relocations);
  List.iter
    (fun entry ->
      match
        Build_igot_and_iplt.igot_symbol_for_got_reloc igot_and_iplt entry
      with
      | Some sym ->
        let offset = Extract_relocations.Relocation_entry.offset entry in
        Hashtbl.add map (offset, Rela.r_x86_64_rex_gotpcrelx) sym
      | None -> ())
    (Extract_relocations.convert_to_got relocations);
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
    { Section_layout.offset; size }
  in
  let igot_t = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt_t = Build_igot_and_iplt.iplt igot_and_iplt in
  let igot = alloc 16 (Igot.section_size igot_t) in
  let rela_igot =
    let count = List.length (Igot.relocations igot_t) in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let iplt = alloc 16 (Iplt.section_size iplt_t) in
  let rela_iplt =
    let count = List.length (Iplt.relocations iplt_t) in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let symtab = alloc 8 (total_symbols * Rela.sym_entry_size) in
  let strtab = alloc 1 strtab_size in
  let rela_text = alloc 8 (rela_text_count * Rela.rela_entry_size) in
  let shstrtab = alloc 1 shstrtab_size in
  let section_headers_offset = align_up !current 8 in
  let total_size = section_headers_offset + (num_sections * shentsize) in
  { Layout.igot;
    rela_igot;
    iplt;
    rela_iplt;
    symtab;
    strtab;
    rela_text;
    shstrtab;
    section_headers_offset;
    total_size
  }

(* Sections that should be renamed for Large_code partitions *)
let sections_to_rename = [".text"; ".rodata"; ".data"; ".bss"; ".eh_frame"]

(* Rename a section name based on partition kind.

   For Large_code partitions, .text -> .caml.p1.text, .rela.text ->
   .rela.caml.p1.text, etc. *)
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
