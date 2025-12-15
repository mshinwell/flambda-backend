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
module Buf = Compiler_owee.Owee_buf

(* ELF section types *)
let sht_progbits = 1

let sht_rela = 4

(* ELF section flags *)
let shf_write = 0x1L

let shf_alloc = 0x2L

let shf_execinstr = 0x4L

let shf_info_link = 0x40L

(* Align a value up to the given alignment *)
let align_up value alignment =
  let mask = alignment - 1 in
  (value + mask) land lnot mask

let align_up64 value alignment =
  let mask = Int64.sub alignment 1L in
  Int64.logand (Int64.add value mask) (Int64.lognot mask)

(* Build a mapping from symbol names to their indices in the new symbol table.
   Returns (symbol_to_index, next_index) *)
let build_symbol_index_map ~original_symbols ~igot_and_iplt strtab =
  let symbol_to_index = Hashtbl.create 256 in
  (* First, add all original symbol names and their indices *)
  List.iteri
    (fun index (name, _st_info, _st_other, _st_shndx, _st_value, _st_size) ->
      if not (Hashtbl.mem symbol_to_index name)
      then Hashtbl.add symbol_to_index name index;
      (* Also add to string table *)
      ignore (Strtab.add strtab name))
    original_symbols;
  let next_index = List.length original_symbols in
  (* Add IGOT symbols *)
  let next_index =
    List.fold_left
      (fun idx (entry : Igot.entry) ->
        Hashtbl.add symbol_to_index entry.igot_symbol idx;
        ignore (Strtab.add strtab entry.igot_symbol);
        idx + 1)
      next_index
      (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot)
  in
  (* Add IPLT symbols *)
  let next_index =
    List.fold_left
      (fun idx (entry : Iplt.entry) ->
        Hashtbl.add symbol_to_index entry.iplt_symbol idx;
        ignore (Strtab.add strtab entry.iplt_symbol);
        idx + 1)
      next_index
      (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt)
  in
  symbol_to_index, next_index

(* Read all symbols from the original symbol table *)
let read_original_symbols ~symtab_body ~strtab_body =
  let num_symbols = Buf.size symtab_body / Rela.sym_entry_size in
  let symbols = ref [] in
  for i = num_symbols - 1 downto 0 do
    let cursor = Buf.cursor symtab_body ~at:(i * Rela.sym_entry_size) in
    let st_name_offset = Buf.Read.u32 cursor in
    let st_info = Buf.Read.u8 cursor in
    let st_other = Buf.Read.u8 cursor in
    let st_shndx = Buf.Read.u16 cursor in
    let st_value = Buf.Read.u64 cursor in
    let st_size = Buf.Read.u64 cursor in
    (* Read the actual name string *)
    let name =
      if st_name_offset = 0
      then ""
      else
        let name_cursor = Buf.cursor strtab_body ~at:st_name_offset in
        match Buf.Read.zero_string name_cursor () with
        | Some s -> s
        | None -> ""
    in
    symbols
      := (name, st_info, st_other, st_shndx, st_value, st_size) :: !symbols
  done;
  !symbols

(* Build a map from (offset, original_type) to new symbol name for relocations
   that need rewriting *)
let build_relocation_rewrite_map ~igot_and_iplt ~relocations =
  let map = Hashtbl.create 256 in
  (* PLT32 relocations -> IPLT symbols *)
  List.iter
    (fun (entry : Extract_relocations.relocation_entry) ->
      match
        Build_igot_and_iplt.iplt_symbol_for_plt_reloc igot_and_iplt entry
      with
      | Some iplt_sym ->
        Hashtbl.add map (entry.offset, Rela.r_x86_64_plt32) iplt_sym
      | None -> ())
    relocations.Extract_relocations.convert_to_plt;
  (* GOTPCRELX relocations -> IGOT symbols *)
  List.iter
    (fun (entry : Extract_relocations.relocation_entry) ->
      match
        Build_igot_and_iplt.igot_symbol_for_got_reloc igot_and_iplt entry
      with
      | Some igot_sym ->
        Hashtbl.add map (entry.offset, Rela.r_x86_64_rex_gotpcrelx) igot_sym
      | None -> ())
    relocations.Extract_relocations.convert_to_got;
  map

(* Rewrite .rela.text entries, returning new entries *)
let rewrite_rela_text ~rela_body ~symbol_to_index ~rewrite_map =
  let entries = ref [] in
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      let new_entry =
        match Hashtbl.find_opt rewrite_map (entry.r_offset, entry.r_type) with
        | Some new_symbol_name -> (
          match Hashtbl.find_opt symbol_to_index new_symbol_name with
          | Some new_sym_index ->
            (* Change to PC32 relocation pointing to IGOT/IPLT symbol *)
            { entry with r_sym = new_sym_index; r_type = Rela.r_x86_64_pc32 }
          | None ->
            (* Symbol not found - keep original (shouldn't happen) *)
            entry)
        | None ->
          (* Not a relocation we need to rewrite *)
          entry
      in
      entries := new_entry :: !entries);
  List.rev !entries

(* Write symbol table entries to a cursor *)
let write_symbols ~cursor ~symbols ~strtab ~igot_section_index
    ~iplt_section_index ~igot_and_iplt =
  (* Write original symbols *)
  List.iter
    (fun (name, st_info, st_other, st_shndx, st_value, st_size) ->
      let st_name = Strtab.add strtab name in
      let entry : Rela.sym_entry =
        { st_name; st_info; st_other; st_shndx; st_value; st_size }
      in
      Rela.write_sym_entry ~cursor entry)
    symbols;
  (* Write IGOT symbol entries *)
  List.iter
    (fun (entry : Igot.entry) ->
      let st_name = Strtab.add strtab entry.igot_symbol in
      let sym_entry : Rela.sym_entry =
        { st_name;
          st_info =
            Rela.make_st_info ~binding:Rela.Stb.local ~typ:Rela.Stt.notype;
          st_other = 0;
          st_shndx = igot_section_index;
          st_value = Int64.of_int (Igot.entry_offset entry);
          st_size = Int64.of_int Igot.entry_size
        }
      in
      Rela.write_sym_entry ~cursor sym_entry)
    (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot);
  (* Write IPLT symbol entries *)
  List.iter
    (fun (entry : Iplt.entry) ->
      let st_name = Strtab.add strtab entry.iplt_symbol in
      let sym_entry : Rela.sym_entry =
        { st_name;
          st_info = Rela.make_st_info ~binding:Rela.Stb.local ~typ:Rela.Stt.func;
          st_other = 0;
          st_shndx = iplt_section_index;
          st_value = Int64.of_int (Iplt.entry_offset entry);
          st_size = Int64.of_int Iplt.entry_size
        }
      in
      Rela.write_sym_entry ~cursor sym_entry)
    (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt)

(* Write relocation entries to a cursor *)
let write_rela_entries ~cursor entries =
  List.iter (fun entry -> Rela.write_rela_entry ~cursor entry) entries

let rewrite unix ~input_file ~output_file ~igot_and_iplt ~relocations =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  (* Read the original ELF *)
  let input_buf = Buf.map_binary (module Unix) input_file in
  let header, sections = Elf.read_elf input_buf in
  (* Find required sections *)
  let symtab_section =
    match
      Array.find_opt
        (fun (s : Elf.section) -> s.sh_type = Rela.sht_symtab)
        sections
    with
    | Some s -> s
    | None -> Misc.fatal_error "rewrite_sections: no symbol table found"
  in
  let strtab_section = sections.(symtab_section.sh_link) in
  let rela_text_section =
    match Elf.find_section sections ".rela.text" with
    | Some s -> s
    | None -> Misc.fatal_error "rewrite_sections: no .rela.text section found"
  in
  let symtab_body = Elf.section_body input_buf symtab_section in
  let strtab_body = Elf.section_body input_buf strtab_section in
  let rela_text_body = Elf.section_body input_buf rela_text_section in
  (* Read original symbols *)
  let original_symbols = read_original_symbols ~symtab_body ~strtab_body in
  (* Build new string table *)
  let strtab = Strtab.create () in
  let symbol_to_index, total_symbols =
    build_symbol_index_map ~original_symbols ~igot_and_iplt strtab
  in
  (* Build relocation rewrite map *)
  let rewrite_map = build_relocation_rewrite_map ~igot_and_iplt ~relocations in
  (* Rewrite .rela.text entries *)
  let new_rela_text_entries =
    rewrite_rela_text ~rela_body:rela_text_body ~symbol_to_index ~rewrite_map
  in
  (* Calculate new section indices *)
  let num_original_sections = Array.length sections in
  let igot_section_index = num_original_sections in
  let rela_igot_section_index = num_original_sections + 1 in
  let iplt_section_index = num_original_sections + 2 in
  let rela_iplt_section_index = num_original_sections + 3 in
  let num_new_sections = num_original_sections + 4 in
  (* Calculate sizes *)
  let igot_size = Igot.section_size igot_and_iplt.Build_igot_and_iplt.igot in
  let iplt_size = Iplt.section_size igot_and_iplt.Build_igot_and_iplt.iplt in
  let igot_relocs = Igot.relocations igot_and_iplt.Build_igot_and_iplt.igot in
  let iplt_relocs = Iplt.relocations igot_and_iplt.Build_igot_and_iplt.iplt in
  let rela_igot_size = List.length igot_relocs * Rela.rela_entry_size in
  let rela_iplt_size = List.length iplt_relocs * Rela.rela_entry_size in
  let new_symtab_size = total_symbols * Rela.sym_entry_size in
  let new_strtab_size = Strtab.length strtab in
  let new_rela_text_size =
    List.length new_rela_text_entries * Rela.rela_entry_size
  in
  (* Calculate file layout - we'll append new data after original sections *)
  (* Find the end of original section data *)
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) ->
        let section_end = Int64.add s.sh_offset s.sh_size in
        if section_end > acc then section_end else acc)
      0L sections
  in
  (* Layout new sections with proper alignment *)
  let current_offset = ref (Int64.to_int original_data_end) in
  let align_and_advance alignment size =
    current_offset := align_up !current_offset alignment;
    let offset = !current_offset in
    current_offset := offset + size;
    offset
  in
  let igot_offset = align_and_advance 16 igot_size in
  let rela_igot_offset = align_and_advance 8 rela_igot_size in
  let iplt_offset = align_and_advance 16 iplt_size in
  let rela_iplt_offset = align_and_advance 8 rela_iplt_size in
  let new_symtab_offset = align_and_advance 8 new_symtab_size in
  let new_strtab_offset = align_and_advance 1 new_strtab_size in
  let new_rela_text_offset = align_and_advance 8 new_rela_text_size in
  (* Section headers go at the end *)
  let section_headers_offset = align_up !current_offset 8 in
  let total_file_size =
    section_headers_offset + (num_new_sections * header.e_shentsize)
  in
  (* Create output buffer *)
  let output_buf =
    Buf.map_binary_write (module Unix) output_file total_file_size
  in
  (* Copy original file content up to the original data end *)
  let original_size = Int64.to_int original_data_end in
  for i = 0 to original_size - 1 do
    Bigarray.Array1.set output_buf i (Bigarray.Array1.get input_buf i)
  done;
  (* Find symtab section index for linking *)
  let symtab_section_index =
    let idx = ref 0 in
    Array.iteri
      (fun i (s : Elf.section) -> if s.sh_type = Rela.sht_symtab then idx := i)
      sections;
    !idx
  in
  (* Write IGOT data *)
  let igot_data = Igot.section_data igot_and_iplt.Build_igot_and_iplt.igot in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:igot_offset)
    igot_size igot_data;
  (* Write IGOT relocations *)
  let igot_rela_cursor = Buf.cursor output_buf ~at:rela_igot_offset in
  List.iter
    (fun (r : Igot.relocation) ->
      (* Need to look up symbol index for original symbol *)
      let r_sym =
        match Hashtbl.find_opt symbol_to_index r.symbol with
        | Some idx -> idx
        | None -> 0
      in
      let entry : Rela.rela_entry =
        { r_offset = Int64.of_int r.offset;
          r_sym;
          r_type = Rela.r_x86_64_64;
          r_addend = r.addend
        }
      in
      Rela.write_rela_entry ~cursor:igot_rela_cursor entry)
    igot_relocs;
  (* Write IPLT data *)
  let iplt_data = Iplt.section_data igot_and_iplt.Build_igot_and_iplt.iplt in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:iplt_offset)
    iplt_size iplt_data;
  (* Write IPLT relocations *)
  let iplt_rela_cursor = Buf.cursor output_buf ~at:rela_iplt_offset in
  List.iter
    (fun (r : Iplt.relocation) ->
      (* IPLT relocs point to IGOT symbols *)
      let r_sym =
        match Hashtbl.find_opt symbol_to_index r.symbol with
        | Some idx -> idx
        | None -> 0
      in
      let entry : Rela.rela_entry =
        { r_offset = Int64.of_int r.offset;
          r_sym;
          r_type = Rela.r_x86_64_pc32;
          r_addend = r.addend
        }
      in
      Rela.write_rela_entry ~cursor:iplt_rela_cursor entry)
    iplt_relocs;
  (* Write new symbol table *)
  let symtab_cursor = Buf.cursor output_buf ~at:new_symtab_offset in
  write_symbols ~cursor:symtab_cursor ~symbols:original_symbols ~strtab
    ~igot_section_index ~iplt_section_index ~igot_and_iplt;
  (* Write new string table *)
  let strtab_data = Strtab.contents strtab in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:new_strtab_offset)
    new_strtab_size strtab_data;
  (* Write new .rela.text *)
  let rela_text_cursor = Buf.cursor output_buf ~at:new_rela_text_offset in
  write_rela_entries ~cursor:rela_text_cursor new_rela_text_entries;
  (* Build new section headers array *)
  let new_sections = Array.make num_new_sections sections.(0) in
  (* Copy and update original sections *)
  Array.iteri
    (fun i (s : Elf.section) ->
      let updated =
        if String.equal s.sh_name_str ".symtab"
        then
          { s with
            sh_offset = Int64.of_int new_symtab_offset;
            sh_size = Int64.of_int new_symtab_size
          }
        else if String.equal s.sh_name_str ".strtab"
        then
          { s with
            sh_offset = Int64.of_int new_strtab_offset;
            sh_size = Int64.of_int new_strtab_size
          }
        else if String.equal s.sh_name_str ".rela.text"
        then
          { s with
            sh_offset = Int64.of_int new_rela_text_offset;
            sh_size = Int64.of_int new_rela_text_size
          }
        else s
      in
      new_sections.(i) <- updated)
    sections;
  (* Add new sections *)
  (* Find shstrtab to add new section names *)
  let shstrtab_section = sections.(header.e_shstrndx) in
  (* Helper to add section name to shstrtab and get offset *)
  let shstrtab_builder = Strtab.create () in
  (* First add all existing section names *)
  Array.iter
    (fun (s : Elf.section) ->
      ignore (Strtab.add shstrtab_builder s.sh_name_str))
    sections;
  (* Add new section names *)
  let igot_name_offset = Strtab.add shstrtab_builder ".data.igot" in
  let rela_igot_name_offset = Strtab.add shstrtab_builder ".rela.data.igot" in
  let iplt_name_offset = Strtab.add shstrtab_builder ".text.iplt" in
  let rela_iplt_name_offset = Strtab.add shstrtab_builder ".rela.text.iplt" in
  (* IGOT section *)
  new_sections.(igot_section_index)
    <- ({ sh_name = igot_name_offset;
          sh_type = sht_progbits;
          sh_flags = Int64.logor shf_write shf_alloc;
          sh_addr = 0L;
          sh_offset = Int64.of_int igot_offset;
          sh_size = Int64.of_int igot_size;
          sh_link = 0;
          sh_info = 0;
          sh_addralign = 16L;
          sh_entsize = 0L;
          sh_name_str = ".data.igot"
        }
         : Elf.section);
  (* .rela.data.igot section *)
  new_sections.(rela_igot_section_index)
    <- ({ sh_name = rela_igot_name_offset;
          sh_type = sht_rela;
          sh_flags = shf_info_link;
          sh_addr = 0L;
          sh_offset = Int64.of_int rela_igot_offset;
          sh_size = Int64.of_int rela_igot_size;
          sh_link = symtab_section_index;
          sh_info = igot_section_index;
          sh_addralign = 8L;
          sh_entsize = Int64.of_int Rela.rela_entry_size;
          sh_name_str = ".rela.data.igot"
        }
         : Elf.section);
  (* IPLT section *)
  new_sections.(iplt_section_index)
    <- ({ sh_name = iplt_name_offset;
          sh_type = sht_progbits;
          sh_flags = Int64.logor shf_execinstr shf_alloc;
          sh_addr = 0L;
          sh_offset = Int64.of_int iplt_offset;
          sh_size = Int64.of_int iplt_size;
          sh_link = 0;
          sh_info = 0;
          sh_addralign = 16L;
          sh_entsize = 0L;
          sh_name_str = ".text.iplt"
        }
         : Elf.section);
  (* .rela.text.iplt section *)
  new_sections.(rela_iplt_section_index)
    <- ({ sh_name = rela_iplt_name_offset;
          sh_type = sht_rela;
          sh_flags = shf_info_link;
          sh_addr = 0L;
          sh_offset = Int64.of_int rela_iplt_offset;
          sh_size = Int64.of_int rela_iplt_size;
          sh_link = symtab_section_index;
          sh_info = iplt_section_index;
          sh_addralign = 8L;
          sh_entsize = Int64.of_int Rela.rela_entry_size;
          sh_name_str = ".rela.text.iplt"
        }
         : Elf.section);
  (* Update shstrtab section with new size and write new content *)
  let new_shstrtab_size = Strtab.length shstrtab_builder in
  let new_shstrtab_offset = align_up !current_offset 1 in
  new_sections.(header.e_shstrndx)
    <- { shstrtab_section with
         sh_offset = Int64.of_int new_shstrtab_offset;
         sh_size = Int64.of_int new_shstrtab_size
       };
  (* Recalculate file size with shstrtab *)
  current_offset := new_shstrtab_offset + new_shstrtab_size;
  let section_headers_offset = align_up !current_offset 8 in
  (* Note: total_file_size = section_headers_offset + (num_new_sections *
     header.e_shentsize). We assume the buffer was allocated large enough. *)
  (* Write new shstrtab *)
  let shstrtab_data = Strtab.contents shstrtab_builder in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:new_shstrtab_offset)
    new_shstrtab_size shstrtab_data;
  (* Update header *)
  let new_header =
    { header with
      e_shoff = Int64.of_int section_headers_offset;
      e_shnum = num_new_sections
    }
  in
  (* Write ELF header and section headers *)
  Elf.write_elf output_buf new_header new_sections
