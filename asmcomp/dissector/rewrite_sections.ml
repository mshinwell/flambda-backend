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

(* -------------------------------------------------------------------------- *)
(* Alignment utilities *)
(* -------------------------------------------------------------------------- *)

let align_up value alignment =
  let mask = alignment - 1 in
  (value + mask) land lnot mask

(* -------------------------------------------------------------------------- *)
(* Symbol table reading - TODO: consider moving to owee *)
(* -------------------------------------------------------------------------- *)

type symbol_entry =
  { name : string;
    st_info : int;
    st_other : int;
    st_shndx : int;
    st_value : int64;
    st_size : int64
  }

let read_symbols ~symtab_body ~strtab_body =
  let num_symbols = Buf.size symtab_body / Rela.sym_entry_size in
  let symbols =
    Array.make num_symbols
      { name = "";
        st_info = 0;
        st_other = 0;
        st_shndx = 0;
        st_value = 0L;
        st_size = 0L
      }
  in
  for i = 0 to num_symbols - 1 do
    let cursor = Buf.cursor symtab_body ~at:(i * Rela.sym_entry_size) in
    let st_name_offset = Buf.Read.u32 cursor in
    let st_info = Buf.Read.u8 cursor in
    let st_other = Buf.Read.u8 cursor in
    let st_shndx = Buf.Read.u16 cursor in
    let st_value = Buf.Read.u64 cursor in
    let st_size = Buf.Read.u64 cursor in
    let name =
      if st_name_offset = 0
      then ""
      else
        let name_cursor = Buf.cursor strtab_body ~at:st_name_offset in
        match Buf.Read.zero_string name_cursor () with
        | Some s -> s
        | None -> ""
    in
    symbols.(i) <- { name; st_info; st_other; st_shndx; st_value; st_size }
  done;
  symbols

(* -------------------------------------------------------------------------- *)
(* Symbol index mapping *)
(* -------------------------------------------------------------------------- *)

let build_symbol_index_map ~original_symbols ~igot_and_iplt strtab =
  let symbol_to_index = Hashtbl.create 256 in
  (* Add original symbols *)
  Array.iteri
    (fun index sym ->
      if not (Hashtbl.mem symbol_to_index sym.name)
      then Hashtbl.add symbol_to_index sym.name index;
      ignore (Strtab.add strtab sym.name))
    original_symbols;
  let next_index = ref (Array.length original_symbols) in
  (* Add IGOT symbols *)
  List.iter
    (fun (entry : Igot.entry) ->
      Hashtbl.add symbol_to_index entry.igot_symbol !next_index;
      ignore (Strtab.add strtab entry.igot_symbol);
      incr next_index)
    (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot);
  (* Add IPLT symbols *)
  List.iter
    (fun (entry : Iplt.entry) ->
      Hashtbl.add symbol_to_index entry.iplt_symbol !next_index;
      ignore (Strtab.add strtab entry.iplt_symbol);
      incr next_index)
    (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt);
  symbol_to_index, !next_index

(* -------------------------------------------------------------------------- *)
(* Relocation rewriting *)
(* -------------------------------------------------------------------------- *)

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

(* -------------------------------------------------------------------------- *)
(* Writing helpers *)
(* -------------------------------------------------------------------------- *)

let write_symbol ~cursor ~strtab sym =
  let st_name = Strtab.add strtab sym.name in
  Rela.write_sym_entry ~cursor
    { st_name;
      st_info = sym.st_info;
      st_other = sym.st_other;
      st_shndx = sym.st_shndx;
      st_value = sym.st_value;
      st_size = sym.st_size
    }

let write_synthetic_symbol ~cursor ~strtab ~name ~section_index ~offset ~size
    ~is_func =
  let st_name = Strtab.add strtab name in
  let st_info =
    Rela.make_st_info ~binding:Rela.Stb.local
      ~typ:(if is_func then Rela.Stt.func else Rela.Stt.notype)
  in
  Rela.write_sym_entry ~cursor
    { st_name;
      st_info;
      st_other = 0;
      st_shndx = section_index;
      st_value = Int64.of_int offset;
      st_size = Int64.of_int size
    }

let write_rela_entry ~cursor ~symbol_to_index ~r_offset ~symbol ~r_type
    ~r_addend =
  let r_sym =
    match Hashtbl.find_opt symbol_to_index symbol with
    | Some idx -> idx
    | None -> 0
  in
  Rela.write_rela_entry ~cursor
    { r_offset = Int64.of_int r_offset; r_sym; r_type; r_addend }

(* -------------------------------------------------------------------------- *)
(* File layout calculation *)
(* -------------------------------------------------------------------------- *)

type section_layout =
  { offset : int;
    size : int
  }

type file_layout =
  { igot : section_layout;
    rela_igot : section_layout;
    iplt : section_layout;
    rela_iplt : section_layout;
    symtab : section_layout;
    strtab : section_layout;
    rela_text : section_layout;
    shstrtab : section_layout;
    section_headers_offset : int;
    total_size : int
  }

let compute_layout ~original_data_end ~igot_and_iplt ~total_symbols ~strtab_size
    ~rela_text_count ~shstrtab_size ~num_sections ~shentsize =
  let current = ref (Int64.to_int original_data_end) in
  let layout alignment size =
    current := align_up !current alignment;
    let offset = !current in
    current := offset + size;
    { offset; size }
  in
  let igot =
    layout 16 (Igot.section_size igot_and_iplt.Build_igot_and_iplt.igot)
  in
  let rela_igot =
    let count =
      List.length (Igot.relocations igot_and_iplt.Build_igot_and_iplt.igot)
    in
    layout 8 (count * Rela.rela_entry_size)
  in
  let iplt =
    layout 16 (Iplt.section_size igot_and_iplt.Build_igot_and_iplt.iplt)
  in
  let rela_iplt =
    let count =
      List.length (Iplt.relocations igot_and_iplt.Build_igot_and_iplt.iplt)
    in
    layout 8 (count * Rela.rela_entry_size)
  in
  let symtab = layout 8 (total_symbols * Rela.sym_entry_size) in
  let strtab = layout 1 strtab_size in
  let rela_text = layout 8 (rela_text_count * Rela.rela_entry_size) in
  let shstrtab = layout 1 shstrtab_size in
  let section_headers_offset = align_up !current 8 in
  let total_size = section_headers_offset + (num_sections * shentsize) in
  { igot;
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

(* -------------------------------------------------------------------------- *)
(* Main rewrite function *)
(* -------------------------------------------------------------------------- *)

let rewrite unix ~input_file ~output_file ~igot_and_iplt ~relocations =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  (* Read original ELF *)
  let input_buf = Buf.map_binary (module Unix) input_file in
  let header, sections = Elf.read_elf input_buf in
  (* Find required sections *)
  let find_section_exn name =
    match Elf.find_section sections name with
    | Some s -> s
    | None -> Misc.fatal_errorf "rewrite_sections: no %s section found" name
  in
  let find_section_by_type_exn typ =
    match
      Array.find_opt (fun (s : Elf.section) -> s.sh_type = typ) sections
    with
    | Some s -> s
    | None -> Misc.fatal_error "rewrite_sections: required section not found"
  in
  let symtab_section = find_section_by_type_exn Elf.Section_type.sht_symtab in
  let strtab_section = sections.(symtab_section.sh_link) in
  let rela_text_section = find_section_exn ".rela.text" in
  let shstrtab_section = sections.(header.e_shstrndx) in
  (* Read section bodies *)
  let symtab_body = Elf.section_body input_buf symtab_section in
  let strtab_body = Elf.section_body input_buf strtab_section in
  let rela_text_body = Elf.section_body input_buf rela_text_section in
  (* Read original symbols *)
  let original_symbols = read_symbols ~symtab_body ~strtab_body in
  (* Build new string table and symbol index map *)
  let strtab = Strtab.create () in
  let symbol_to_index, total_symbols =
    build_symbol_index_map ~original_symbols ~igot_and_iplt strtab
  in
  (* Build relocation rewrite map and rewrite .rela.text *)
  let rewrite_map = build_relocation_rewrite_map ~igot_and_iplt ~relocations in
  let new_rela_text =
    rewrite_rela_text ~rela_body:rela_text_body ~symbol_to_index ~rewrite_map
  in
  (* Build shstrtab with new section names *)
  let shstrtab_builder = Strtab.create () in
  Array.iter
    (fun (s : Elf.section) ->
      ignore (Strtab.add shstrtab_builder s.sh_name_str))
    sections;
  let igot_name_offset = Strtab.add shstrtab_builder ".data.igot" in
  let rela_igot_name_offset = Strtab.add shstrtab_builder ".rela.data.igot" in
  let iplt_name_offset = Strtab.add shstrtab_builder ".text.iplt" in
  let rela_iplt_name_offset = Strtab.add shstrtab_builder ".rela.text.iplt" in
  (* Calculate new section indices *)
  let num_original = Array.length sections in
  let igot_idx, rela_igot_idx = num_original, num_original + 1 in
  let iplt_idx, rela_iplt_idx = num_original + 2, num_original + 3 in
  let num_sections = num_original + 4 in
  (* Find symtab section index *)
  let symtab_idx =
    let idx = ref 0 in
    Array.iteri
      (fun i (s : Elf.section) ->
        if s.sh_type = Elf.Section_type.sht_symtab then idx := i)
      sections;
    !idx
  in
  (* Find end of original data *)
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  (* Compute layout *)
  let layout =
    compute_layout ~original_data_end ~igot_and_iplt ~total_symbols
      ~strtab_size:(Strtab.length strtab)
      ~rela_text_count:(List.length new_rela_text)
      ~shstrtab_size:(Strtab.length shstrtab_builder)
      ~num_sections ~shentsize:header.e_shentsize
  in
  (* Create output buffer and copy original data *)
  let output_buf =
    Buf.map_binary_write (module Unix) output_file layout.total_size
  in
  let original_size = Int64.to_int original_data_end in
  for i = 0 to original_size - 1 do
    Bigarray.Array1.set output_buf i (Bigarray.Array1.get input_buf i)
  done;
  (* Write IGOT section data *)
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:layout.igot.offset)
    layout.igot.size
    (Igot.section_data igot_and_iplt.Build_igot_and_iplt.igot);
  (* Write IGOT relocations *)
  let cursor = Buf.cursor output_buf ~at:layout.rela_igot.offset in
  List.iter
    (fun (r : Igot.relocation) ->
      write_rela_entry ~cursor ~symbol_to_index ~r_offset:r.offset
        ~symbol:r.symbol ~r_type:Rela.r_x86_64_64 ~r_addend:r.addend)
    (Igot.relocations igot_and_iplt.Build_igot_and_iplt.igot);
  (* Write IPLT section data *)
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:layout.iplt.offset)
    layout.iplt.size
    (Iplt.section_data igot_and_iplt.Build_igot_and_iplt.iplt);
  (* Write IPLT relocations *)
  let cursor = Buf.cursor output_buf ~at:layout.rela_iplt.offset in
  List.iter
    (fun (r : Iplt.relocation) ->
      write_rela_entry ~cursor ~symbol_to_index ~r_offset:r.offset
        ~symbol:r.symbol ~r_type:Rela.r_x86_64_pc32 ~r_addend:r.addend)
    (Iplt.relocations igot_and_iplt.Build_igot_and_iplt.iplt);
  (* Write symbol table *)
  let cursor = Buf.cursor output_buf ~at:layout.symtab.offset in
  Array.iter (fun sym -> write_symbol ~cursor ~strtab sym) original_symbols;
  List.iter
    (fun (entry : Igot.entry) ->
      write_synthetic_symbol ~cursor ~strtab ~name:entry.igot_symbol
        ~section_index:igot_idx ~offset:(Igot.entry_offset entry)
        ~size:Igot.entry_size ~is_func:false)
    (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot);
  List.iter
    (fun (entry : Iplt.entry) ->
      write_synthetic_symbol ~cursor ~strtab ~name:entry.iplt_symbol
        ~section_index:iplt_idx ~offset:(Iplt.entry_offset entry)
        ~size:Iplt.entry_size ~is_func:true)
    (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt);
  (* Write string table *)
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:layout.strtab.offset)
    layout.strtab.size (Strtab.contents strtab);
  (* Write .rela.text *)
  let cursor = Buf.cursor output_buf ~at:layout.rela_text.offset in
  List.iter (fun e -> Rela.write_rela_entry ~cursor e) new_rela_text;
  (* Write shstrtab *)
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:layout.shstrtab.offset)
    layout.shstrtab.size
    (Strtab.contents shstrtab_builder);
  (* Build section headers *)
  let new_sections = Array.make num_sections sections.(0) in
  Array.iteri
    (fun i (s : Elf.section) ->
      new_sections.(i)
        <- (if String.equal s.sh_name_str ".symtab"
           then
             { s with
               sh_offset = Int64.of_int layout.symtab.offset;
               sh_size = Int64.of_int layout.symtab.size
             }
           else if String.equal s.sh_name_str ".strtab"
           then
             { s with
               sh_offset = Int64.of_int layout.strtab.offset;
               sh_size = Int64.of_int layout.strtab.size
             }
           else if String.equal s.sh_name_str ".rela.text"
           then
             { s with
               sh_offset = Int64.of_int layout.rela_text.offset;
               sh_size = Int64.of_int layout.rela_text.size
             }
           else s))
    sections;
  (* Add IGOT section *)
  new_sections.(igot_idx)
    <- Elf.make_progbits_section ~sh_name:igot_name_offset
         ~sh_name_str:".data.igot"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_write Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int layout.igot.offset)
         ~sh_size:(Int64.of_int layout.igot.size)
         ~sh_addralign:16L;
  (* Add IGOT relocation section *)
  new_sections.(rela_igot_idx)
    <- Elf.make_rela_section ~sh_name:rela_igot_name_offset
         ~sh_name_str:".rela.data.igot"
         ~sh_offset:(Int64.of_int layout.rela_igot.offset)
         ~sh_size:(Int64.of_int layout.rela_igot.size)
         ~sh_link:symtab_idx ~sh_info:igot_idx;
  (* Add IPLT section *)
  new_sections.(iplt_idx)
    <- Elf.make_progbits_section ~sh_name:iplt_name_offset
         ~sh_name_str:".text.iplt"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_execinstr
              Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int layout.iplt.offset)
         ~sh_size:(Int64.of_int layout.iplt.size)
         ~sh_addralign:16L;
  (* Add IPLT relocation section *)
  new_sections.(rela_iplt_idx)
    <- Elf.make_rela_section ~sh_name:rela_iplt_name_offset
         ~sh_name_str:".rela.text.iplt"
         ~sh_offset:(Int64.of_int layout.rela_iplt.offset)
         ~sh_size:(Int64.of_int layout.rela_iplt.size)
         ~sh_link:symtab_idx ~sh_info:iplt_idx;
  (* Update shstrtab section *)
  new_sections.(header.e_shstrndx)
    <- { shstrtab_section with
         sh_offset = Int64.of_int layout.shstrtab.offset;
         sh_size = Int64.of_int layout.shstrtab.size
       };
  (* Write ELF header and section headers *)
  let new_header =
    { header with
      e_shoff = Int64.of_int layout.section_headers_offset;
      e_shnum = num_sections
    }
  in
  Elf.write_elf output_buf new_header new_sections
