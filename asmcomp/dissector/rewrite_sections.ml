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

type rewrite_plan =
  { original_symbols : symbol_entry array;
    symbol_to_index : (string, int) Hashtbl.t;
    total_symbols : int;
    new_rela_text : Rela.rela_entry list;
    strtab : Strtab.t;
    shstrtab : Strtab.t;
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

and layout =
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

let compute_plan ~header ~sections ~symtab_body ~strtab_body ~rela_text_body
    ~igot_and_iplt ~relocations =
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
  Array.iter
    (fun (s : Elf.section) -> ignore (Strtab.add shstrtab s.sh_name_str))
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

let write_symbol ~cursor ~strtab sym =
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab sym.name;
      st_info = sym.st_info;
      st_other = sym.st_other;
      st_shndx = sym.st_shndx;
      st_value = sym.st_value;
      st_size = sym.st_size
    }

let write_synthetic_symbol ~cursor ~strtab ~name ~section_index ~offset ~size
    ~is_func =
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab name;
      st_info =
        Rela.make_st_info ~binding:Rela.Stb.local
          ~typ:(if is_func then Rela.Stt.func else Rela.Stt.notype);
      st_other = 0;
      st_shndx = section_index;
      st_value = Int64.of_int offset;
      st_size = Int64.of_int size
    }

let write_rela ~cursor ~symbol_to_index ~r_offset ~symbol ~r_type ~r_addend =
  let r_sym =
    Hashtbl.find_opt symbol_to_index symbol |> Option.value ~default:0
  in
  Rela.write_rela_entry ~cursor
    { r_offset = Int64.of_int r_offset; r_sym; r_type; r_addend }

let execute_plan unix ~input_file ~output_file ~header ~sections
    ~shstrtab_section ~igot_and_iplt ~plan =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let input_buf = Buf.map_binary (module Unix) input_file in
  let output_buf =
    Buf.map_binary_write (module Unix) output_file plan.layout.total_size
  in
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  let original_size = Int64.to_int original_data_end in
  for i = 0 to original_size - 1 do
    Bigarray.Array1.set output_buf i (Bigarray.Array1.get input_buf i)
  done;
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:plan.layout.igot.offset)
    plan.layout.igot.size
    (Igot.section_data igot_and_iplt.Build_igot_and_iplt.igot);
  let cursor = Buf.cursor output_buf ~at:plan.layout.rela_igot.offset in
  List.iter
    (fun (r : Igot.relocation) ->
      write_rela ~cursor ~symbol_to_index:plan.symbol_to_index
        ~r_offset:r.offset ~symbol:r.symbol ~r_type:Rela.r_x86_64_64
        ~r_addend:r.addend)
    (Igot.relocations igot_and_iplt.Build_igot_and_iplt.igot);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:plan.layout.iplt.offset)
    plan.layout.iplt.size
    (Iplt.section_data igot_and_iplt.Build_igot_and_iplt.iplt);
  let cursor = Buf.cursor output_buf ~at:plan.layout.rela_iplt.offset in
  List.iter
    (fun (r : Iplt.relocation) ->
      write_rela ~cursor ~symbol_to_index:plan.symbol_to_index
        ~r_offset:r.offset ~symbol:r.symbol ~r_type:Rela.r_x86_64_pc32
        ~r_addend:r.addend)
    (Iplt.relocations igot_and_iplt.Build_igot_and_iplt.iplt);
  let cursor = Buf.cursor output_buf ~at:plan.layout.symtab_layout.offset in
  Array.iter
    (fun sym -> write_symbol ~cursor ~strtab:plan.strtab sym)
    plan.original_symbols;
  List.iter
    (fun (entry : Igot.entry) ->
      write_synthetic_symbol ~cursor ~strtab:plan.strtab ~name:entry.igot_symbol
        ~section_index:plan.igot_idx ~offset:(Igot.entry_offset entry)
        ~size:Igot.entry_size ~is_func:false)
    (Igot.entries igot_and_iplt.Build_igot_and_iplt.igot);
  List.iter
    (fun (entry : Iplt.entry) ->
      write_synthetic_symbol ~cursor ~strtab:plan.strtab ~name:entry.iplt_symbol
        ~section_index:plan.iplt_idx ~offset:(Iplt.entry_offset entry)
        ~size:Iplt.entry_size ~is_func:true)
    (Iplt.entries igot_and_iplt.Build_igot_and_iplt.iplt);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:plan.layout.strtab_layout.offset)
    plan.layout.strtab_layout.size
    (Strtab.contents plan.strtab);
  let cursor = Buf.cursor output_buf ~at:plan.layout.rela_text.offset in
  List.iter (fun e -> Rela.write_rela_entry ~cursor e) plan.new_rela_text;
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:plan.layout.shstrtab_layout.offset)
    plan.layout.shstrtab_layout.size
    (Strtab.contents plan.shstrtab);
  let new_sections = Array.make plan.num_sections sections.(0) in
  Array.iteri
    (fun i (s : Elf.section) ->
      new_sections.(i)
        <- (if String.equal s.sh_name_str ".symtab"
           then
             { s with
               sh_offset = Int64.of_int plan.layout.symtab_layout.offset;
               sh_size = Int64.of_int plan.layout.symtab_layout.size
             }
           else if String.equal s.sh_name_str ".strtab"
           then
             { s with
               sh_offset = Int64.of_int plan.layout.strtab_layout.offset;
               sh_size = Int64.of_int plan.layout.strtab_layout.size
             }
           else if String.equal s.sh_name_str ".rela.text"
           then
             { s with
               sh_offset = Int64.of_int plan.layout.rela_text.offset;
               sh_size = Int64.of_int plan.layout.rela_text.size
             }
           else s))
    sections;
  new_sections.(plan.igot_idx)
    <- Elf.make_progbits_section ~sh_name:plan.igot_name_offset
         ~sh_name_str:".data.igot"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_write Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int plan.layout.igot.offset)
         ~sh_size:(Int64.of_int plan.layout.igot.size)
         ~sh_addralign:16L;
  new_sections.(plan.rela_igot_idx)
    <- Elf.make_rela_section ~sh_name:plan.rela_igot_name_offset
         ~sh_name_str:".rela.data.igot"
         ~sh_offset:(Int64.of_int plan.layout.rela_igot.offset)
         ~sh_size:(Int64.of_int plan.layout.rela_igot.size)
         ~sh_link:plan.symtab_idx ~sh_info:plan.igot_idx;
  new_sections.(plan.iplt_idx)
    <- Elf.make_progbits_section ~sh_name:plan.iplt_name_offset
         ~sh_name_str:".text.iplt"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_execinstr
              Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int plan.layout.iplt.offset)
         ~sh_size:(Int64.of_int plan.layout.iplt.size)
         ~sh_addralign:16L;
  new_sections.(plan.rela_iplt_idx)
    <- Elf.make_rela_section ~sh_name:plan.rela_iplt_name_offset
         ~sh_name_str:".rela.text.iplt"
         ~sh_offset:(Int64.of_int plan.layout.rela_iplt.offset)
         ~sh_size:(Int64.of_int plan.layout.rela_iplt.size)
         ~sh_link:plan.symtab_idx ~sh_info:plan.iplt_idx;
  new_sections.(header.Elf.e_shstrndx)
    <- ({ shstrtab_section with
          sh_offset = Int64.of_int plan.layout.shstrtab_layout.offset;
          sh_size = Int64.of_int plan.layout.shstrtab_layout.size
        }
         : Elf.section);
  let new_header : Elf.header =
    { header with
      e_shoff = Int64.of_int plan.layout.section_headers_offset;
      e_shnum = plan.num_sections
    }
  in
  Elf.write_elf output_buf new_header new_sections

let rewrite unix ~input_file ~output_file ~igot_and_iplt ~relocations =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let input_buf = Buf.map_binary (module Unix) input_file in
  let header, sections = Elf.read_elf input_buf in
  let find_section_exn name =
    match Elf.find_section sections name with
    | Some s -> s
    | None -> Misc.fatal_errorf "rewrite_sections: no %s section found" name
  in
  let symtab_section =
    match
      Array.find_opt
        (fun (s : Elf.section) -> s.sh_type = Elf.Section_type.sht_symtab)
        sections
    with
    | Some s -> s
    | None -> Misc.fatal_error "rewrite_sections: no symbol table found"
  in
  let strtab_section = sections.(symtab_section.sh_link) in
  let rela_text_section = find_section_exn ".rela.text" in
  let shstrtab_section = sections.(header.e_shstrndx) in
  let symtab_body = Elf.section_body input_buf symtab_section in
  let strtab_body = Elf.section_body input_buf strtab_section in
  let rela_text_body = Elf.section_body input_buf rela_text_section in
  let plan =
    compute_plan ~header ~sections ~symtab_body ~strtab_body ~rela_text_body
      ~igot_and_iplt ~relocations
  in
  execute_plan unix ~input_file ~output_file ~header ~sections ~shstrtab_section
    ~igot_and_iplt ~plan
