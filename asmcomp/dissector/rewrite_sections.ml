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

let write_symbol ~cursor ~strtab (sym : Form_rewrite_plan.symbol_entry) =
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
    ~shstrtab_section ~igot_and_iplt ~(plan : Form_rewrite_plan.t) =
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
  let relocate_section (s : Elf.section)
      (layout : Form_rewrite_plan.section_layout) : Elf.section =
    { s with
      sh_offset = Int64.of_int layout.offset;
      sh_size = Int64.of_int layout.size
    }
  in
  (* Update sh_name to point to the (possibly renamed) section name in
     shstrtab *)
  let rename_section (s : Elf.section) : Elf.section =
    match Hashtbl.find_opt plan.section_name_offsets s.sh_name_str with
    | Some new_name_offset -> { s with sh_name = new_name_offset }
    | None -> s
  in
  let update_section (s : Elf.section) =
    let s = rename_section s in
    match s.sh_name_str with
    | ".symtab" -> relocate_section s plan.layout.symtab_layout
    | ".strtab" -> relocate_section s plan.layout.strtab_layout
    | ".rela.text" -> relocate_section s plan.layout.rela_text
    | _ -> s
  in
  let new_sections = Array.make plan.num_sections sections.(0) in
  Array.iteri (fun i s -> new_sections.(i) <- update_section s) sections;
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

let rewrite unix ~input_file ~output_file ~partition_kind ~igot_and_iplt
    ~relocations =
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
    Form_rewrite_plan.compute ~header ~sections ~symtab_body ~strtab_body
      ~rela_text_body ~partition_kind ~igot_and_iplt ~relocations
  in
  execute_plan unix ~input_file ~output_file ~header ~sections ~shstrtab_section
    ~igot_and_iplt ~plan
