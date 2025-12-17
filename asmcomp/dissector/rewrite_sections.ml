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

let write_symbol ~cursor ~strtab sym =
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab (Form_rewrite_plan.symbol_name sym);
      st_info = Form_rewrite_plan.symbol_st_info sym;
      st_other = Form_rewrite_plan.symbol_st_other sym;
      st_shndx = Form_rewrite_plan.symbol_st_shndx sym;
      st_value = Form_rewrite_plan.symbol_st_value sym;
      st_size = Form_rewrite_plan.symbol_st_size sym
    }

(* Symbol visibility: STV_HIDDEN = 2 *)
let stv_hidden = 2

let write_synthetic_symbol ~cursor ~strtab ~name ~section_index ~offset ~size
    ~is_func =
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab name;
      st_info =
        Rela.make_st_info ~binding:Rela.Stb.global
          ~typ:(if is_func then Rela.Stt.func else Rela.Stt.notype);
      st_other = stv_hidden;
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
  let plan_layout = Form_rewrite_plan.layout plan in
  let output_buf =
    Buf.map_binary_write
      (module Unix)
      output_file
      (Form_rewrite_plan.layout_total_size plan_layout)
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
  let igot_layout = Form_rewrite_plan.layout_igot plan_layout in
  let rela_igot_layout = Form_rewrite_plan.layout_rela_igot plan_layout in
  let iplt_layout = Form_rewrite_plan.layout_iplt plan_layout in
  let rela_iplt_layout = Form_rewrite_plan.layout_rela_iplt plan_layout in
  let symtab_layout = Form_rewrite_plan.layout_symtab plan_layout in
  let strtab_layout = Form_rewrite_plan.layout_strtab plan_layout in
  let rela_text_layout = Form_rewrite_plan.layout_rela_text plan_layout in
  let shstrtab_layout = Form_rewrite_plan.layout_shstrtab plan_layout in
  let igot = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt = Build_igot_and_iplt.iplt igot_and_iplt in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset igot_layout))
    (Form_rewrite_plan.layout_size igot_layout)
    (Igot.section_data igot);
  let cursor =
    Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset rela_igot_layout)
  in
  List.iter
    (fun r ->
      write_rela ~cursor
        ~symbol_to_index:(Form_rewrite_plan.symbol_to_index plan)
        ~r_offset:(Igot.Relocation.offset r) ~symbol:(Igot.Relocation.symbol r)
        ~r_type:Rela.r_x86_64_64 ~r_addend:(Igot.Relocation.addend r))
    (Igot.relocations igot);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset iplt_layout))
    (Form_rewrite_plan.layout_size iplt_layout)
    (Iplt.section_data iplt);
  let cursor =
    Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset rela_iplt_layout)
  in
  List.iter
    (fun r ->
      write_rela ~cursor
        ~symbol_to_index:(Form_rewrite_plan.symbol_to_index plan)
        ~r_offset:(Iplt.Relocation.offset r) ~symbol:(Iplt.Relocation.symbol r)
        ~r_type:Rela.r_x86_64_pc32 ~r_addend:(Iplt.Relocation.addend r))
    (Iplt.relocations iplt);
  let cursor =
    Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset symtab_layout)
  in
  let plan_strtab = Form_rewrite_plan.strtab plan in
  Array.iter
    (fun sym -> write_symbol ~cursor ~strtab:plan_strtab sym)
    (Form_rewrite_plan.original_symbols plan);
  List.iter
    (fun entry ->
      write_synthetic_symbol ~cursor ~strtab:plan_strtab
        ~name:(Igot.Entry.igot_symbol entry)
        ~section_index:(Form_rewrite_plan.igot_idx plan)
        ~offset:(Igot.Entry.offset entry) ~size:Igot.entry_size ~is_func:false)
    (Igot.entries igot);
  List.iter
    (fun entry ->
      write_synthetic_symbol ~cursor ~strtab:plan_strtab
        ~name:(Iplt.Entry.iplt_symbol entry)
        ~section_index:(Form_rewrite_plan.iplt_idx plan)
        ~offset:(Iplt.Entry.offset entry) ~size:Iplt.entry_size ~is_func:true)
    (Iplt.entries iplt);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset strtab_layout))
    (Form_rewrite_plan.layout_size strtab_layout)
    (Strtab.contents plan_strtab);
  let cursor =
    Buf.cursor output_buf ~at:(Form_rewrite_plan.layout_offset rela_text_layout)
  in
  List.iter
    (fun e -> Rela.write_rela_entry ~cursor e)
    (Form_rewrite_plan.new_rela_text plan);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf
       ~at:(Form_rewrite_plan.layout_offset shstrtab_layout))
    (Form_rewrite_plan.layout_size shstrtab_layout)
    (Strtab.contents (Form_rewrite_plan.shstrtab plan));
  let relocate_section (s : Elf.section) layout : Elf.section =
    { s with
      sh_offset = Int64.of_int (Form_rewrite_plan.layout_offset layout);
      sh_size = Int64.of_int (Form_rewrite_plan.layout_size layout)
    }
  in
  (* Update sh_name to point to the (possibly renamed) section name in
     shstrtab *)
  let section_name_offsets = Form_rewrite_plan.section_name_offsets plan in
  let rename_section (s : Elf.section) : Elf.section =
    match Hashtbl.find_opt section_name_offsets s.sh_name_str with
    | Some new_name_offset -> { s with sh_name = new_name_offset }
    | None -> s
  in
  let update_section (s : Elf.section) =
    let s = rename_section s in
    match s.sh_name_str with
    | ".symtab" -> relocate_section s symtab_layout
    | ".strtab" -> relocate_section s strtab_layout
    | ".rela.text" -> relocate_section s rela_text_layout
    | _ -> s
  in
  let num_sections = Form_rewrite_plan.num_sections plan in
  let igot_idx = Form_rewrite_plan.igot_idx plan in
  let rela_igot_idx = Form_rewrite_plan.rela_igot_idx plan in
  let iplt_idx = Form_rewrite_plan.iplt_idx plan in
  let rela_iplt_idx = Form_rewrite_plan.rela_iplt_idx plan in
  let symtab_idx = Form_rewrite_plan.symtab_idx plan in
  let new_sections = Array.make num_sections sections.(0) in
  Array.iteri (fun i s -> new_sections.(i) <- update_section s) sections;
  new_sections.(igot_idx)
    <- Elf.make_progbits_section
         ~sh_name:(Form_rewrite_plan.igot_name_offset plan)
         ~sh_name_str:".data.igot"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_write Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int (Form_rewrite_plan.layout_offset igot_layout))
         ~sh_size:(Int64.of_int (Form_rewrite_plan.layout_size igot_layout))
         ~sh_addralign:16L;
  new_sections.(rela_igot_idx)
    <- Elf.make_rela_section
         ~sh_name:(Form_rewrite_plan.rela_igot_name_offset plan)
         ~sh_name_str:".rela.data.igot"
         ~sh_offset:
           (Int64.of_int (Form_rewrite_plan.layout_offset rela_igot_layout))
         ~sh_size:
           (Int64.of_int (Form_rewrite_plan.layout_size rela_igot_layout))
         ~sh_link:symtab_idx ~sh_info:igot_idx;
  new_sections.(iplt_idx)
    <- Elf.make_progbits_section
         ~sh_name:(Form_rewrite_plan.iplt_name_offset plan)
         ~sh_name_str:".text.iplt"
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_execinstr
              Elf.Section_flags.shf_alloc)
         ~sh_offset:(Int64.of_int (Form_rewrite_plan.layout_offset iplt_layout))
         ~sh_size:(Int64.of_int (Form_rewrite_plan.layout_size iplt_layout))
         ~sh_addralign:16L;
  new_sections.(rela_iplt_idx)
    <- Elf.make_rela_section
         ~sh_name:(Form_rewrite_plan.rela_iplt_name_offset plan)
         ~sh_name_str:".rela.text.iplt"
         ~sh_offset:
           (Int64.of_int (Form_rewrite_plan.layout_offset rela_iplt_layout))
         ~sh_size:
           (Int64.of_int (Form_rewrite_plan.layout_size rela_iplt_layout))
         ~sh_link:symtab_idx ~sh_info:iplt_idx;
  new_sections.(header.Elf.e_shstrndx)
    <- ({ shstrtab_section with
          sh_offset =
            Int64.of_int (Form_rewrite_plan.layout_offset shstrtab_layout);
          sh_size = Int64.of_int (Form_rewrite_plan.layout_size shstrtab_layout)
        }
         : Elf.section);
  let new_header : Elf.header =
    { header with
      e_shoff =
        Int64.of_int
          (Form_rewrite_plan.layout_section_headers_offset plan_layout);
      e_shnum = num_sections
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
