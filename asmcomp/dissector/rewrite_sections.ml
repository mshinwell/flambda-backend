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
    { st_name = Strtab.add strtab (Form_rewrite_plan.Symbol_entry.name sym);
      st_info = Form_rewrite_plan.Symbol_entry.st_info sym;
      st_other = Form_rewrite_plan.Symbol_entry.st_other sym;
      st_shndx = Form_rewrite_plan.Symbol_entry.st_shndx sym;
      st_value = Form_rewrite_plan.Symbol_entry.st_value sym;
      st_size = Form_rewrite_plan.Symbol_entry.st_size sym
    }

(* Symbol visibility: STV_HIDDEN = 2 *)
let stv_hidden = 2

(* When section_index >= SHN_LORESERVE, we must use SHN_XINDEX and store the
   actual index in the SYMTAB_SHNDX section. *)
let write_synthetic_symbol ~cursor ~strtab ~name ~section_index ~offset ~size
    ~is_func =
  let st_shndx =
    if section_index >= Rela.shn_loreserve
    then Rela.shn_xindex
    else section_index
  in
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab name;
      st_info =
        Rela.make_st_info ~binding:Rela.Stb.global
          ~typ:(if is_func then Rela.Stt.func else Rela.Stt.notype);
      st_other = stv_hidden;
      st_shndx;
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
      (Form_rewrite_plan.Layout.total_size plan_layout)
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
  let igot_layout = Form_rewrite_plan.Layout.igot plan_layout in
  let rela_igot_layout = Form_rewrite_plan.Layout.rela_igot plan_layout in
  let iplt_layout = Form_rewrite_plan.Layout.iplt plan_layout in
  let rela_iplt_layout = Form_rewrite_plan.Layout.rela_iplt plan_layout in
  let symtab_layout = Form_rewrite_plan.Layout.symtab plan_layout in
  let strtab_layout = Form_rewrite_plan.Layout.strtab plan_layout in
  let shstrtab_layout = Form_rewrite_plan.Layout.shstrtab plan_layout in
  let igot = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt = Build_igot_and_iplt.iplt igot_and_iplt in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf
       ~at:(Form_rewrite_plan.Section_layout.offset igot_layout))
    (Form_rewrite_plan.Section_layout.size igot_layout)
    (Igot.section_data igot);
  let cursor =
    Buf.cursor output_buf
      ~at:(Form_rewrite_plan.Section_layout.offset rela_igot_layout)
  in
  List.iter
    (fun r ->
      write_rela ~cursor
        ~symbol_to_index:(Form_rewrite_plan.symbol_to_index plan)
        ~r_offset:(Igot.Relocation.offset r) ~symbol:(Igot.Relocation.symbol r)
        ~r_type:Rela.r_x86_64_64 ~r_addend:(Igot.Relocation.addend r))
    (Igot.relocations igot);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf
       ~at:(Form_rewrite_plan.Section_layout.offset iplt_layout))
    (Form_rewrite_plan.Section_layout.size iplt_layout)
    (Iplt.section_data iplt);
  let cursor =
    Buf.cursor output_buf
      ~at:(Form_rewrite_plan.Section_layout.offset rela_iplt_layout)
  in
  List.iter
    (fun r ->
      write_rela ~cursor
        ~symbol_to_index:(Form_rewrite_plan.symbol_to_index plan)
        ~r_offset:(Iplt.Relocation.offset r) ~symbol:(Iplt.Relocation.symbol r)
        ~r_type:Rela.r_x86_64_pc32 ~r_addend:(Iplt.Relocation.addend r))
    (Iplt.relocations iplt);
  let cursor =
    Buf.cursor output_buf
      ~at:(Form_rewrite_plan.Section_layout.offset symtab_layout)
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
    (Buf.cursor output_buf
       ~at:(Form_rewrite_plan.Section_layout.offset strtab_layout))
    (Form_rewrite_plan.Section_layout.size strtab_layout)
    (Strtab.contents plan_strtab);
  (* Write extended SYMTAB_SHNDX section if needed. This section must have the
     same number of entries as the symbol table.

     For symbols with st_shndx < SHN_LORESERVE, the SYMTAB_SHNDX entry is 0.
     For symbols with st_shndx = SHN_XINDEX, the SYMTAB_SHNDX entry contains
     the actual section index. *)
  (match
     ( Form_rewrite_plan.symtab_shndx_idx plan,
       Form_rewrite_plan.Layout.symtab_shndx plan_layout )
   with
  | _, Some symtab_shndx_layout ->
    let cursor =
      Buf.cursor output_buf
        ~at:(Form_rewrite_plan.Section_layout.offset symtab_shndx_layout)
    in
    (* Helper to write a 32-bit little-endian value *)
    let write_u32_le cursor value =
      Buf.Write.u8 cursor (value land 0xff);
      Buf.Write.u8 cursor ((value lsr 8) land 0xff);
      Buf.Write.u8 cursor ((value lsr 16) land 0xff);
      Buf.Write.u8 cursor ((value lsr 24) land 0xff)
    in
    (* Copy original entries if input has SYMTAB_SHNDX, else write zeros *)
    (match Form_rewrite_plan.symtab_shndx_idx plan with
    | Some symtab_shndx_idx ->
      let original_symtab_shndx_section = sections.(symtab_shndx_idx) in
      let original_symtab_shndx_body =
        Elf.section_body input_buf original_symtab_shndx_section
      in
      let original_size = Buf.size original_symtab_shndx_body in
      for i = 0 to original_size - 1 do
        Buf.Write.u8 cursor (Bigarray.Array1.get original_symtab_shndx_body i)
      done
    | None ->
      (* Input doesn't have SYMTAB_SHNDX; write zeros for all original symbols
         (they all have st_shndx < SHN_LORESERVE) *)
      let num_original =
        Array.length (Form_rewrite_plan.original_symbols plan)
      in
      for _ = 1 to num_original do
        write_u32_le cursor 0
      done);
    (* Write extended section indices for IGOT symbols *)
    let igot_idx = Form_rewrite_plan.igot_idx plan in
    let igot_shndx_entry =
      if igot_idx >= Rela.shn_loreserve then igot_idx else 0
    in
    List.iter
      (fun _ -> write_u32_le cursor igot_shndx_entry)
      (Igot.entries igot);
    (* Write extended section indices for IPLT symbols *)
    let iplt_idx = Form_rewrite_plan.iplt_idx plan in
    let iplt_shndx_entry =
      if iplt_idx >= Rela.shn_loreserve then iplt_idx else 0
    in
    List.iter
      (fun _ -> write_u32_le cursor iplt_shndx_entry)
      (Iplt.entries iplt)
  | None, None -> ()
  | Some _, None ->
    Misc.fatal_error "SYMTAB_SHNDX in input but no layout allocated");
  (* Write rewritten .rela.text* sections back to their original locations *)
  List.iter
    (fun rewritten_section ->
      let cursor =
        Buf.cursor output_buf
          ~at:(Int64.to_int
                 (Form_rewrite_plan.Rewritten_rela_section.section_offset
                    rewritten_section))
      in
      List.iter
        (fun e -> Rela.write_rela_entry ~cursor e)
        (Form_rewrite_plan.Rewritten_rela_section.entries rewritten_section))
    (Form_rewrite_plan.rewritten_rela_sections plan);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf
       ~at:(Form_rewrite_plan.Section_layout.offset shstrtab_layout))
    (Form_rewrite_plan.Section_layout.size shstrtab_layout)
    (Strtab.contents (Form_rewrite_plan.shstrtab plan));
  let relocate_section (s : Elf.section) layout : Elf.section =
    { s with
      sh_offset = Int64.of_int (Form_rewrite_plan.Section_layout.offset layout);
      sh_size = Int64.of_int (Form_rewrite_plan.Section_layout.size layout)
    }
  in
  (* Update sh_name to point to the (possibly renamed) section name in
     shstrtab, and also update sh_name_str so owee writes the correct name *)
  let section_name_offsets = Form_rewrite_plan.section_name_offsets plan in
  let rename_section (s : Elf.section) : Elf.section =
    match Hashtbl.find_opt section_name_offsets s.sh_name_str with
    | Some (new_name_offset, renamed_str) ->
      { s with sh_name = new_name_offset; sh_name_str = renamed_str }
    | None -> s
  in
  let symtab_shndx_layout_opt =
    Form_rewrite_plan.Layout.symtab_shndx plan_layout
  in
  let update_section (s : Elf.section) =
    let s = rename_section s in
    match s.sh_name_str with
    | ".symtab" -> relocate_section s symtab_layout
    | ".strtab" -> relocate_section s strtab_layout
    | ".symtab_shndx" -> (
      match symtab_shndx_layout_opt with
      | Some symtab_shndx_layout -> relocate_section s symtab_shndx_layout
      | None -> s)
    (* .rela.text* sections are rewritten in place, so no relocation needed *)
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
         ~sh_name_str:(Form_rewrite_plan.igot_name_str plan)
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_write Elf.Section_flags.shf_alloc)
         ~sh_offset:
           (Int64.of_int (Form_rewrite_plan.Section_layout.offset igot_layout))
         ~sh_size:
           (Int64.of_int (Form_rewrite_plan.Section_layout.size igot_layout))
         ~sh_addralign:16L;
  new_sections.(rela_igot_idx)
    <- Elf.make_rela_section
         ~sh_name:(Form_rewrite_plan.rela_igot_name_offset plan)
         ~sh_name_str:(Form_rewrite_plan.rela_igot_name_str plan)
         ~sh_offset:
           (Int64.of_int
              (Form_rewrite_plan.Section_layout.offset rela_igot_layout))
         ~sh_size:
           (Int64.of_int
              (Form_rewrite_plan.Section_layout.size rela_igot_layout))
         ~sh_link:symtab_idx ~sh_info:igot_idx;
  new_sections.(iplt_idx)
    <- Elf.make_progbits_section
         ~sh_name:(Form_rewrite_plan.iplt_name_offset plan)
         ~sh_name_str:(Form_rewrite_plan.iplt_name_str plan)
         ~sh_flags:
           (Int64.logor Elf.Section_flags.shf_execinstr
              Elf.Section_flags.shf_alloc)
         ~sh_offset:
           (Int64.of_int (Form_rewrite_plan.Section_layout.offset iplt_layout))
         ~sh_size:
           (Int64.of_int (Form_rewrite_plan.Section_layout.size iplt_layout))
         ~sh_addralign:16L;
  new_sections.(rela_iplt_idx)
    <- Elf.make_rela_section
         ~sh_name:(Form_rewrite_plan.rela_iplt_name_offset plan)
         ~sh_name_str:(Form_rewrite_plan.rela_iplt_name_str plan)
         ~sh_offset:
           (Int64.of_int
              (Form_rewrite_plan.Section_layout.offset rela_iplt_layout))
         ~sh_size:
           (Int64.of_int
              (Form_rewrite_plan.Section_layout.size rela_iplt_layout))
         ~sh_link:symtab_idx ~sh_info:iplt_idx;
  (* Create new SYMTAB_SHNDX section if needed *)
  (match
     ( Form_rewrite_plan.new_symtab_shndx_idx plan,
       Form_rewrite_plan.symtab_shndx_name_offset plan,
       symtab_shndx_layout_opt )
   with
  | Some new_idx, Some name_offset, Some symtab_shndx_layout ->
    new_sections.(new_idx)
      <- Elf.make_symtab_shndx_section ~sh_name:name_offset
           ~sh_name_str:".symtab_shndx"
           ~sh_offset:
             (Int64.of_int
                (Form_rewrite_plan.Section_layout.offset symtab_shndx_layout))
           ~sh_size:
             (Int64.of_int
                (Form_rewrite_plan.Section_layout.size symtab_shndx_layout))
           ~sh_link:symtab_idx
  | None, None, _ -> ()
  | _ -> Misc.fatal_error "Inconsistent new SYMTAB_SHNDX state");
  (* Update the shstrtab section that was already processed by update_section
     (which updated sh_name and sh_name_str), not the original shstrtab_section *)
  let updated_shstrtab = new_sections.(header.Elf.e_shstrndx) in
  new_sections.(header.Elf.e_shstrndx)
    <- ({ updated_shstrtab with
          sh_offset =
            Int64.of_int
              (Form_rewrite_plan.Section_layout.offset shstrtab_layout);
          sh_size =
            Int64.of_int (Form_rewrite_plan.Section_layout.size shstrtab_layout)
        }
         : Elf.section);
  let new_header : Elf.header =
    { header with
      e_shoff =
        Int64.of_int
          (Form_rewrite_plan.Layout.section_headers_offset plan_layout);
      e_shnum = num_sections
    }
  in
  Elf.write_elf output_buf new_header new_sections

(* Find all sections with names starting with prefix *)
let find_sections_with_prefix sections prefix =
  Array.to_list sections
  |> List.filter (fun (section : Elf.section) ->
         String.starts_with ~prefix section.sh_name_str)

let rewrite unix ~input_file ~output_file ~partition_kind ~igot_and_iplt
    ~relocations =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let input_buf = Buf.map_binary (module Unix) input_file in
  let header, sections = Elf.read_elf input_buf in
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
  (* Find all .rela.text* sections (handles function sections) *)
  let rela_text_section_list =
    find_sections_with_prefix sections ".rela.text"
  in
  let shstrtab_section = sections.(header.e_shstrndx) in
  let symtab_body = Elf.section_body input_buf symtab_section in
  let strtab_body = Elf.section_body input_buf strtab_section in
  (* Build list of (section, body) pairs *)
  let rela_text_sections =
    List.map
      (fun section -> section, Elf.section_body input_buf section)
      rela_text_section_list
  in
  let plan =
    Form_rewrite_plan.compute ~header ~sections ~symtab_body ~strtab_body
      ~rela_text_sections ~partition_kind ~igot_and_iplt ~relocations
  in
  execute_plan unix ~input_file ~output_file ~header ~sections ~shstrtab_section
    ~igot_and_iplt ~plan
