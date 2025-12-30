(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Jane Street Group LLC                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type section_mismatch =
  { section_name : string;
    byte_offset : int;
    instruction_offset : int option;
    expected : string;
    actual : string;
    expected_size : int;
    actual_size : int
  }

type relocation_mismatch =
  { section_name : string;
    offset : int;
    expected : string;
    actual : string
  }

type mismatch =
  | Section_content of section_mismatch
  | Section_size of
      { section_name : string;
        expected : int;
        actual : int
      }
  | Relocation of relocation_mismatch
  | Missing_section of string
  | Missing_binary_sections_dir of string

type result =
  | Match of
      { text_size : int;
        data_size : int
      }
  | Mismatch of mismatch
  | Object_file_error of string

(* Helper to read a file as bytes *)
let read_file_bytes filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Helper to format bytes as hex dump *)
let hex_dump ?(max_bytes = 32) (s : string) (offset : int) : string =
  let len = min max_bytes (String.length s - offset) in
  if len <= 0
  then "(empty)"
  else
    let buf = Buffer.create (len * 3) in
    for i = 0 to len - 1 do
      if i > 0 && i mod 4 = 0 then Buffer.add_char buf ' ';
      Printf.bprintf buf "%02x" (Char.code s.[offset + i])
    done;
    Buffer.contents buf

(* Find the first byte difference between two strings *)
let find_first_difference s1 s2 =
  let len = min (String.length s1) (String.length s2) in
  let rec loop i =
    if i >= len
    then if String.length s1 <> String.length s2 then Some len else None
    else if s1.[i] <> s2.[i]
    then Some i
    else loop (i + 1)
  in
  loop 0

(* Instruction size for alignment in text section *)
let instruction_size () =
  match Target_system.architecture () with
  | AArch64 -> Some 4
  | X86_64 -> None (* Variable length instructions *)
  | _ -> None

(* Align offset to instruction boundary *)
let align_to_instruction offset =
  match instruction_size () with
  | Some size -> offset / size * size
  | None -> offset

(* Compare section contents *)
let compare_section ~section_name ~expected ~actual =
  let expected_size = String.length expected in
  let actual_size = String.length actual in
  if expected_size <> actual_size
  then
    Some
      (Section_size
         { section_name; expected = expected_size; actual = actual_size })
  else
    match find_first_difference expected actual with
    | None -> None
    | Some byte_offset ->
      let instruction_offset =
        if String.equal section_name "text" || String.equal section_name ".text"
        then Some (align_to_instruction byte_offset)
        else None
      in
      let display_offset =
        Option.value instruction_offset ~default:byte_offset
      in
      Some
        (Section_content
           { section_name;
             byte_offset;
             instruction_offset;
             expected = hex_dump expected display_offset;
             actual = hex_dump actual display_offset;
             expected_size;
             actual_size
           })

(* Read binary emitter section from .binary-sections directory *)
let read_binary_section binary_sections_dir section_name =
  let filename =
    Filename.concat binary_sections_dir ("section_" ^ section_name ^ ".bin")
  in
  if Sys.file_exists filename then Some (read_file_bytes filename) else None

(* A relocation: offset, symbol name, and addend *)
type be_relocation =
  { be_offset : int;
    be_symbol : string;
    be_addend : int64
  }

(* Symbol information: which section it's in and its offset within that section *)
type symbol_info =
  { sym_section : string;
    sym_offset : int64
  }

(* Read binary emitter relocations from .relocs file.
   Format: "offset symbol [addend]" where addend defaults to 0 *)
let read_binary_relocations binary_sections_dir section_name =
  let filename =
    Filename.concat binary_sections_dir ("section_" ^ section_name ^ ".relocs")
  in
  if not (Sys.file_exists filename)
  then []
  else
    let ic = open_in filename in
    let relocs = ref [] in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char ' ' line with
         | [offset_str; symbol] ->
           let offset = int_of_string offset_str in
           relocs
             := { be_offset = offset; be_symbol = symbol; be_addend = 0L }
                :: !relocs
         | [offset_str; symbol; addend_str] ->
           let offset = int_of_string offset_str in
           let addend = Int64.of_string addend_str in
           relocs
             := { be_offset = offset; be_symbol = symbol; be_addend = addend }
                :: !relocs
         | _ -> ()
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !relocs

(* Convert saved section filename back to original section name. e.g.,
   "section_text.caml.foo.bin" -> ".text.caml.foo" *)
let section_name_of_filename filename =
  if String.length filename > 12
     && String.sub filename 0 8 = "section_"
     && String.sub filename (String.length filename - 4) 4 = ".bin"
  then
    let name_part = String.sub filename 8 (String.length filename - 12) in
    "." ^ name_part
  else filename

(* List all text section files in binary-sections directory. Returns list of
   (section_name, content) pairs. *)
let list_binary_text_sections binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then []
  else if not (Sys.is_directory binary_sections_dir)
  then []
  else
    let files = Sys.readdir binary_sections_dir in
    Array.to_list files
    |> List.filter (fun f ->
           String.length f > 12
           && String.sub f 0 12 = "section_text"
           && String.sub f (String.length f - 4) 4 = ".bin")
    |> List.map (fun f ->
           let section_name = section_name_of_filename f in
           let content =
             read_file_bytes (Filename.concat binary_sections_dir f)
           in
           section_name, content)

(* List all text section relocation files in binary-sections directory. Returns
   list of (section_name, relocations) pairs. *)
let list_binary_text_relocations binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then []
  else if not (Sys.is_directory binary_sections_dir)
  then []
  else
    let files = Sys.readdir binary_sections_dir in
    Array.to_list files
    |> List.filter (fun f ->
           String.length f > 15
           && String.sub f 0 12 = "section_text"
           && String.sub f (String.length f - 7) 7 = ".relocs")
    |> List.map (fun f ->
           (* Convert section_text.caml.foo.relocs -> .text.caml.foo *)
           let name_part = String.sub f 8 (String.length f - 15) in
           let section_name = "." ^ name_part in
           let relocs = read_binary_relocations binary_sections_dir name_part in
           section_name, relocs)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_elf = Compiler_owee.Owee_elf
module Owee_macho = Compiler_owee.Owee_macho

(* Module for Mach-O parsing using owee *)
module Macho = struct
  let find_segment commands seg_name =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SEGMENT_64 seg :: _
        when String.equal (Lazy.force seg).seg_segname seg_name ->
        Some (Lazy.force seg)
      | _ :: rest -> loop rest
    in
    loop commands

  let find_section segment sect_name =
    let sections = segment.Owee_macho.seg_sections in
    let rec loop i =
      if i >= Array.length sections
      then None
      else if String.equal sections.(i).sec_sectname sect_name
      then Some sections.(i)
      else loop (i + 1)
    in
    loop 0

  let extract_section buf commands ~seg_name ~sect_name =
    match find_segment commands seg_name with
    | None -> None
    | Some segment -> (
      match find_section segment sect_name with
      | None -> None
      | Some section ->
        let body = Owee_macho.section_body buf segment section in
        let cursor = Owee_buf.cursor body in
        let size = Owee_buf.size body in
        Some (Owee_buf.Read.fixed_string cursor size))

  (* Find section by name in any segment (for object files with unnamed
     segments) *)
  let find_section_any_segment commands sect_name =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SEGMENT_64 seg :: rest -> (
        let seg = Lazy.force seg in
        match find_section seg sect_name with
        | Some sec -> Some (seg, sec)
        | None -> loop rest)
      | _ :: rest -> loop rest
    in
    loop commands

  let extract_section_any buf commands ~sect_name =
    match find_section_any_segment commands sect_name with
    | None -> None
    | Some (segment, section) ->
      let body = Owee_macho.section_body buf segment section in
      let cursor = Owee_buf.cursor body in
      let size = Owee_buf.size body in
      Some (Owee_buf.Read.fixed_string cursor size)

  let extract_sections buf commands =
    (* Try named segment first (__TEXT), then any segment (for .o files) *)
    let text =
      match
        extract_section buf commands ~seg_name:"__TEXT" ~sect_name:"__text"
      with
      | Some _ as t -> t
      | None -> extract_section_any buf commands ~sect_name:"__text"
    in
    (* Try __DATA,__data first, then __DATA,__const, then any segment *)
    let data =
      match
        extract_section buf commands ~seg_name:"__DATA" ~sect_name:"__data"
      with
      | Some _ as d -> d
      | None -> (
        match
          extract_section buf commands ~seg_name:"__DATA" ~sect_name:"__const"
        with
        | Some _ as d -> d
        | None -> extract_section_any buf commands ~sect_name:"__data")
    in
    text, data

  (* Get symbol table from load commands *)
  let get_symbol_table commands =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SYMTAB syms :: _ -> Some (Lazy.force syms)
      | _ :: rest -> loop rest
    in
    loop commands

  (* Extract relocations from a section, resolving symbol names *)
  let extract_section_relocations symbols section =
    let relocs = section.Owee_macho.sec_relocs in
    Array.to_list relocs
    |> List.filter_map (fun reloc ->
           match reloc with
           | `Relocation_info ri when ri.Owee_macho.ri_extern ->
             let sym_idx = ri.Owee_macho.ri_symbolnum in
             if sym_idx < Array.length symbols
             then
               let sym = symbols.(sym_idx) in
               (* Mach-O uses implicit addends in the instruction/data *)
               Some
                 { be_offset = ri.Owee_macho.ri_address;
                   be_symbol = sym.Owee_macho.sym_name;
                   be_addend = 0L
                 }
             else None
           | _ -> None)

  (* Extract all relocations for text and data sections *)
  let extract_relocations commands =
    match get_symbol_table commands with
    | None -> [], []
    | Some (symbols, _strtab) ->
      let text_relocs = ref [] in
      let data_relocs = ref [] in
      List.iter
        (function
          | Owee_macho.LC_SEGMENT_64 seg ->
            let seg = Lazy.force seg in
            Array.iter
              (fun section ->
                let relocs = extract_section_relocations symbols section in
                if String.equal section.Owee_macho.sec_sectname "__text"
                then text_relocs := relocs @ !text_relocs
                else if String.equal section.Owee_macho.sec_sectname "__data"
                then data_relocs := relocs @ !data_relocs)
              seg.Owee_macho.seg_sections
          | _ -> ())
        commands;
      (* Sort by offset for comparison *)
      let sort_relocs =
        List.sort (fun a b -> compare a.be_offset b.be_offset)
      in
      sort_relocs !text_relocs, sort_relocs !data_relocs
end

(* Module for ELF parsing using owee *)
module Elf = struct
  let extract_section buf sections ~section_name =
    match Owee_elf.find_section sections section_name with
    | None -> None
    | Some section ->
      let body = Owee_elf.section_body buf section in
      let cursor = Owee_buf.cursor body in
      let size = Owee_buf.size body in
      Some (Owee_buf.Read.fixed_string cursor size)

  (* Check if a section name is a text section (includes .text.caml.* for
     function sections) *)
  let is_text_section_name name =
    String.length name >= 5 && String.sub name 0 5 = ".text"

  (* Extract all individual text sections as a list of (name, content) pairs.
     When function sections are enabled, code is split into
     .text.caml.<funcname> sections. We compare each individual section to
     ensure correctness. *)
  let extract_individual_text_sections buf sections =
    Array.to_list sections
    |> List.filter (fun sec -> is_text_section_name sec.Owee_elf.sh_name_str)
    |> List.map (fun sec ->
           let body = Owee_elf.section_body buf sec in
           let cursor = Owee_buf.cursor body in
           let size = Owee_buf.size body in
           sec.Owee_elf.sh_name_str, Owee_buf.Read.fixed_string cursor size)

  let extract_sections buf sections =
    (* Try .data first, then .rodata *)
    let data =
      match extract_section buf sections ~section_name:".data" with
      | Some _ as d -> d
      | None -> extract_section buf sections ~section_name:".rodata"
    in
    (* For text, if there's a single .text section, return it; otherwise return
       None and let the caller use individual sections *)
    let text_sections = extract_individual_text_sections buf sections in
    let text =
      match text_sections with
      | [(".text", content)] -> Some content
      | _ -> None (* Multiple sections or function sections *)
    in
    text, data

  (* Get all individual text sections from ELF *)
  let get_individual_text_sections buf sections =
    extract_individual_text_sections buf sections

  (* ELF relocation entry size for RELA (with addend): 24 bytes *)
  let rela_entry_size = 24

  (* Extract symbol index from r_info (upper 32 bits) *)
  let sym_index_of_r_info r_info =
    Int64.to_int (Int64.shift_right_logical r_info 32)

  (* Build a symbol name table from .symtab and .strtab sections. For section
     symbols (st_info type = STT_SECTION = 3), the name is derived from the
     section header table using st_shndx. Returns an array indexed by symbol
     index. *)
  let build_symbol_names buf sections =
    match
      ( Owee_elf.find_section sections ".symtab",
        Owee_elf.find_section sections ".strtab" )
    with
    | Some symtab_sec, Some strtab_sec ->
      let strtab_body = Owee_elf.section_body buf strtab_sec in
      let symtab_body = Owee_elf.section_body buf symtab_sec in
      let sym_entry_size = 24 in
      (* ELF64 symbol entry size *)
      let num_symbols = Owee_buf.size symtab_body / sym_entry_size in
      let names = Array.make num_symbols "" in
      for i = 0 to num_symbols - 1 do
        let cursor = Owee_buf.cursor symtab_body ~at:(i * sym_entry_size) in
        let st_name = Owee_buf.Read.u32 cursor in
        let st_info = Owee_buf.Read.u8 cursor in
        let _st_other = Owee_buf.Read.u8 cursor in
        let st_shndx = Owee_buf.Read.u16 cursor in
        let st_type = st_info land 0xf in
        let name =
          if st_type = 3 (* STT_SECTION *)
          then
            (* Section symbol: get name from section header table *)
            if st_shndx > 0 && st_shndx < Array.length sections
            then sections.(st_shndx).Owee_elf.sh_name_str
            else ""
          else
            (* Regular symbol: get name from string table *)
            let name_cursor = Owee_buf.cursor strtab_body ~at:st_name in
            match Owee_buf.Read.zero_string name_cursor () with
            | Some s -> s
            | None -> ""
        in
        names.(i) <- name
      done;
      Some names
    | _ -> None

  (* Build a symbol table mapping symbol name to (section_name, offset).
     This is used to resolve relocations that target different symbols
     but refer to the same location. *)
  let build_symbol_table buf sections =
    match
      ( Owee_elf.find_section sections ".symtab",
        Owee_elf.find_section sections ".strtab" )
    with
    | Some symtab_sec, Some strtab_sec ->
      let strtab_body = Owee_elf.section_body buf strtab_sec in
      let symtab_body = Owee_elf.section_body buf symtab_sec in
      let sym_entry_size = 24 in
      let num_symbols = Owee_buf.size symtab_body / sym_entry_size in
      let tbl = Hashtbl.create num_symbols in
      for i = 0 to num_symbols - 1 do
        let cursor = Owee_buf.cursor symtab_body ~at:(i * sym_entry_size) in
        let st_name = Owee_buf.Read.u32 cursor in
        let st_info = Owee_buf.Read.u8 cursor in
        let _st_other = Owee_buf.Read.u8 cursor in
        let st_shndx = Owee_buf.Read.u16 cursor in
        let st_value = Owee_buf.Read.u64 cursor in
        let st_type = st_info land 0xf in
        let name, section_name =
          if st_type = 3 (* STT_SECTION *)
          then
            if st_shndx > 0 && st_shndx < Array.length sections
            then
              let sec_name = sections.(st_shndx).Owee_elf.sh_name_str in
              sec_name, sec_name
            else "", ""
          else
            let sym_name =
              let name_cursor = Owee_buf.cursor strtab_body ~at:st_name in
              match Owee_buf.Read.zero_string name_cursor () with
              | Some s -> s
              | None -> ""
            in
            let sec_name =
              if st_shndx > 0 && st_shndx < Array.length sections
              then sections.(st_shndx).Owee_elf.sh_name_str
              else ""
            in
            sym_name, sec_name
        in
        if name <> ""
        then
          Hashtbl.add tbl name { sym_section = section_name; sym_offset = st_value }
      done;
      Some tbl
    | _ -> None

  (* Extract relocations from a RELA section *)
  let extract_rela_section buf sections symbol_names ~rela_section_name =
    match Owee_elf.find_section sections rela_section_name with
    | None -> []
    | Some rela_sec ->
      let rela_body = Owee_elf.section_body buf rela_sec in
      let num_entries = Owee_buf.size rela_body / rela_entry_size in
      let relocs = ref [] in
      for i = 0 to num_entries - 1 do
        let cursor = Owee_buf.cursor rela_body ~at:(i * rela_entry_size) in
        let r_offset = Owee_buf.Read.u64 cursor in
        let r_info = Owee_buf.Read.u64 cursor in
        let r_addend = Owee_buf.Read.u64 cursor in
        let sym_idx = sym_index_of_r_info r_info in
        let symbol_name =
          match symbol_names with
          | Some names when sym_idx < Array.length names -> names.(sym_idx)
          | _ -> Printf.sprintf "sym_%d" sym_idx
        in
        (* Only include non-empty symbol names (index 0 is usually empty) *)
        if symbol_name <> ""
        then
          relocs
            := { be_offset = Int64.to_int r_offset;
                 be_symbol = symbol_name;
                 be_addend = r_addend
               }
               :: !relocs
      done;
      (* Sort by offset for comparison *)
      List.sort (fun a b -> compare a.be_offset b.be_offset) !relocs

  (* Extract relocations from a RELA section with an offset adjustment *)
  let extract_rela_section_with_offset buf rela_sec symbol_names ~base_offset =
    let rela_body = Owee_elf.section_body buf rela_sec in
    let num_entries = Owee_buf.size rela_body / rela_entry_size in
    let relocs = ref [] in
    for i = 0 to num_entries - 1 do
      let cursor = Owee_buf.cursor rela_body ~at:(i * rela_entry_size) in
      let r_offset = Owee_buf.Read.u64 cursor in
      let r_info = Owee_buf.Read.u64 cursor in
      let r_addend = Owee_buf.Read.u64 cursor in
      let sym_idx = sym_index_of_r_info r_info in
      let symbol_name =
        match symbol_names with
        | Some names when sym_idx < Array.length names -> names.(sym_idx)
        | _ -> Printf.sprintf "sym_%d" sym_idx
      in
      if symbol_name <> ""
      then
        relocs
          := { be_offset = base_offset + Int64.to_int r_offset;
               be_symbol = symbol_name;
               be_addend = r_addend
             }
             :: !relocs
    done;
    !relocs

  (* Extract relocations for a specific section by name *)
  let extract_section_relocations buf sections symbol_names ~section_name =
    let rela_name = ".rela" ^ section_name in
    extract_rela_section buf sections symbol_names ~rela_section_name:rela_name

  (* Extract relocations for each individual text section as a list of
     (section_name, relocations) pairs *)
  let extract_individual_text_relocations buf sections =
    let symbol_names = build_symbol_names buf sections in
    Array.to_list sections
    |> List.filter (fun sec -> is_text_section_name sec.Owee_elf.sh_name_str)
    |> List.map (fun sec ->
           let relocs =
             extract_section_relocations buf sections symbol_names
               ~section_name:sec.Owee_elf.sh_name_str
           in
           sec.Owee_elf.sh_name_str, relocs)

  (* Extract all relocations for text and data sections. For text, returns
     relocations for the single .text section if it exists, otherwise empty. *)
  let extract_relocations buf sections =
    let symbol_names = build_symbol_names buf sections in
    (* Only get .text relocations if there's a single .text section *)
    let text_relocs =
      let text_sections =
        Array.to_list sections
        |> List.filter (fun sec ->
               is_text_section_name sec.Owee_elf.sh_name_str)
      in
      match text_sections with
      | [sec] when sec.Owee_elf.sh_name_str = ".text" ->
        extract_rela_section buf sections symbol_names
          ~rela_section_name:".rela.text"
      | _ -> [] (* Multiple sections; use individual extraction *)
    in
    let data_relocs =
      extract_rela_section buf sections symbol_names
        ~rela_section_name:".rela.data"
    in
    text_relocs, data_relocs

  (* Get all individual text section relocations from ELF *)
  let get_individual_text_relocations buf sections =
    extract_individual_text_relocations buf sections
end

(* Extract sections from object file using owee *)
let extract_obj_sections unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  (* Check magic to determine format *)
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" ->
    (* ELF format *)
    let _header, sections = Owee_elf.read_elf buf in
    Elf.extract_sections buf sections
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe" | "\xfe\xed\xfa\xce"
  | "\xce\xfa\xed\xfe" ->
    (* Mach-O format (32 or 64 bit, big or little endian) *)
    let _header, commands = Owee_macho.read buf in
    Macho.extract_sections buf commands
  | _ -> None, None

(* Extract relocations from object file using owee *)
let extract_obj_relocations unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe" | "\xfe\xed\xfa\xce"
  | "\xce\xfa\xed\xfe" ->
    (* Mach-O format *)
    let _header, commands = Owee_macho.read buf in
    Macho.extract_relocations commands
  | "\x7FELF" ->
    (* ELF format *)
    let _header, sections = Owee_elf.read_elf buf in
    Elf.extract_relocations buf sections
  | _ -> [], []

(* Extract individual text sections from ELF object file. Returns a list of
   (section_name, content) pairs for all .text* sections. *)
let extract_obj_individual_text_sections unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" ->
    let _header, sections = Owee_elf.read_elf buf in
    Elf.get_individual_text_sections buf sections
  | _ -> []

(* Extract individual text section relocations from ELF object file. Returns a
   list of (section_name, relocations) pairs for all .text* sections. *)
let extract_obj_individual_text_relocations unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" ->
    let _header, sections = Owee_elf.read_elf buf in
    Elf.get_individual_text_relocations buf sections
  | _ -> []

(* Extract symbol table from object file. Returns a hashtable mapping symbol
   names to their section and offset within that section. *)
let extract_obj_symbol_table unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" ->
    let _header, sections = Owee_elf.read_elf buf in
    Elf.build_symbol_table buf sections
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe" | "\xfe\xed\xfa\xce"
  | "\xce\xfa\xed\xfe" ->
    (* TODO: Mach-O symbol table extraction *)
    None
  | _ -> None

(* Resolve a relocation to its target location: (section_name, offset_in_section).
   Uses the symbol table to look up where the symbol is defined, then adds the
   addend to get the final offset. *)
let resolve_relocation sym_table (r : be_relocation) : string * int64 =
  match sym_table with
  | None ->
    (* No symbol table available; use symbol name as section, addend as offset *)
    r.be_symbol, r.be_addend
  | Some tbl -> (
    match Hashtbl.find_opt tbl r.be_symbol with
    | Some info ->
      (* Symbol found: section + (symbol's offset in section + addend) *)
      info.sym_section, Int64.add info.sym_offset r.be_addend
    | None ->
      (* Symbol not in table (e.g., external); use name as-is *)
      r.be_symbol, r.be_addend)

(* A resolved target: section name and offset within that section *)
type resolved_target =
  { rt_section : string;
    rt_offset : int64
  }

let compare_resolved (a : resolved_target) (b : resolved_target) =
  let c = String.compare a.rt_section b.rt_section in
  if c <> 0 then c else Int64.compare a.rt_offset b.rt_offset

(* Group relocations by offset, resolving each to its target location.
   Returns (offset, [resolved_targets]) pairs sorted by offset. *)
let group_relocations_by_offset sym_table relocs =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun r ->
      let section, offset = resolve_relocation sym_table r in
      let target = { rt_section = section; rt_offset = offset } in
      let existing = try Hashtbl.find tbl r.be_offset with Not_found -> [] in
      Hashtbl.replace tbl r.be_offset (target :: existing))
    relocs;
  let pairs =
    Hashtbl.fold (fun offset targets acc -> (offset, targets) :: acc) tbl []
  in
  (* Sort by offset, and sort targets within each group for stable comparison *)
  List.sort (fun (o1, _) (o2, _) -> compare o1 o2) pairs
  |> List.map (fun (offset, targets) ->
         offset, List.sort compare_resolved targets)

(* Format a resolved target for error messages *)
let format_resolved (r : resolved_target) =
  if r.rt_offset = 0L
  then r.rt_section
  else Printf.sprintf "%s+0x%Lx" r.rt_section r.rt_offset

let format_resolved_list targets =
  String.concat ", " (List.map format_resolved targets)

(* Compare two lists of relocations by resolving them to actual locations.
   Two relocations are equivalent if they resolve to the same (section, offset). *)
let compare_relocations sym_table ~section_name ~expected ~actual =
  let exp_grouped = group_relocations_by_offset sym_table expected in
  let act_grouped = group_relocations_by_offset sym_table actual in
  let rec loop exp act =
    match exp, act with
    | [], [] -> None
    | [], (offset, targets) :: _ ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = "(none)";
             actual = format_resolved_list targets
           })
    | (offset, targets) :: _, [] ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = format_resolved_list targets;
             actual = "(none)"
           })
    | (e_off, e_targets) :: erest, (a_off, a_targets) :: arest ->
      if e_off <> a_off
      then
        Some
          (Relocation
             { section_name;
               offset = min e_off a_off;
               expected =
                 Printf.sprintf "%s @ 0x%x" (format_resolved_list e_targets)
                   e_off;
               actual =
                 Printf.sprintf "%s @ 0x%x" (format_resolved_list a_targets)
                   a_off
             })
      else if not (List.equal (fun a b -> compare_resolved a b = 0)
                     e_targets a_targets)
      then
        Some
          (Relocation
             { section_name;
               offset = e_off;
               expected = format_resolved_list e_targets;
               actual = format_resolved_list a_targets
             })
      else loop erest arest
  in
  loop exp_grouped act_grouped

(* Compare individual text sections. Returns None if all match, or Some
   mismatch. *)
let compare_individual_text_sections ~be_sections ~asm_sections =
  (* Build a map of assembler sections for lookup *)
  let asm_map = Hashtbl.create (List.length asm_sections) in
  List.iter
    (fun (name, content) -> Hashtbl.add asm_map name content)
    asm_sections;
  (* Compare each binary emitter section against the corresponding assembler
     section *)
  let rec loop = function
    | [] -> None
    | (be_name, be_content) :: rest -> (
      match Hashtbl.find_opt asm_map be_name with
      | None -> Some (Missing_section (be_name ^ " (in object file)"))
      | Some asm_content -> (
        match
          compare_section ~section_name:be_name ~expected:be_content
            ~actual:asm_content
        with
        | Some mismatch -> Some mismatch
        | None -> loop rest))
  in
  loop be_sections

(* Compare individual text section relocations. *)
let compare_individual_text_relocations sym_table ~be_relocs ~asm_relocs =
  let asm_map = Hashtbl.create (List.length asm_relocs) in
  List.iter (fun (name, relocs) -> Hashtbl.add asm_map name relocs) asm_relocs;
  let rec loop = function
    | [] -> None
    | (be_name, be_relocs) :: rest -> (
      let asm_section_relocs =
        match Hashtbl.find_opt asm_map be_name with
        | None -> []
        | Some relocs -> relocs
      in
      match
        compare_relocations sym_table ~section_name:be_name ~expected:be_relocs
          ~actual:asm_section_relocs
      with
      | Some mismatch -> Some mismatch
      | None -> loop rest)
  in
  loop be_relocs

let compare unix ~obj_file ~binary_sections_dir =
  (* Check if binary sections directory exists *)
  if not (Sys.file_exists binary_sections_dir)
  then Mismatch (Missing_binary_sections_dir binary_sections_dir)
  else if not (Sys.is_directory binary_sections_dir)
  then Object_file_error (binary_sections_dir ^ " is not a directory")
  else
    (* Read binary emitter output *)
    let be_text = read_binary_section binary_sections_dir "text" in
    let be_data = read_binary_section binary_sections_dir "data" in
    (* Check for individual text sections (function sections) *)
    let be_individual_text = list_binary_text_sections binary_sections_dir in
    let has_individual_sections = List.length be_individual_text > 0 in
    (* Extract sections from object file *)
    let asm_text, asm_data =
      try extract_obj_sections unix obj_file
      with exn ->
        let msg =
          Printf.sprintf "Failed to read %s: %s" obj_file
            (Printexc.to_string exn)
        in
        raise (Failure msg)
    in
    (* Build symbol table for relocation resolution *)
    let sym_table =
      try extract_obj_symbol_table unix obj_file with _ -> None
    in
    (* Compare text sections - either individual or aggregate *)
    let text_result =
      if has_individual_sections
      then
        (* Individual function sections: compare each separately *)
        let asm_individual_text =
          try extract_obj_individual_text_sections unix obj_file with _ -> []
        in
        compare_individual_text_sections ~be_sections:be_individual_text
          ~asm_sections:asm_individual_text
      else
        (* Single text section *)
        match be_text, asm_text with
        | None, None -> None
        | Some _, None -> Some (Missing_section ".text (in object file)")
        | None, Some _ ->
          Some (Missing_section ".text (in binary emitter output)")
        | Some expected, Some actual ->
          compare_section ~section_name:"text" ~expected ~actual
    in
    match text_result with
    | Some mismatch -> Mismatch mismatch
    | None -> (
      (* Compare data section *)
      let data_result =
        match be_data, asm_data with
        | None, None -> None
        | Some _, None ->
          (* Data section may be legitimately missing in object file *)
          None
        | None, Some _ ->
          (* Binary emitter didn't produce data, but assembler did - skip *)
          None
        | Some expected, Some actual ->
          compare_section ~section_name:"data" ~expected ~actual
      in
      match data_result with
      | Some mismatch -> Mismatch mismatch
      | None -> (
        (* Compare relocations *)
        let text_reloc_result =
          if has_individual_sections
          then
            (* Individual function sections: compare each separately *)
            let be_individual_relocs =
              list_binary_text_relocations binary_sections_dir
            in
            let asm_individual_relocs =
              try extract_obj_individual_text_relocations unix obj_file
              with _ -> []
            in
            compare_individual_text_relocations sym_table
              ~be_relocs:be_individual_relocs ~asm_relocs:asm_individual_relocs
          else
            let be_text_relocs =
              read_binary_relocations binary_sections_dir "text"
            in
            let asm_text_relocs, _ =
              try extract_obj_relocations unix obj_file with _ -> [], []
            in
            compare_relocations sym_table ~section_name:"text"
              ~expected:be_text_relocs ~actual:asm_text_relocs
        in
        let be_data_relocs =
          read_binary_relocations binary_sections_dir "data"
        in
        let _, asm_data_relocs =
          try extract_obj_relocations unix obj_file with _ -> [], []
        in
        match text_reloc_result with
        | Some mismatch -> Mismatch mismatch
        | None -> (
          let data_reloc_result =
            compare_relocations sym_table ~section_name:"data"
              ~expected:be_data_relocs ~actual:asm_data_relocs
          in
          match data_reloc_result with
          | Some mismatch -> Mismatch mismatch
          | None ->
            let text_size = Option.fold ~none:0 ~some:String.length be_text in
            let data_size = Option.fold ~none:0 ~some:String.length be_data in
            Match { text_size; data_size })))

let print_result ppf = function
  | Match { text_size; data_size } ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification passed@,\
       text: %d bytes, data: %d bytes@]@." text_size data_size
  | Mismatch (Section_content m) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Assembler size:      %d bytes@,\
       Binary emitter size: %d bytes@,\
       @,\
       First difference at byte offset 0x%x%s:@,\
       @,\
       Assembler bytes:@,\
      \  %s@,\
       @,\
       Binary emitter bytes:@,\
      \  %s@]@." m.section_name m.actual_size m.expected_size m.byte_offset
      (match m.instruction_offset with
      | Some off when off <> m.byte_offset ->
        Printf.sprintf " (instruction at 0x%x)" off
      | _ -> "")
      m.actual m.expected
  | Mismatch (Section_size { section_name; expected; actual }) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Size mismatch: assembler produced %d bytes, binary emitter produced %d \
       bytes@]@."
      section_name actual expected
  | Mismatch (Relocation r) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Relocation mismatch in %s at offset 0x%x:@,\
       Assembler:      %s@,\
       Binary emitter: %s@]@." r.section_name r.offset r.actual r.expected
  | Mismatch (Missing_section name) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,Missing section: %s@]@." name
  | Mismatch (Missing_binary_sections_dir dir) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Binary sections directory not found: %s@,\
       (Did the binary emitter run?)@]@." dir
  | Object_file_error msg ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Error reading object file: %s@]@." msg
