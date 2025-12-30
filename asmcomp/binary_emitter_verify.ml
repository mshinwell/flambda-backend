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

(* Binary emitter verification: compare binary emitter output against system
   assembler output to ensure they produce identical machine code. *)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_object = Compiler_owee.Owee_object

(* Result types *)

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

(* Local relocation type matching Owee_object.relocation *)
type relocation =
  { r_offset : int;
    r_symbol : string;
    r_addend : int64
  }

(* Text sections from binary emitter *)
type text_sections =
  | No_function_sections of string option
  | Function_sections of (string * string) list

(* Text relocations from binary emitter *)
type text_relocations =
  | No_function_section_relocs of relocation list
  | Function_section_relocs of (string * relocation list) list

(* Helper functions *)

let read_file_bytes filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

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

let instruction_size () =
  match Target_system.architecture () with
  | AArch64 -> Some 4
  | X86_64 -> None
  | _ -> None

let align_to_instruction offset =
  match instruction_size () with
  | Some size -> offset / size * size
  | None -> offset

(* Reading binary emitter output from .binary-sections directory *)

let read_binary_section binary_sections_dir section_name =
  let filename =
    Filename.concat binary_sections_dir ("section_" ^ section_name ^ ".bin")
  in
  if Sys.file_exists filename then Some (read_file_bytes filename) else None

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
             := { r_offset = offset; r_symbol = symbol; r_addend = 0L }
                :: !relocs
         | [offset_str; symbol; addend_str] ->
           let offset = int_of_string offset_str in
           let addend = Int64.of_string addend_str in
           relocs
             := { r_offset = offset; r_symbol = symbol; r_addend = addend }
                :: !relocs
         | _ -> ()
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !relocs

let section_name_of_filename filename =
  if String.length filename > 12
     && String.sub filename 0 8 = "section_"
     && String.sub filename (String.length filename - 4) 4 = ".bin"
  then
    let name_part = String.sub filename 8 (String.length filename - 12) in
    "." ^ name_part
  else filename

let read_binary_text_sections binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then No_function_sections None
  else if not (Sys.is_directory binary_sections_dir)
  then No_function_sections None
  else
    let files = Sys.readdir binary_sections_dir in
    let individual_sections =
      Array.to_list files
      |> List.filter (fun f ->
             String.length f > 17
             && String.sub f 0 13 = "section_text."
             && String.sub f (String.length f - 4) 4 = ".bin"
             && f <> "section_text.bin")
      |> List.map (fun f ->
             let section_name = section_name_of_filename f in
             let content =
               read_file_bytes (Filename.concat binary_sections_dir f)
             in
             section_name, content)
    in
    if List.length individual_sections > 0
    then Function_sections individual_sections
    else No_function_sections (read_binary_section binary_sections_dir "text")

let read_binary_text_relocations binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then No_function_section_relocs []
  else if not (Sys.is_directory binary_sections_dir)
  then No_function_section_relocs []
  else
    let files = Sys.readdir binary_sections_dir in
    let individual_relocs =
      Array.to_list files
      |> List.filter (fun f ->
             String.length f > 20
             && String.sub f 0 13 = "section_text."
             && String.sub f (String.length f - 7) 7 = ".relocs"
             && f <> "section_text.relocs")
      |> List.map (fun f ->
             let name_part = String.sub f 8 (String.length f - 15) in
             let section_name = "." ^ name_part in
             let relocs =
               read_binary_relocations binary_sections_dir name_part
             in
             section_name, relocs)
    in
    if List.length individual_relocs > 0
    then Function_section_relocs individual_relocs
    else
      No_function_section_relocs
        (read_binary_relocations binary_sections_dir "text")

(* Convert Owee_object.relocation to local relocation type *)
let convert_reloc (r : Owee_object.relocation) : relocation =
  { r_offset = r.r_offset; r_symbol = r.r_symbol; r_addend = r.r_addend }

(* Comparison functions *)

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

let format_reloc r =
  if r.r_addend = 0L
  then r.r_symbol
  else Printf.sprintf "%s+0x%Lx" r.r_symbol r.r_addend

let relocs_equal ~is_rela e a =
  if String.equal e.r_symbol a.r_symbol
  then if is_rela then e.r_addend = a.r_addend else true
  else false

let group_relocations_by_offset relocs =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun r ->
      let existing = try Hashtbl.find tbl r.r_offset with Not_found -> [] in
      Hashtbl.replace tbl r.r_offset (r :: existing))
    relocs;
  let pairs = Hashtbl.fold (fun offset rs acc -> (offset, rs) :: acc) tbl [] in
  let compare_reloc a b =
    let c = String.compare a.r_symbol b.r_symbol in
    if c <> 0 then c else Int64.compare a.r_addend b.r_addend
  in
  List.sort (fun (o1, _) (o2, _) -> compare o1 o2) pairs
  |> List.map (fun (offset, rs) -> offset, List.sort compare_reloc rs)

let format_reloc_list relocs = String.concat ", " (List.map format_reloc relocs)

let compare_relocations ~is_rela ~section_name ~expected ~actual =
  let exp_grouped = group_relocations_by_offset expected in
  let act_grouped = group_relocations_by_offset actual in
  let rec loop exp act =
    match exp, act with
    | [], [] -> None
    | [], (offset, relocs) :: _ ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = "(none)";
             actual = format_reloc_list relocs
           })
    | (offset, relocs) :: _, [] ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = format_reloc_list relocs;
             actual = "(none)"
           })
    | (e_off, e_relocs) :: erest, (a_off, a_relocs) :: arest ->
      if e_off <> a_off
      then
        Some
          (Relocation
             { section_name;
               offset = min e_off a_off;
               expected =
                 Printf.sprintf "%s @ 0x%x" (format_reloc_list e_relocs) e_off;
               actual =
                 Printf.sprintf "%s @ 0x%x" (format_reloc_list a_relocs) a_off
             })
      else if not (List.equal (relocs_equal ~is_rela) e_relocs a_relocs)
      then
        Some
          (Relocation
             { section_name;
               offset = e_off;
               expected = format_reloc_list e_relocs;
               actual = format_reloc_list a_relocs
             })
      else loop erest arest
  in
  loop exp_grouped act_grouped

let compare_individual_text_sections ~be_sections ~asm_sections =
  let asm_map = Hashtbl.create (List.length asm_sections) in
  List.iter
    (fun (name, content) -> Hashtbl.add asm_map name content)
    asm_sections;
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

let compare_individual_text_relocations ~is_rela ~be_relocs ~asm_relocs =
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
        compare_relocations ~is_rela ~section_name:be_name ~expected:be_relocs
          ~actual:asm_section_relocs
      with
      | Some mismatch -> Some mismatch
      | None -> loop rest)
  in
  loop be_relocs

(* Main comparison function *)

let compare unix ~obj_file ~binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then Mismatch (Missing_binary_sections_dir binary_sections_dir)
  else if not (Sys.is_directory binary_sections_dir)
  then Object_file_error (binary_sections_dir ^ " is not a directory")
  else
    let be_text_sections = read_binary_text_sections binary_sections_dir in
    let be_data = read_binary_section binary_sections_dir "data" in
    let buf =
      try Owee_buf.map_binary unix obj_file
      with exn ->
        let msg =
          Printf.sprintf "Failed to read %s: %s" obj_file
            (Printexc.to_string exn)
        in
        raise (Failure msg)
    in
    let asm_text = Owee_object.extract_text_section buf in
    let asm_data = Owee_object.extract_data_section buf in
    let is_rela = Owee_object.uses_rela_relocations buf in
    let text_result =
      match be_text_sections with
      | Function_sections be_individual_text ->
        let asm_individual_text =
          Owee_object.extract_individual_text_sections buf
        in
        compare_individual_text_sections ~be_sections:be_individual_text
          ~asm_sections:asm_individual_text
      | No_function_sections be_text -> (
        match be_text, asm_text with
        | None, None -> None
        | Some _, None -> Some (Missing_section ".text (in object file)")
        | None, Some _ ->
          Some (Missing_section ".text (in binary emitter output)")
        | Some expected, Some actual ->
          compare_section ~section_name:"text" ~expected ~actual)
    in
    match text_result with
    | Some mismatch -> Mismatch mismatch
    | None -> (
      let data_result =
        match be_data, asm_data with
        | None, None -> None
        | Some _, None -> None
        | None, Some _ -> None
        | Some expected, Some actual ->
          compare_section ~section_name:"data" ~expected ~actual
      in
      match data_result with
      | Some mismatch -> Mismatch mismatch
      | None -> (
        let be_text_relocs = read_binary_text_relocations binary_sections_dir in
        let text_reloc_result =
          match be_text_relocs with
          | Function_section_relocs be_individual_relocs ->
            let asm_individual_relocs =
              Owee_object.extract_individual_text_relocations buf
              |> List.map (fun (name, relocs) ->
                     name, List.map convert_reloc relocs)
            in
            compare_individual_text_relocations ~is_rela
              ~be_relocs:be_individual_relocs ~asm_relocs:asm_individual_relocs
          | No_function_section_relocs be_relocs ->
            let asm_text_relocs =
              Owee_object.extract_text_relocations buf |> List.map convert_reloc
            in
            compare_relocations ~is_rela ~section_name:"text"
              ~expected:be_relocs ~actual:asm_text_relocs
        in
        let be_data_relocs =
          read_binary_relocations binary_sections_dir "data"
        in
        let asm_data_relocs =
          Owee_object.extract_data_relocations buf |> List.map convert_reloc
        in
        match text_reloc_result with
        | Some mismatch -> Mismatch mismatch
        | None -> (
          let data_reloc_result =
            compare_relocations ~is_rela ~section_name:"data"
              ~expected:be_data_relocs ~actual:asm_data_relocs
          in
          match data_reloc_result with
          | Some mismatch -> Mismatch mismatch
          | None ->
            let text_size =
              match be_text_sections with
              | No_function_sections (Some s) -> String.length s
              | No_function_sections None -> 0
              | Function_sections sections ->
                List.fold_left
                  (fun acc (_, content) -> acc + String.length content)
                  0 sections
            in
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
