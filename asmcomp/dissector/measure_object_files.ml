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

type error =
  | File_not_found of string
  | Duplicate_file of string

exception Error of error

let report_error ppf = function
  | File_not_found filename ->
    Format.fprintf ppf "Dissector: file not found: %s" filename
  | Duplicate_file filename ->
    Format.fprintf ppf "Dissector: duplicate file in link: %s" filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

(* Compute the total allocated section size for a single ELF buffer *)
let allocated_size_of_elf_buf buf =
  let _header, sections = Compiler_owee.Owee_elf.read_elf buf in
  Array.fold_left
    (fun acc (section : Compiler_owee.Owee_elf.section) ->
      if Compiler_owee.Owee_elf.Section_flags.is_alloc section.sh_flags
      then Int64.add acc section.sh_size
      else acc)
    0L sections

(* Read a .cmxa file and return its library_infos *)
let read_cmxa filename : Cmx_format.library_infos =
  let chan = open_in_bin filename in
  let _magic =
    really_input_string chan (String.length Config.cmxa_magic_number)
  in
  let cmxa : Cmx_format.library_infos = input_value chan in
  close_in chan;
  cmxa

(* Check for duplicate files in the input list *)
let check_for_duplicates files =
  let seen = Hashtbl.create 256 in
  List.iter
    (fun file ->
      if Hashtbl.mem seen file
      then raise (Error (Duplicate_file file))
      else Hashtbl.add seen file ())
    files

let total_allocated_section_size (unix : (module Compiler_owee.Unix_intf.S))
    ~files =
  (* Check for duplicates in the input list first *)
  check_for_duplicates files;
  let module Unix = (val unix) in
  (* Analyze a single .o file *)
  let allocated_size_of_object_file filename =
    if not (Sys.file_exists filename)
    then raise (Error (File_not_found filename))
    else
      let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
      allocated_size_of_elf_buf buf
  in
  (* Analyze an archive (.a) file *)
  let allocated_size_of_archive_file filename =
    if not (Sys.file_exists filename)
    then raise (Error (File_not_found filename))
    else
      let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
      let archive, members = Compiler_owee.Owee_archive.read buf in
      List.fold_left
        (fun acc member ->
          if Filename.check_suffix member.Compiler_owee.Owee_archive.name ".o"
          then
            let member_buf =
              Compiler_owee.Owee_archive.member_body archive member
            in
            Int64.add acc (allocated_size_of_elf_buf member_buf)
          else acc)
        0L members
  in
  (* Track which files we've already analyzed to avoid double-counting from
     transitive dependencies (e.g., lib_ccobjs in .cmxa files) *)
  let analyzed = Hashtbl.create 256 in
  (* Analyze a single file based on its extension *)
  let rec analyze_one filename =
    if Hashtbl.mem analyzed filename
    then 0L
    else (
      Hashtbl.add analyzed filename ();
      if Filename.check_suffix filename ".o"
      then allocated_size_of_object_file filename
      else if Filename.check_suffix filename ".a"
      then allocated_size_of_archive_file filename
      else if Filename.check_suffix filename ".cmx"
      then
        let obj_file = Filename.chop_suffix filename ".cmx" ^ ".o" in
        if Hashtbl.mem analyzed obj_file
        then 0L
        else (
          Hashtbl.add analyzed obj_file ();
          allocated_size_of_object_file obj_file)
      else if Filename.check_suffix filename ".cmxa"
      then
        let archive_file = Filename.chop_suffix filename ".cmxa" ^ ".a" in
        let archive_size =
          if Hashtbl.mem analyzed archive_file
          then 0L
          else (
            Hashtbl.add analyzed archive_file ();
            allocated_size_of_archive_file archive_file)
        in
        let cmxa = read_cmxa filename in
        let ccobjs_size =
          List.fold_left
            (fun acc ccobj -> Int64.add acc (analyze_one ccobj))
            0L cmxa.lib_ccobjs
        in
        Int64.add archive_size ccobjs_size
      else 0L)
  in
  List.fold_left (fun acc file -> Int64.add acc (analyze_one file)) 0L files
