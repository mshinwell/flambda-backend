(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

type normal_or_dwo =
  | Normal
  | Dwo

type dwarf_section =
  | Debug_info of normal_or_dwo
  | Debug_abbrev of normal_or_dwo
  | Debug_aranges
  | Debug_addr
  | Debug_loclists of normal_or_dwo
  | Debug_rnglists of normal_or_dwo
  | Debug_str
  | Debug_line of normal_or_dwo
  | Debug_str_offsets of normal_or_dwo
  | Debug_macro of normal_or_dwo

type t =
  | DWARF of dwarf_section
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | Text
  | Stapsdt_base
  | Stapsdt_note
  | Probes
  | Note_ocaml_eh

let dwarf_sections_in_order normal_or_dwo =
  let sections =
    [ DWARF (Debug_info normal_or_dwo);
      DWARF (Debug_abbrev normal_or_dwo);
      DWARF Debug_aranges;
      DWARF Debug_str;
      DWARF (Debug_line normal_or_dwo);
      DWARF (Debug_str_offsets normal_or_dwo);
      DWARF (Debug_macro normal_or_dwo) ]
  in
  let dwarf_version_dependent_sections =
    match !Dwarf_flags.gdwarf_version with
    | Five ->
      [ DWARF Debug_addr;
        DWARF (Debug_loclists normal_or_dwo);
        DWARF (Debug_rnglists normal_or_dwo) ]
  in
  sections @ dwarf_version_dependent_sections

let is_delayed = function
  (* Only .debug_line and .debug_frames are delayed. All other sections should
     be emitted directly. See PR #1719. *)
  | DWARF (Debug_line _) -> true
  | DWARF
      ( Debug_info _ | Debug_abbrev _ | Debug_aranges | Debug_str
      | Debug_loclists _ | Debug_rnglists _ | Debug_addr | Debug_str_offsets _
      | Debug_macro _ )
  | Data | Read_only_data | Eight_byte_literals | Sixteen_byte_literals
  | Jump_tables | Text | Stapsdt_base | Stapsdt_note | Probes | Note_ocaml_eh ->
    false

let print ppf t =
  let str =
    match t with
    | DWARF (Debug_info Normal) -> "(DWARF Debug_info)"
    | DWARF (Debug_info Dwo) -> "(DWARF Debug_info Dwo)"
    | DWARF (Debug_abbrev Normal) -> "(DWARF Debug_abbrev)"
    | DWARF (Debug_abbrev Dwo) -> "(DWARF Debug_abbrev Dwo)"
    | DWARF Debug_aranges -> "(DWARF Debug_aranges)"
    | DWARF Debug_addr -> "(DWARF Debug_addr)"
    | DWARF (Debug_loclists Normal) -> "(DWARF Debug_loclists)"
    | DWARF (Debug_loclists Dwo) -> "(DWARF Debug_loclists Dwo)"
    | DWARF (Debug_rnglists Normal) -> "(DWARF Debug_rnglists)"
    | DWARF (Debug_rnglists Dwo) -> "(DWARF Debug_rnglists Dwo)"
    | DWARF Debug_str -> "(DWARF Debug_str)"
    | DWARF (Debug_line Normal) -> "(DWARF Debug_line)"
    | DWARF (Debug_line Dwo) -> "(DWARF Debug_line)"
    | DWARF (Debug_str_offsets Normal) -> "(DWARF Debug_str_offsets)"
    | DWARF (Debug_str_offsets Dwo) -> "(DWARF Debug_str_offsets Dwo)"
    | DWARF (Debug_macro Normal) -> "(DWARF Debug_macro)"
    | DWARF (Debug_macro Dwo) -> "(DWARF Debug_macro Dwo)"
    | Data -> "Data"
    | Read_only_data -> "Read_only_data"
    | Eight_byte_literals -> "Eight_byte_literals"
    | Sixteen_byte_literals -> "Sixteen_byte_literals"
    | Jump_tables -> "Jump_tables"
    | Text -> "Text"
    | Stapsdt_base -> "Stapsdt_base"
    | Stapsdt_note -> "Stapsdt_note"
    | Probes -> "Probes"
    | Note_ocaml_eh -> "Note_ocaml_eh"
  in
  Format.pp_print_string ppf str

let compare t1 t2 = Stdlib.compare t1 t2

let equal t1 t2 = Stdlib.compare t1 t2 = 0

let section_is_text = function
  | Text -> true
  | Data | Read_only_data | Eight_byte_literals | Sixteen_byte_literals
  | Jump_tables | DWARF _ | Stapsdt_base | Stapsdt_note | Probes | Note_ocaml_eh
    ->
    false

type section_details =
  { names : string list;
    flags : string option;
    args : string list;
    is_delayed : bool
  }

let details t ~first_occurrence =
  let text () = [".text"], None, [] in
  let data () = [".data"], None, [] in
  let rodata () = [".rodata"], None, [] in
  let system = Target_system.derived_system () in
  let names, flags, args =
    match t, Target_system.architecture (), system with
    | Text, _, _ -> text ()
    | Data, _, _ -> data ()
    | DWARF dwarf, _, MacOS_like ->
      let name =
        match dwarf with
        | Debug_info Normal -> "__debug_info"
        | Debug_abbrev Normal -> "__debug_abbrev"
        | Debug_aranges -> "__debug_aranges"
        | Debug_addr -> "__debug_addr"
        | Debug_loclists Normal -> "__debug_loclists"
        | Debug_rnglists Normal -> "__debug_rnglists"
        | Debug_str -> "__debug_str"
        | Debug_line Normal -> "__debug_line"
        | Debug_str_offsets Normal -> "__debug_line"
        | Debug_macro Normal -> "__debug_line"
        | Debug_info Dwo -> "__debug_info.dwo"
        | Debug_abbrev Dwo -> "__debug_abbrev.dwo"
        | Debug_line Dwo -> "__debug_line.dwo"
        | Debug_loclists Dwo -> "__debug_loclists.dwo"
        | Debug_rnglists Dwo -> "__debug_rnglists.dwo"
        | Debug_str_offsets Dwo -> "__debug_str.dwo"
        | Debug_macro Dwo -> "__debug_line.dwo"
      in
      ["__DWARF"; name], None, ["regular"; "debug"]
    | DWARF dwarf, _, _ ->
      let name, dwo =
        match dwarf with
        | Debug_info Normal -> ".debug_info", Normal
        | Debug_abbrev Normal -> ".debug_abbrev", Normal
        | Debug_aranges -> ".debug_aranges", Normal
        | Debug_addr -> ".debug_addr", Normal
        | Debug_loclists Normal -> ".debug_loclists", Normal
        | Debug_rnglists Normal -> ".debug_rnglists", Normal
        | Debug_str -> ".debug_str", Normal
        | Debug_line Normal -> ".debug_line", Normal
        | Debug_str_offsets Normal -> ".debug_line", Normal
        | Debug_macro Normal -> ".debug_line", Normal
        | Debug_info Dwo -> ".debug_info.dwo", Dwo
        | Debug_abbrev Dwo -> ".debug_abbrev.dwo", Dwo
        | Debug_line Dwo -> ".debug_line.dwo", Dwo
        | Debug_loclists Dwo -> ".debug_loclists.dwo", Dwo
        | Debug_rnglists Dwo -> ".debug_rnglists.dwo", Dwo
        | Debug_str_offsets Dwo -> ".debug_str.dwo", Dwo
        | Debug_macro Dwo -> ".debug_line.dwo", Dwo
      in
      let flags =
        let flags =
          match first_occurrence, dwarf with
          | true, Debug_str -> Some "MS" (* #3078 *)
          | true, _ -> Some ""
          | false, _ -> None
        in
        match dwo with
        | Normal -> flags
        | Dwo ->
          (* Set SHF_EXCLUDE to ensure a linker never processes these (although
             it shouldn't see them under the ocamlopt split DWARF scheme). *)
          Some (Option.value flags ~default:"" ^ "e")
      in
      let args =
        match first_occurrence, dwarf with
        | true, Debug_str -> ["%progbits,1"] (* #3078 *)
        | true, _ -> ["%progbits"]
        | false, _ -> []
      in
      [name], flags, args
    (* Eight Byte Literals; based on corresponding upstream secions *)
    | Eight_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal8"], None, ["8byte_literals"]
    | Eight_byte_literals, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Eight_byte_literals, _, Win64 -> data ()
    | Eight_byte_literals, _, _ ->
      [".rodata.cst8"], Some "aM", ["@progbits"; "8"]
    (* Sixteen Byte Literals; based on corresponding upstream secions *)
    | Sixteen_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal16"], None, ["16byte_literals"]
    | Sixteen_byte_literals, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Sixteen_byte_literals, _, Win64 -> data ()
    | Sixteen_byte_literals, _, _ ->
      [".rodata.cst16"], Some "aM", ["@progbits"; "16"]
    | Jump_tables, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Jump_tables, _, (MinGW_32 | Win32) -> data ()
    | Jump_tables, _, (MacOS_like | Win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _, _ -> [".rodata"], None, []
    | Read_only_data, _, (MinGW_32 | Win32) -> data ()
    | Read_only_data, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Read_only_data, _, _ -> rodata ()
    | Stapsdt_base, _, Linux ->
      [".stapsdt.base"], Some "aG", ["\"progbits\""; ".stapsdt.base"; "comdat"]
    | Stapsdt_base, _, _ ->
      Misc.fatal_error "stapsdt not supported on platforms other than Linux."
    | Stapsdt_note, _, MacOS_like ->
      ["__DATA"; "__note_stapsdt"], None, ["regular"]
      (* NOTE: This is section is currently not tested. *)
    | Stapsdt_note, _, (GNU | Solaris | Linux | Generic_BSD | BeOS) ->
      [".note.stapsdt"], Some "?", ["\"note\""]
    | Stapsdt_note, _, _ ->
      Misc.fatal_error "Target systems does not support stapsdt."
    | Probes, _, MacOS_like -> ["__TEXT"; "__probes"], None, ["regular"]
    | Probes, _, _ -> [".probes"], Some "wa", ["\"progbits\""]
    | Note_ocaml_eh, _, _ -> [".note.ocaml_eh"], Some "?", ["\"note\""]
  in
  let is_delayed = is_delayed t in
  { names; flags; args; is_delayed }

let to_string t =
  let { names; flags = _; args = _; is_delayed = _ } =
    details t ~first_occurrence:true
  in
  String.concat " " names
