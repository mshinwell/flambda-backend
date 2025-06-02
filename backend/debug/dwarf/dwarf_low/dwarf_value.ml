(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Review again, especially the distance-measuring parts. *)

(** Values written into DWARF sections. (For attribute values, see
    [Dwarf_attribute_values].) *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare
open Asm_targets
module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module Uint8 = Numbers.Uint8
module Uint16 = Numbers.Uint16
module Uint32 = Numbers.Uint32
module Uint64 = Numbers.Uint64
module A = Asm_directives

type not_in_dwo =
  | Offset_into_debug_addr of Address_index.t
  | Address_table_entry_from_label_symbol_diff of
      { upper : Asm_label.t;
        lower : Asm_symbol.t;
        offset_upper : Targetint.t
      }
  | Address_table_entry_from_label_label_diff of
      { upper : Asm_label.t;
        lower : Asm_label.t;
        offset_upper : Targetint.t
      }

type value =
  | Flag_true
  | Bool of bool
  | Int8 of Int8.t
  | Int16 of Int16.t
  | Int32 of Int32.t
  | Int64 of Int64.t
  | Uint8 of Uint8.t
  | Uint16 of Uint16.t
  | Uint32 of Uint32.t
  | Uint64 of Uint64.t
  | Uleb128 of Uint64.t
  | Sleb128 of Int64.t
  | String of string
  | Indirect_string of string
  | Absolute_address of Targetint.t
  | Address_index of Address_index.t
  | Offset_into_debug_info of Address_index.t
  | Offset_into_debug_line of Address_index.t
  | Offset_into_debug_loclists of Address_index.t
  | Offset_into_debug_rnglists of Address_index.t
  | Offset_into_debug_abbrev of Address_index.t
  | Not_in_dwo of not_in_dwo

type t =
  { value : value;
    comment : string option
  }

let print ppf { value; comment = _ } =
  let dwo = function Asm_section.Normal -> "" | Asm_section.Dwo -> ".dwo" in
  match value with
  | Flag_true -> Format.pp_print_string ppf "true"
  | Bool b -> Format.fprintf ppf "%b" b
  | Int8 i -> Int8.print ppf i
  | Int16 i -> Int16.print ppf i
  | Int32 i -> Format.fprintf ppf "%ld" i
  | Int64 i -> Format.fprintf ppf "%Ld" i
  | Uint8 i -> Uint8.print ppf i
  | Uint16 i -> Uint16.print ppf i
  | Uint32 i -> Uint32.print ppf i
  | Uint64 i -> Uint64.print ppf i
  | Uleb128 i -> Format.fprintf ppf "(uleb128 %a)" Uint64.print i
  | Sleb128 i -> Format.fprintf ppf "(sleb128 %Ld)" i
  | String str -> Format.fprintf ppf "\"%S\"" str
  | Indirect_string str -> Format.fprintf ppf "\"%S\" [indirect]" str
  | Absolute_address addr ->
    Format.fprintf ppf "0x%Lx" (Targetint.to_int64 addr)
  | Address_index addr_index -> Address_index.print ppf addr_index
  | Offset_into_debug_info (normal_or_dwo, addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_info%s %a)" (dwo normal_or_dwo)
      Address_index.print addr_index
  | Offset_into_debug_line (normal_or_dwo, addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_line%s %a)" (dwo normal_or_dwo)
      Address_index.print addr_index
  | Offset_into_debug_loclists (normal_or_dwo, addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_loclists%s %a)" (dwo normal_or_dwo)
      Address_index.print addr_index
  | Offset_into_debug_rnglists (normal_or_dwo, addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_rnglists%s %a)" (dwo normal_or_dwo)
      Address_index.print addr_index
  | Offset_into_debug_abbrev (normal_or_dwo, addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_abbrev%s %a)" (dwo normal_or_dwo)
      Address_index.print addr_index
  | Not_in_dwo (Offset_into_debug_addr addr_index) ->
    Format.fprintf ppf "(Offset_into_debug_addr %a)" Address_index.print
      addr_index
  | Not_in_dwo
      (Address_table_entry_from_label_symbol_diff
        { upper; lower; offset_upper }) ->
    Format.fprintf ppf
      "(Address_table_entry_from_label_symbol_diff@ (lower %a)@ (upper %a)@ \
       (offset_upper@ %a))"
      Asm_symbol.print lower Asm_label.print upper Targetint.print offset_upper
  | Not_in_dwo
      (Address_table_entry_from_label_label_diff { upper; lower; offset_upper })
    ->
    Format.fprintf ppf
      "(Address_table_entry_from_label_label_diff@ (lower %a)@ (upper %a)@ \
       (offset_upper@ %a))"
      Asm_label.print lower Asm_label.print upper Targetint.print offset_upper

let flag_true ?comment () = { value = Flag_true; comment }

let bool ?comment b = { value = Bool b; comment }

let int8 ?comment i = { value = Int8 i; comment }

let int16 ?comment i = { value = Int16 i; comment }

let int32 ?comment i = { value = Int32 i; comment }

let int64 ?comment i = { value = Int64 i; comment }

let uint8 ?comment i = { value = Uint8 i; comment }

let uint16 ?comment i = { value = Uint16 i; comment }

let uint32 ?comment i = { value = Uint32 i; comment }

let uint64 ?comment i = { value = Uint64 i; comment }

let uleb128 ?comment i = { value = Uleb128 i; comment }

let sleb128 ?comment i = { value = Sleb128 i; comment }

let string ?comment str = { value = String str; comment }

let indirect_string ?comment str = { value = Indirect_string str; comment }

let absolute_address ?comment addr = { value = Absolute_address addr; comment }

module Not_in_dwo = struct
  let offset_into_debug_addr ?comment lbl =
    { value = Offset_into_debug_addr lbl; comment }

  let address_table_entry_from_label_symbol_diff ?comment ~upper ~lower
      ~offset_upper () =
    { value =
        Address_table_entry_from_label_symbol_diff
          { upper; lower; offset_upper };
      comment
    }
end

let offset_into_debug_info ?comment addr_index =
  { value = Offset_into_debug_info addr_index; comment }

let offset_into_debug_line ?comment addr_index =
  { value = Offset_into_debug_line addr_index; comment }

let offset_into_debug_loclists ?comment addr_index =
  { value = Offset_into_debug_loclists addr_index; comment }

let offset_into_debug_rnglists ?comment addr_index =
  { value = Offset_into_debug_rnglists addr_index; comment }

let offset_into_debug_abbrev ?comment addr_index =
  { value = Offset_into_debug_abbrev addr_index; comment }

let append_to_comment { value; comment } to_append =
  let comment =
    match comment with
    | None -> Some to_append
    | Some comment -> Some (comment ^ " " ^ to_append)
  in
  { value; comment }

(* DWARF-4 standard section 7.6. *)
let uleb128_size i =
  let rec uleb128_size i =
    if Int64.compare i 128L < 0
    then Dwarf_int.one ()
    else Dwarf_int.succ (uleb128_size (Int64.shift_right_logical i 7))
  in
  let i = Uint64.to_int64 i in
  if Int64.compare i 0L < 0
  then
    Misc.fatal_errorf
      "Cannot compute ULEB128 encodings on unsigned numbers that don't fit in \
       [Int64].t";
  uleb128_size i

let rec sleb128_size i =
  if Int64.compare i (-64L) >= 0 && Int64.compare i 64L < 0
  then Dwarf_int.one ()
  else Dwarf_int.succ (sleb128_size (Int64.shift_right i 7))

let size { value; comment = _ } =
  match value with
  | Flag_true -> Dwarf_int.zero () (* see comment below *)
  | Bool _ -> Dwarf_int.one ()
  | Int8 _ | Uint8 _ -> Dwarf_int.one ()
  | Int16 _ | Uint16 _ -> Dwarf_int.two ()
  | Int32 _ | Uint32 _ -> Dwarf_int.four ()
  | Int64 _ | Uint64 _ -> Dwarf_int.eight ()
  | Uleb128 i -> uleb128_size i
  | Sleb128 i -> sleb128_size i
  | Absolute_address _ | Code_address_from_label _
  | Code_address_from_label_plus_offset _ | Code_address_from_symbol _
  | Address_table_entry_from_label_symbol_diff _
  | Code_address_from_symbol_diff _ | Code_address_from_symbol_plus_bytes _ -> (
    match Targetint.size with
    | 32 -> Dwarf_int.four ()
    | 64 -> Dwarf_int.eight ()
    | bits -> Misc.fatal_errorf "Unsupported Targetint.size %d" bits)
  | String str ->
    Dwarf_int.of_targetint_exn (Targetint.of_int (String.length str + 1))
  | Indirect_string _ | Offset_into_debug_line _
  | Offset_into_debug_line_from_symbol _ | Offset_into_debug_info _
  | Offset_into_debug_info_from_symbol _ | Offset_into_debug_addr _
  | Offset_into_debug_loc _ | Offset_into_debug_ranges _
  | Offset_into_debug_loclists _ | Offset_into_debug_rnglists _
  | Offset_into_debug_abbrev _ ->
    Dwarf_int.size (Dwarf_int.zero ())
  | Distance_between_labels_16_bit _ -> Dwarf_int.two ()
  | Distance_between_labels_32_bit _ -> Dwarf_int.four ()
  | Distance_between_labels_64_bit _
  | Distance_between_labels_64_bit_with_offsets _ ->
    Dwarf_int.eight ()

let emit ~asm_directives:_ { value; comment } =
  let width_for_ref_addr_or_sec_offset = !Dwarf_flags.gdwarf_format in
  match value with
  | Flag_true -> (
    (* See DWARF-4 specification p.148 *)
    match comment with
    | None -> ()
    | Some comment -> A.comment (comment ^ " (Flag_true elided)"))
  | Bool b -> A.int8 ?comment (if b then Int8.one else Int8.zero)
  | Int8 i -> A.int8 ?comment i
  | Int16 i -> A.int16 ?comment i
  | Int32 i -> A.int32 ?comment i
  | Int64 i -> A.int64 ?comment i
  | Uint8 i -> A.uint8 ?comment i
  | Uint16 i -> A.uint16 ?comment i
  | Uint32 i -> A.uint32 ?comment i
  | Uint64 i -> A.uint64 ?comment i
  | Uleb128 i -> A.uleb128 ?comment i
  | Sleb128 i -> A.sleb128 ?comment i
  | String str -> A.string ?comment str
  | Indirect_string str ->
    (* "Indirect" strings are collected together into ".debug_str". *)
    let label = A.cache_string ?comment (DWARF Debug_str) str in
    A.offset_into_dwarf_section_label ?comment Debug_str label
      ~width:width_for_ref_addr_or_sec_offset;
    if !Clflags.keep_asm_file
    then
      let str_len = String.length str in
      let max_str_len = 30 in
      let abbrev =
        if str_len <= max_str_len
        then Printf.sprintf "  (.debug_str entry is %S)" str
        else
          let abbrev = String.sub str 0 max_str_len in
          Printf.sprintf "  (.debug_str entry starts %S [...])" abbrev
      in
      A.comment abbrev
  | Absolute_address addr -> A.targetint ?comment addr
  | Code_address_from_label lbl -> A.label ?comment lbl
  | Code_address_from_label_plus_offset { label; offset_in_bytes } ->
    A.label_plus_offset ?comment label ~offset_in_bytes
  | Code_address_from_symbol sym -> A.symbol ?comment sym
  | Address_table_entry_from_label_symbol_diff { upper; lower; offset_upper } ->
    A.between_symbol_in_current_unit_and_label_offset ?comment ~upper ~lower
      ~offset_upper ()
  | Code_address_from_symbol_diff { upper; lower } ->
    A.between_symbols_in_current_unit ~upper ~lower
  | Code_address_from_symbol_plus_bytes { sym; offset_in_bytes } ->
    A.symbol_plus_offset sym ~offset_in_bytes
  | Offset_into_debug_line label ->
    let normal_or_dwo = Asm_label.normal_or_dwo label in
    A.offset_into_dwarf_section_label ?comment (Debug_line normal_or_dwo) label
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_line_from_symbol (normal_or_dwo, symbol) ->
    A.offset_into_dwarf_section_symbol ?comment (Debug_line normal_or_dwo)
      symbol ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_info lbl ->
    let normal_or_dwo = Asm_label.normal_or_dwo lbl in
    A.offset_into_dwarf_section_label ?comment (Debug_info normal_or_dwo) lbl
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_info_from_symbol (normal_or_dwo, sym) ->
    A.offset_into_dwarf_section_symbol ?comment (Debug_info normal_or_dwo) sym
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_addr label ->
    A.offset_into_dwarf_section_label ?comment Debug_addr label
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_loc label ->
    A.offset_into_dwarf_section_label ?comment Debug_loc label
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_ranges label ->
    A.offset_into_dwarf_section_label ?comment Debug_ranges label
      ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_loclists label ->
    let normal_or_dwo = Asm_label.normal_or_dwo label in
    A.offset_into_dwarf_section_label ?comment (Debug_loclists normal_or_dwo)
      label ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_rnglists label ->
    let normal_or_dwo = Asm_label.normal_or_dwo label in
    A.offset_into_dwarf_section_label ?comment (Debug_rnglists normal_or_dwo)
      label ~width:width_for_ref_addr_or_sec_offset
  | Offset_into_debug_abbrev label ->
    let normal_or_dwo = Asm_label.normal_or_dwo label in
    A.offset_into_dwarf_section_label ?comment (Debug_abbrev normal_or_dwo)
      label ~width:width_for_ref_addr_or_sec_offset
  | Distance_between_labels_16_bit { upper; lower } ->
    (* We rely on the assembler for overflow checking here and in the 32-bit
       case below. *)
    A.between_labels_16_bit ?comment ~upper ~lower ()
  | Distance_between_labels_32_bit { upper; lower } ->
    A.between_labels_32_bit ?comment ~upper ~lower ()
  | Distance_between_labels_64_bit { upper; lower } ->
    A.between_labels_64_bit ?comment ~upper ~lower ()
  | Distance_between_labels_64_bit_with_offsets
      { upper; upper_offset; lower; lower_offset } ->
    A.between_labels_64_bit_with_offsets ?comment ~upper ~upper_offset ~lower
      ~lower_offset ()
