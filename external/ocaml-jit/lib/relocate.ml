(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

let out_of_text_error ~got_or_plt ~section_name =
  errorf
    "Relocation through %s in section %S. Such relocations should not be found \
     outside .text section"
    got_or_plt section_name

(* Generic relocate using Binary_emitter.S interface *)
module Generic = struct
  type table_lookup = string -> Address.t option

  let one (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      ~symbols ~got_lookup ~plt_lookup ~section_name (binary_section : a addressed) (reloc : r) =
    let open Result.Op in
    let _sym = E.Relocation.target_symbol reloc in
    let is_got = E.Relocation.is_got_reloc reloc in
    let is_plt = E.Relocation.is_plt_reloc reloc in
    (* Build lookup function that routes to GOT/PLT/symbols as appropriate *)
    let lookup_symbol name =
      if is_got then
        match got_lookup with
        | None -> None
        | Some lookup ->
          Option.map Address.to_int64 (lookup name)
      else if is_plt then
        match plt_lookup with
        | None -> None
        | Some lookup ->
          Option.map Address.to_int64 (lookup name)
      else
        Option.map Address.to_int64 (Symbols.find symbols name)
    in
    (* Check for invalid GOT/PLT outside .text *)
    let* () =
      if is_got && Option.is_none got_lookup then
        out_of_text_error ~got_or_plt:"GOT" ~section_name
      else if is_plt && Option.is_none plt_lookup then
        out_of_text_error ~got_or_plt:"PLT" ~section_name
      else Ok ()
    in
    let offset = E.Relocation.offset_from_section_beginning reloc in
    let place_address =
      Int64.add (Address.to_int64 binary_section.address) (Int64.of_int offset)
    in
    let* data = E.Relocation.compute_value reloc ~place_address ~lookup_symbol in
    let size = E.Relocation.size reloc in
    E.Assembled_section.add_patch binary_section.value ~offset ~size ~data;
    Ok ()

  let all (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      ~symbols ~got_lookup ~plt_lookup ~section_name (binary_section : a addressed) =
    let relocs = E.Assembled_section.relocations binary_section.value in
    Result.List.iter_all relocs
      ~f:(one (module E) ~symbols ~got_lookup ~plt_lookup ~section_name binary_section)
end

(* Legacy X86-specific implementation for backwards compatibility *)
let relocation_doesn't_fit_error ~value ~target_address ~section_name
      ~min_value ~max_value =
  errorf
    "Computed value 0x%Lx for relocation of target address 0x%a in section %s \
     doesn't fit; permissible range is (0x%Lx, 0x%Lx)"
    value Address.pp target_address section_name min_value max_value

let unauthorized_absolute_reloc ~got_or_plt ~section_name =
  errorf
    "Absolute %s relocation in section %s, such relocations should always be \
     relative"
    got_or_plt section_name

let lookup_symbol symbols name =
  match Symbols.find symbols name with
  | Some addr -> Ok addr
  | None -> errorf "Cannot proceed with relocation %s, symbol is unknown" name

let lookup_got got symbol =
  match Jit_got.symbol_address got symbol with
  | Some addr -> Ok addr
  | None ->
      errorf "Symbol %s should be in the GOT but it is missing from there"
        symbol

let lookup_plt plt symbol =
  match Jit_plt.symbol_address plt symbol with
  | Some addr -> Ok addr
  | None ->
      errorf "Symbol %s should be in the GOT but it is missing from there"
        symbol

let one ~symbols ~got ~plt ~section_name binary_section t =
  let open Result.Op in
  let* target_symbol_address =
    match ((t:Relocation.t), got, plt) with
    | { target = Direct name; _ }, _, _ -> lookup_symbol symbols name
    | { target = Got name; kind = Relative; _ }, Some got, _ ->
        lookup_got got name
    | { target = Got _; kind = Absolute; _ }, Some _, _ ->
        unauthorized_absolute_reloc ~got_or_plt:"GOT" ~section_name
    | { target = Got _; _ }, None, _ ->
        out_of_text_error ~got_or_plt:"GOT" ~section_name
    | { target = Plt name; kind = Relative; _ }, _, Some plt ->
        lookup_plt plt name
    | { target = Plt _; kind = Absolute; _ }, _, Some _ ->
        unauthorized_absolute_reloc ~got_or_plt:"PLT" ~section_name
    | { target = Plt _; _ }, _, None ->
        out_of_text_error ~got_or_plt:"PLT" ~section_name
  in
  (* TODO: The rest of this function belongs in [Relocation],
     we should have some thing like this here:
     [Relocation.patch t target_symbol_address target_symbol_address ~section_name
     ~binary_section]
     instead of exposing the details of min and max value and the offset
     calculations.
  *)
  let target_address = Address.add_int64 target_symbol_address t.addend in
  let data =
    match t.kind with
    | Absolute -> Address.to_int64 target_address
    | Relative ->
        let src_address =
          (* The offset is taken form the beginning of the next instruction, where
             the program counter  is at, that is the address of the relocation + its size .*)
          let offset = t.offset_from_section_beginning + Relocation.Size.to_int t.size in
          Address.add_int binary_section.address offset
        in
        Int64.sub
          (Address.to_int64 target_address)
          (Address.to_int64 src_address)
  in
  let min_value = Relocation.min_value t in
  let max_value = Relocation.max_value t in
  let comparator = Relocation.value_comparator t in
  if comparator data min_value < 0 || comparator data max_value > 0 then
    relocation_doesn't_fit_error ~value:data ~target_address ~section_name
      ~min_value ~max_value
  else (
    let size : X86_binary_emitter.data_size =
      match t.size with
      | Relocation.Size.S32 -> B32
      | S64 -> B64
    in
    X86_binary_emitter.add_patch ~offset:t.offset_from_section_beginning ~size
      ~data binary_section.value;
    Ok ()
  )

let all_gen ~symbols ~got ~plt ~section_name binary_section =
  let open Result.Op in
  let raw_relocations = X86_binary_emitter.relocations binary_section.value in
  let* relocations =
    Result.List.map_all ~f:Relocation.from_x86_relocation_err raw_relocations
  in
  Result.List.iter_all relocations
    ~f:(one ~symbols ~got ~plt ~section_name binary_section)

let all_text ~symbols ~got ~plt binary_section =
  let got = Some got in
  let plt = Some plt in
  let section_name = ".text" in
  all_gen ~symbols ~got ~plt ~section_name binary_section

let all ~symbols ~section_name binary_section =
  let got = None in
  let plt = None in
  all_gen ~symbols ~got ~plt ~section_name binary_section
