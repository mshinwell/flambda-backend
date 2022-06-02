(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cmm_helpers

type t =
  { gc_roots : Symbol.t list;
    data_list : Cmm.phrase list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    module_symbol : Symbol.t;
    module_symbol_defined : bool;
    offsets : Exported_offsets.t;
    next_symbol_offset : int;
    data_symbol : Symbol.t
  }

let create ~module_symbol ~data_symbol offsets =
  { gc_roots = [];
    data_list = [];
    functions = [];
    current_data = [];
    module_symbol;
    module_symbol_defined = false;
    offsets;
    next_symbol_offset = 0;
    data_symbol
  }

let record_symbol_offset t symbol ~size_in_words_excluding_header =
  let offsets =
    Exported_offsets.add_symbol_offset t.offsets symbol
      ~bytes:t.next_symbol_offset
  in
  let next_symbol_offset =
    t.next_symbol_offset + (8 * (size_in_words_excluding_header + 1))
  in
  { t with offsets; next_symbol_offset }

let check_for_module_symbol t symbol =
  if Symbol.equal symbol t.module_symbol
  then begin
    if t.module_symbol_defined
    then
      Misc.fatal_errorf
        "check_for_module_symbol %a: Module block symbol (%a) already defined"
        Symbol.print symbol Symbol.print t.module_symbol;
    { t with module_symbol_defined = true }, true
  end
  else t, false

let defines_a_symbol data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol _ -> true
  | Cglobal_symbol _ | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _
  | Cdouble _ | Csymbol_address _ | Cstring _ | Cskip _ | Calign _ ->
    false

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a"
        Printcmm.data x;
    C.cdata x :: l

let archive_data r =
  { r with
    current_data = [];
    data_list = add_to_data_list r.current_data r.data_list
  }

let update_data r f = { r with current_data = f r.current_data }

let set_data r l =
  update_data r (function
    | [] -> l
    | _ ->
      Misc.fatal_errorf "To_cmm_result.set_data: %s"
        "about to lose some translated static data items")

let add_archive_data_items r l =
  { r with data_list = add_to_data_list l r.data_list }

let add_gc_roots r l = { r with gc_roots = l @ r.gc_roots }

let add_function r f = { r with functions = f :: r.functions }

let static_symbol_address t symbol =
  let name = Symbol.linkage_name_as_string symbol in
  let t =
    match Exported_offsets.symbol_offset_in_bytes t.offsets symbol with
    | Some bytes ->
      Format.eprintf "LOCAL Symbol %a can be reached via offset %d\n"
        Symbol.print symbol bytes;
      t
    | None -> (
      match
        Exported_offsets.symbol_offset_in_bytes
          (Exported_offsets.imported_offsets ())
          symbol
      with
      | Some bytes ->
        Format.eprintf "IMPORTED Symbol %a can be reached via offset %d\n"
          Symbol.print symbol bytes;
        t
      | None -> t)
  in
  Cmm.Csymbol_address name, t

type result =
  { data_items : Cmm.phrase list;
    gc_roots : Symbol.t list;
    functions : Cmm.phrase list;
    offsets : Exported_offsets.t
  }

let define_module_symbol_if_missing r =
  if r.module_symbol_defined
  then r
  else
    let linkage_name =
      Linkage_name.to_string (Symbol.linkage_name r.module_symbol)
    in
    let l = C.emit_block (linkage_name, Global) (C.black_block_header 0 0) [] in
    set_data r l

let to_cmm r =
  (* Make sure the module symbol is defined *)
  let r = define_module_symbol_if_missing r in
  (* Make sure we do not forget any current data *)
  let r = archive_data r in
  (* Sort functions according to debuginfo, to get a stable ordering *)
  let sorted_functions =
    List.sort
      (fun (f1 : Cmm.fundecl) (f2 : Cmm.fundecl) ->
        Debuginfo.compare f1.fun_dbg f2.fun_dbg)
      r.functions
  in
  let function_phrases = List.map (fun f -> C.cfunction f) sorted_functions in
  let data_items = r.data_list in
  (* let symbol_offsets_in_bytes, _offset_in_bytes = List.fold_left (fun
     (symbol_offsets_in_bytes, offset_in_bytes) (phrase : Cmm.phrase) -> match
     phrase with | Cfunction _ -> symbol_offsets_in_bytes, offset_in_bytes |
     Cdata items -> List.fold_left (fun (symbol_offsets_in_bytes,
     offset_in_bytes) (item : Cmm.data_item) -> match item with | Cdefine_symbol
     symbol -> let symbol_offsets_in_bytes = | Cglobal_symbol _ ->
     symbol_offsets_in_bytes_in_bytes | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ |
     Csingle _ | Cdouble _ | Csymbol_address _ | Cstring _ | Cskip _ -> | Calign
     _ -> Misc.fatal_error "Calign not supported")
     (symbol_offsets_in_bytes_in_bytes, 0) items ) Symbol.Map.empty
     data_items *)
  { data_items;
    gc_roots = r.gc_roots;
    functions = function_phrases;
    offsets = r.offsets
  }
