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
    offset_data_list : Cmm.data_item list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    module_symbol : Symbol.t;
    module_symbol_defined : bool;
    offsets : Exported_offsets.t;
    next_symbol_offset : Targetint.t;
    data_symbol : Symbol.t
  }

let create ~module_symbol ~data_symbol offsets =
  { gc_roots = [];
    data_list = [];
    offset_data_list = [];
    functions = [];
    current_data = [];
    module_symbol;
    module_symbol_defined = false;
    offsets;
    next_symbol_offset = Targetint.of_int 0;
    data_symbol
  }

let record_symbol_offset t symbol ~size_in_words_excluding_header =
  let offsets =
    Exported_offsets.add_symbol_offset t.offsets symbol
      ~bytes:t.next_symbol_offset
  in
  (* Format.eprintf "OFFSET for %a = %a bytes\n%!" Symbol.print symbol
     Targetint.print t.next_symbol_offset; *)
  let next_symbol_offset =
    Targetint.add t.next_symbol_offset
      (Targetint.of_int (8 * (size_in_words_excluding_header + 1)))
  in
  { t with offsets; next_symbol_offset }

let increment_symbol_offset t ~size_in_words_excluding_header =
  let next_symbol_offset =
    Targetint.add t.next_symbol_offset
      (Targetint.of_int (8 * size_in_words_excluding_header))
  in
  { t with next_symbol_offset }

let is_module_symbol t symbol = Symbol.equal t.module_symbol symbol

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
  | Cdouble _ | Csymbol_address _ | Coffset_symbol_address _ | Cstring _
  | Cskip _ | Calign _ ->
    false

let defines_a_symbol_or_global data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol _ | Cglobal_symbol _ -> true
  | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _ | Cdouble _
  | Csymbol_address _ | Coffset_symbol_address _ | Cstring _ | Cskip _
  | Calign _ ->
    false

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a\n\n\
         Backtrace:\n\
         %s\n"
        Printcmm.data x
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100));
    C.cdata x :: l

let archive_data r =
  { r with
    current_data = [];
    data_list = add_to_data_list r.current_data r.data_list
  }

let archive_offset_data r =
  (* CR mshinwell: Add [Ccomment] to [Cmm.data_item] for debugging. *)
  { r with
    current_data = [];
    offset_data_list =
      r.offset_data_list
      @ List.filter
          (fun data_item -> not (defines_a_symbol_or_global data_item))
          r.current_data
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

let is_module_symbol' symbol =
  let comp_unit = Compilation_unit.name (Symbol.compilation_unit symbol) in
  let linkage_name = Symbol.linkage_name_as_string symbol in
  String.equal ("caml" ^ comp_unit) linkage_name

let symbol_offset_in_bytes t symbol =
  match Exported_offsets.symbol_offset_in_bytes t.offsets symbol with
  | Some bytes ->
    (* Format.eprintf "LOCAL Symbol %a can be reached via offset %a\n"
       Symbol.print symbol Targetint.print bytes; *)
    Some bytes
  | None -> (
    match
      Exported_offsets.symbol_offset_in_bytes
        (Exported_offsets.imported_offsets ())
        symbol
    with
    | Some bytes ->
      (* Format.eprintf "IMPORTED Symbol %a can be reached via offset %a\n"
         Symbol.print symbol Targetint.print bytes; *)
      Some bytes
    | None ->
      if (not (Symbol.is_predefined_exception symbol))
         && not (is_module_symbol' symbol)
      then
        Misc.fatal_errorf "Cannot find offset for symbol %a" Symbol.print symbol;
      None)

let data_symbol_for_unit comp_unit =
  Symbol.create comp_unit (Linkage_name.create "data_symbol")

let static_symbol_address t symbol : Cmm.data_item =
  match symbol_offset_in_bytes t symbol with
  | None -> C.symbol_address (Symbol.linkage_name_as_string symbol)
  | Some bytes ->
    let data_symbol = data_symbol_for_unit (Symbol.compilation_unit symbol) in
    if Targetint.equal bytes Targetint.zero
    then C.symbol_address (Symbol.linkage_name_as_string data_symbol)
    else
      C.offset_symbol_address (Symbol.linkage_name_as_string data_symbol) ~bytes

let expr_symbol_address t symbol dbg : Cmm.expression =
  match symbol_offset_in_bytes t symbol with
  | None -> C.symbol_from_string ~dbg (Symbol.linkage_name_as_string symbol)
  | Some bytes ->
    let sym_expr =
      C.symbol_from_string ~dbg
        (Symbol.linkage_name_as_string
           (data_symbol_for_unit (Symbol.compilation_unit symbol)))
    in
    if Targetint.equal bytes Targetint.zero
    then sym_expr
    else
      let bytes = Int64.to_nativeint (Targetint.to_int64 bytes) in
      (* Beware: this must be all in machine integers, not tagged integers. *)
      Cop (Caddi, [sym_expr; Cconst_natint (bytes, dbg)], dbg)

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
  let data_items =
    match r.offset_data_list with
    | [] -> r.data_list
    | header :: rest -> (
      (* Move the first block header prior to the data symbol definition. *)
      match header with
      | Cint _ ->
        C.cdata
          (header
           :: C.define_symbol ~global:true
                (Symbol.linkage_name_as_string r.data_symbol)
          @ rest)
        :: r.data_list
      | Cdefine_symbol _ | Cglobal_symbol _ | Cint8 _ | Cint16 _ | Cint32 _
      | Csingle _ | Cdouble _ | Csymbol_address _ | Coffset_symbol_address _
      | Cstring _ | Cskip _ | Calign _ ->
        Misc.fatal_errorf
          "Malformed [offset_data_list], expected block header to begin:@ %a"
          Printcmm.data r.offset_data_list)
  in
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
