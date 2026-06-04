(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street                       *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

module C = Cmm_helpers
module R = To_cmm_result

let linkage_name code_id =
  C.code_block_symbol_name
    (Linkage_name.to_string (Code_id.linkage_name code_id))

let code_block_symbol_for code_id : Cmm.symbol =
  (* Code_block symbols always belong to the current CU (we only emit them for
     code defined here). They are exported as [Global] so the JIT loader can
     locate them by name when populating the unit's [code_blocks] table. *)
  { sym_name = linkage_name code_id; sym_global = Global }

let entry_symbol_for code_id : Cmm.symbol =
  (* The function's own entry symbol (the address whose [entry - 1] back-pointer
     refers to its code block). Same-CU unloadable function entries are
     exported [Global] (they are also listed in the unit's code-block
     sentinel). *)
  { sym_name = Linkage_name.to_string (Code_id.linkage_name code_id);
    sym_global = Global
  }

let dep_is_unloadable all_code dep_code_id =
  match Exported_code.find all_code dep_code_id with
  | None -> false
  | Some com -> Code_metadata.is_unloadable (Code_or_metadata.code_metadata com)

let emit_code_block_for ~all_code (code : Code.t) res =
  if (not !Clflags.unit_is_unloadable) || not (Code.is_unloadable code)
  then res
  else
    let code_id = Code.code_id code in
    let entry_linkage_name =
      Linkage_name.to_string (Code_id.linkage_name code_id)
    in
    C.register_unloadable_code_block_entry entry_linkage_name;
    let free_names = Code.free_names_of_params_and_body code in
    (* Code dependencies become packed code-pointer slots in the code block;
       each is recorded by its function-entry symbol (filtered to unloadable
       same-CU callees, which are reached directly rather than via closures —
       cross-CU edges go through closures carrying the closinfo flag). *)
    let code_dep_entries =
      Name_occurrences.code_ids free_names
      |> Code_id.Set.filter (dep_is_unloadable all_code)
      |> Code_id.Set.elements
      |> List.map entry_symbol_for
    in
    (* Data dependencies become the code block's closure environment. We filter
       to symbols defined in the current (unloadable) CU. Cross-CU symbols
       (e.g. [caml_int_ops], stdlib lifted constants, predefined exceptions)
       have NOT_MARKABLE headers, so [caml_darken] is a no-op on them and
       including them would only bloat the environment and the mark scan.
       Same-CU [Local] symbols are also no-op darkens (black headers) but the
       same-CU filter keeps them in: any same-CU data block referenced from a
       function's code path may be marked via the environment scan, and B.1
       emits same-CU unloadable data blocks with white headers. *)
    let data_deps =
      Name_occurrences.symbols free_names
      |> Symbol.Set.filter (fun sym ->
             Compilation_unit.is_current (Symbol.compilation_unit sym))
      |> Symbol.Set.elements
      |> List.map (fun sym : Cmm.symbol ->
             { sym_name = Linkage_name.to_string (Symbol.linkage_name sym);
               sym_global = Local
             })
    in
    let block_sym = code_block_symbol_for code_id in
    let own_entry = entry_symbol_for code_id in
    (* Suppress unloadable_data_block tracking for code blocks: they are tracked
       separately via the runtime's [code_blocks] list (located by the JIT
       loader using the [_code_block] symbol-name suffix). *)
    let prev = !C.suppress_unloadable_data_block_tracking in
    C.suppress_unloadable_data_block_tracking := true;
    let data_items =
      C.emit_code_block ~block_sym ~own_entry ~code_dep_entries ~data_deps []
    in
    C.suppress_unloadable_data_block_tracking := prev;
    R.add_archive_data_items res data_items

(* The entry function's [Code_block] has zero dependency fields, even though the
   entry calls top-level functions in the unit and references the unit's static
   data. The entry is only on-stack during initialisation (so F.2 keeps its
   [Code_block] alive while running), and once [Eval.eval] returns nothing
   reaches the entry's Code_block.

   If a major GC fires *during* eval'd initialisation and walks the running
   entry, the entry's Code_block is darkened but the rest of the unit is not
   marked through its dep fields — only through whatever the stack and closures
   already point at. This is fine because every same-CU function the entry
   transitively calls is itself reachable via stack frames or live closures at
   that point. *)
let emit_entry_code_block ~(entry_sym : Cmm.symbol) res =
  if not !Clflags.unit_is_unloadable
  then res
  else (
    C.register_unloadable_code_block_entry entry_sym.sym_name;
    let block_sym : Cmm.symbol =
      { sym_name = C.code_block_symbol_name entry_sym.sym_name;
        sym_global = Global
      }
    in
    (* The entry's code block carries no dependencies (see the comment above):
       [emit_code_block] therefore emits a single dummy function slot holding
       [entry_sym] and no environment. Suppress data-block tracking: code
       blocks are tracked separately via the unit's [unloadable_code_blocks]
       sentinel array (see [to_cmm.ml] and the JIT loader). *)
    let prev = !C.suppress_unloadable_data_block_tracking in
    C.suppress_unloadable_data_block_tracking := true;
    let data_items =
      C.emit_code_block ~block_sym ~own_entry:entry_sym ~code_dep_entries:[]
        ~data_deps:[] []
    in
    C.suppress_unloadable_data_block_tracking := prev;
    R.add_archive_data_items res data_items)
