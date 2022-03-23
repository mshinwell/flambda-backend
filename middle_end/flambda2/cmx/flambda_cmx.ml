(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module EC = Exported_code
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

type loader =
  { get_global_info : Compilation_unit.t -> Flambda_cmx_format.t option;
    mutable imported_names : Name.Set.t;
    mutable imported_code : Exported_code.t;
    mutable imported_units : TE.t option Compilation_unit.Map.t
  }

let rec load_cmx_file_contents loader comp_unit =
  match Compilation_unit.Map.find comp_unit loader.imported_units with
  | typing_env_or_none -> typing_env_or_none
  | exception Not_found -> (
    match loader.get_global_info comp_unit with
    | None ->
      (* To make things easier to think about, we never retry after a .cmx load
         fails. *)
      loader.imported_units
        <- Compilation_unit.Map.add comp_unit None loader.imported_units;
      None
    | Some cmx ->
      let resolver comp_unit = load_cmx_file_contents loader comp_unit in
      let get_imported_names () = loader.imported_names in
      let typing_env, all_code =
        Flambda_cmx_format.import_typing_env_and_code cmx
      in
      let typing_env =
        TE.Serializable.to_typing_env ~resolver ~get_imported_names typing_env
      in
      let newly_imported_names = TE.name_domain typing_env in
      loader.imported_names
        <- Name.Set.union newly_imported_names loader.imported_names;
      loader.imported_code <- EC.merge all_code loader.imported_code;
      let offsets = Flambda_cmx_format.exported_offsets cmx in
      Exported_offsets.import_offsets offsets;
      loader.imported_units
        <- Compilation_unit.Map.add comp_unit (Some typing_env)
             loader.imported_units;
      Some typing_env)

let load_symbol_approx loader symbol : Code_or_metadata.t Value_approximation.t
    =
  let comp_unit = Symbol.compilation_unit symbol in
  match load_cmx_file_contents loader comp_unit with
  | None -> Value_unknown
  | Some typing_env ->
    let find_code code_id =
      match Exported_code.find loader.imported_code code_id with
      | Some code_or_meta -> code_or_meta
      | _ ->
        Misc.fatal_errorf
          "Failed to load informations for %a. Code id not found." Code_id.print
          code_id
    in
    T.extract_symbol_approx typing_env symbol find_code

let all_predefined_exception_symbols ~symbol_for_global =
  Predef.all_predef_exns
  |> List.map (fun ident ->
         symbol_for_global
           ?comp_unit:(Some (Compilation_unit.predefined_exception ()))
           ident)
  |> Symbol.Set.of_list

let predefined_exception_typing_env ~symbol_for_global loader =
  let comp_unit = Compilation_unit.get_current_exn () in
  Compilation_unit.set_current (Compilation_unit.predefined_exception ());
  let resolver comp_unit = load_cmx_file_contents loader comp_unit in
  let get_imported_names () = loader.imported_names in
  let typing_env =
    Symbol.Set.fold
      (fun sym typing_env ->
        TE.add_definition typing_env
          (Bound_name.create_symbol sym)
          Flambda_kind.value)
      (all_predefined_exception_symbols ~symbol_for_global)
      (TE.create ~resolver ~get_imported_names)
  in
  Compilation_unit.set_current comp_unit;
  typing_env

let create_loader ~get_global_info ~symbol_for_global =
  let loader =
    { get_global_info;
      imported_names = Name.Set.empty;
      imported_code = Exported_code.empty;
      imported_units = Compilation_unit.Map.empty
    }
  in
  let predefined_exception_typing_env =
    predefined_exception_typing_env ~symbol_for_global loader
  in
  loader.imported_units
    <- Compilation_unit.Map.singleton
         (Compilation_unit.predefined_exception ())
         (Some predefined_exception_typing_env);
  loader.imported_names <- TE.name_domain predefined_exception_typing_env;
  loader

let get_imported_names loader () = loader.imported_names

let get_imported_code loader () = loader.imported_code

let compute_reachable_names_and_code ~module_symbol ~free_names_of_name code =
  let rec fixpoint names_to_add names_already_added =
    if Name_occurrences.is_empty names_to_add
    then names_already_added
    else
      let names_already_added =
        Name_occurrences.union names_to_add names_already_added
      in
      let fold_code_id names_to_add code_id =
        if not
             (Code_id.in_compilation_unit code_id
                (Compilation_unit.get_current_exn ()))
        then
          (* Code in units upon which the current unit depends cannot reference
             this unit. *)
          names_to_add
        else
          match Exported_code.find code code_id with
          | None -> names_to_add
          | Some code_or_metadata ->
            let free_names = Code_or_metadata.free_names code_or_metadata in
            let names_to_consider =
              Name_occurrences
              .with_only_names_and_code_ids_promoting_newer_version_of
                free_names
            in
            let new_names =
              Name_occurrences.diff names_to_consider names_already_added
            in
            Name_occurrences.union new_names names_to_add
      in
      let fold_name names_to_add name =
        if not
             (Compilation_unit.equal
                (Name.compilation_unit name)
                (Compilation_unit.get_current_exn ()))
        then
          (* Names in units upon which the current unit depends cannot reference
             this unit. *)
          names_to_add
        else
          match free_names_of_name name with
          | Some ty_names ->
            let names_to_consider =
              Name_occurrences
              .with_only_names_and_code_ids_promoting_newer_version_of ty_names
            in
            let new_names =
              Name_occurrences.diff names_to_consider names_already_added
            in
            Name_occurrences.union new_names names_to_add
          | None ->
            (* A missing type cannot refer to names defined in the current
               unit *)
            names_to_add
      in
      let from_code_ids =
        Name_occurrences.fold_code_ids names_to_add ~init:Name_occurrences.empty
          ~f:fold_code_id
      in
      let from_names_and_code_ids =
        Name_occurrences.fold_names names_to_add ~init:from_code_ids
          ~f:fold_name
      in
      fixpoint from_names_and_code_ids names_already_added
  in
  let init_names =
    Name_occurrences.singleton_symbol module_symbol Name_mode.normal
  in
  fixpoint init_names Name_occurrences.empty

let prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
    ~used_closure_vars ~canonicalise ~exported_offsets all_code =
  let reachable_names =
    compute_reachable_names_and_code ~module_symbol ~free_names_of_name all_code
  in
  let all_code =
    (* CR mshinwell: do we need to remove unused closure ID bindings from the
       result types too? *)
    all_code
    |> EC.remove_unused_closure_vars_from_result_types_and_shortcut_aliases
         ~used_closure_vars ~canonicalise
    |> EC.remove_unreachable ~reachable_names
  in
  let final_typing_env = create_typing_env reachable_names in
  (* We need to re-export offsets for everything reachable from the cmx file;
     closure_vars/ids can be reachable from the typing env, and the exported
     code.

     In the case of the exported code, we already have offsets for all
     closure_ids/vars reachable from the code of the current compilation unit,
     but since we also re-export code from other compilation units, we need to
     take those into account. *)
  (* CR gbury: it might be more efficient to not compute the free names for all
     exported code, but fold over the exported code to avoid allocating some
     free_names *)
  let free_names_of_all_code = EC.free_names all_code in
  let closure_elts_used_in_typing_env =
    TE.Serializable.free_closure_ids_and_closure_vars final_typing_env
  in
  let exported_offsets =
    exported_offsets
    |> Closure_offsets.reexport_closure_ids
         (Name_occurrences.all_closure_ids free_names_of_all_code)
    |> Closure_offsets.reexport_closure_vars
         (Name_occurrences.all_closure_vars free_names_of_all_code)
    |> Closure_offsets.reexport_closure_ids
         (Name_occurrences.all_closure_ids closure_elts_used_in_typing_env)
    |> Closure_offsets.reexport_closure_vars
         (Name_occurrences.all_closure_vars closure_elts_used_in_typing_env)
  in
  Some
    (Flambda_cmx_format.create ~final_typing_env ~all_code ~exported_offsets
       ~used_closure_vars)

let prepare_cmx_file_contents ~final_typing_env ~module_symbol
    ~used_closure_vars ~exported_offsets all_code =
  match final_typing_env with
  | None -> None
  | Some _ when Flambda_features.opaque () -> None
  | Some final_typing_env ->
    let typing_env, canonicalise =
      TE.Pre_serializable.create final_typing_env ~used_closure_vars
    in
    let create_typing_env reachable_names =
      TE.Serializable.create typing_env ~reachable_names
    in
    let free_names_of_name name =
      Option.map T.free_names
        (TE.Pre_serializable.find_or_missing typing_env name)
    in
    prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
      ~used_closure_vars ~canonicalise ~exported_offsets all_code

let prepare_cmx_from_approx ~approxs ~module_symbol ~exported_offsets
    ~used_closure_vars all_code =
  if Flambda_features.opaque ()
  then None
  else
    let create_typing_env reachable_names =
      let approxs =
        Symbol.Map.filter
          (fun sym _ -> Name_occurrences.mem_symbol reachable_names sym)
          approxs
      in
      TE.Serializable.create_from_closure_conversion_approx approxs
    in
    let free_names_of_name name =
      let symbol = Name.must_be_symbol name in
      match Symbol.Map.find_opt symbol approxs with
      | None -> None
      | Some approx ->
        Some
          (Value_approximation.free_names
             ~code_free_names:Code_or_metadata.free_names approx)
    in
    prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
      ~used_closure_vars
      ~canonicalise:(fun id -> id)
      ~exported_offsets all_code
