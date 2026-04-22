(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Invariants (when the value is not being mutated in-flight): - The value
   represented by [t] is [t.descr] with [t.delayed_renaming] applied. - If
   [t.free_names = Some fn] then [fn] are the free names of the value
   represented by [t] (i.e. of [t.descr] with [t.delayed_renaming] applied). *)
type 'descr t =
  { mutable descr : 'descr;
    mutable free_names : Name_occurrences.t option;
    mutable delayed_renaming : Renaming.t
  }

let create descr =
  { descr; free_names = None; delayed_renaming = Renaming.empty }

let[@inline always] force_renaming ~apply_renaming_descr t =
  if not (Renaming.is_identity t.delayed_renaming)
  then (
    let descr = apply_renaming_descr t.descr t.delayed_renaming in
    t.descr <- descr;
    t.delayed_renaming <- Renaming.empty)

let[@inline always] descr ~apply_renaming_descr t =
  force_renaming ~apply_renaming_descr t;
  t.descr

let[@inline always] peek_descr t = t.descr

let[@inline always] peek_delayed_renaming t = t.delayed_renaming

let[@inline always] free_names ~free_names_descr t =
  match t.free_names with
  | Some free_names -> free_names
  | None ->
    let free_names = free_names_descr t.descr in
    let free_names =
      if Renaming.is_identity t.delayed_renaming
      then free_names
      else Name_occurrences.apply_renaming free_names t.delayed_renaming
    in
    t.free_names <- Some free_names;
    free_names

let[@inline always] free_names_no_cache ~free_names_descr t =
  let free_names = free_names_descr t.descr in
  if Renaming.is_identity t.delayed_renaming
  then free_names
  else Name_occurrences.apply_renaming free_names t.delayed_renaming

let apply_renaming ~apply_renaming_descr ~free_names_descr:_ t renaming =
  if Renaming.is_identity renaming
  then t
  else if Renaming.has_import_map renaming
  then (
    (* Renamings carrying an import map cannot be composed with an existing
       delayed renaming (see [Renaming.compose]); force and apply eagerly. *)
    force_renaming ~apply_renaming_descr t;
    let descr = apply_renaming_descr t.descr renaming in
    let free_names =
      match t.free_names with
      | None -> None
      | Some fn -> Some (Name_occurrences.apply_renaming fn renaming)
    in
    { descr; free_names; delayed_renaming = Renaming.empty })
  else
    let delayed_renaming =
      Renaming.compose ~second:renaming ~first:t.delayed_renaming
    in
    let free_names =
      match t.free_names with
      | None -> None
      | Some fn -> Some (Name_occurrences.apply_renaming fn renaming)
    in
    { descr = t.descr; free_names; delayed_renaming }

let remove_unused_value_slots_and_shortcut_aliases ~apply_renaming_descr
    ~remove_unused_value_slots_and_shortcut_aliases_descr t ~used_value_slots
    ~canonicalise =
  force_renaming ~apply_renaming_descr t;
  let descr =
    remove_unused_value_slots_and_shortcut_aliases_descr t.descr
      ~used_value_slots ~canonicalise
  in
  if descr == t.descr
  then t
  else { descr; free_names = None; delayed_renaming = Renaming.empty }

let project_variables_out ~apply_renaming_descr ~free_names_descr ~to_project
    ~project_descr t =
  let free_names = free_names ~free_names_descr t in
  let has_variable_to_project =
    Variable.Set.fold
      (fun var has_variable_to_project ->
        has_variable_to_project || Name_occurrences.mem_var free_names var)
      to_project false
  in
  if has_variable_to_project
  then (
    force_renaming ~apply_renaming_descr t;
    let descr' = project_descr t.descr in
    if descr' == t.descr then t else create descr')
  else t

let print ~apply_renaming_descr ~print_descr ppf t =
  print_descr ppf (descr ~apply_renaming_descr t)
