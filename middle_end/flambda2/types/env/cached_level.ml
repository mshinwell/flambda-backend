(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module IT = Binding_time.Non_overlapping_interval_tree_for_name_modes

type t =
  { names_to_types : (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t;
    aliases : Aliases.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let print_kind_and_mode ~name_mode_restrictions ppf (ty, binding_time, mode) =
  let kind = Type_grammar.kind ty in
  let mode =
    IT.scoped_name_mode name_mode_restrictions
      (Binding_time.With_name_mode.create binding_time mode)
  in
  Format.fprintf ppf ":: %a %a" Flambda_kind.print kind Name_mode.print mode

let print_name_modes ~restrict_to ~name_mode_restrictions ppf t =
  Name.Map.print
    (print_kind_and_mode ~name_mode_restrictions)
    ppf
    (Name.Map.filter
       (fun name _ -> Name.Set.mem name restrict_to)
       t.names_to_types)

(* CR mshinwell: add [invariant] function *)

let empty =
  { names_to_types = Name.Map.empty;
    aliases = Aliases.empty;
    symbol_projections = Variable.Map.empty
  }

let names_to_types t = t.names_to_types

let aliases t = t.aliases

let symbol_projections t = t.symbol_projections

(* CR mshinwell: At least before the following two functions were split (used to
   be add-or-replace), the [names_to_types] map addition was a major source of
   allocation. *)

let add_or_replace_binding t (name : Name.t) ty binding_time name_mode =
  let names_to_types =
    Name.Map.add name (ty, binding_time, name_mode) t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections
  }

let replace_variable_binding t var ty =
  let names_to_types =
    Name.Map.replace (Name.var var)
      (function
        | _old_ty, binding_time, name_mode -> ty, binding_time, name_mode)
      t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections
  }

let with_aliases t ~aliases = { t with aliases }

let add_symbol_projection t var proj =
  let symbol_projections = Variable.Map.add var proj t.symbol_projections in
  { t with symbol_projections }

let find_symbol_projection t var =
  match Variable.Map.find var t.symbol_projections with
  | exception Not_found -> None
  | proj -> Some proj

let clean_for_export t ~reachable_names =
  (* Names coming from other compilation units or unreachable are removed *)
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let names_to_types =
    Name.Map.filter
      (fun name _info ->
        Name_occurrences.mem_name reachable_names name
        && Compilation_unit.equal
             (Name.compilation_unit name)
             current_compilation_unit)
      t.names_to_types
  in
  let aliases = Aliases.clean_for_export t.aliases in
  { t with names_to_types; aliases }

let apply_renaming { names_to_types; aliases; symbol_projections } renaming =
  let names_to_types =
    Name.Map.fold
      (fun name (ty, binding_time, mode) acc ->
        Name.Map.add
          (Renaming.apply_name renaming name)
          (Type_grammar.apply_renaming ty renaming, binding_time, mode)
          acc)
      names_to_types Name.Map.empty
  in
  let aliases = Aliases.apply_renaming aliases renaming in
  let symbol_projections =
    Variable.Map.fold
      (fun var proj acc ->
        Variable.Map.add
          (Renaming.apply_variable renaming var)
          (Symbol_projection.apply_renaming proj renaming)
          acc)
      symbol_projections Variable.Map.empty
  in
  { names_to_types; aliases; symbol_projections }

let merge t1 t2 =
  let names_to_types =
    Name.Map.disjoint_union t1.names_to_types t2.names_to_types
  in
  let aliases = Aliases.merge t1.aliases t2.aliases in
  let symbol_projections =
    Variable.Map.union
      (fun var proj1 proj2 ->
        if Symbol_projection.equal proj1 proj2
        then Some proj1
        else
          Misc.fatal_errorf
            "Cannot merge symbol projections for %a:@ %a@ and@ %a"
            Variable.print var Symbol_projection.print proj1
            Symbol_projection.print proj2)
      t1.symbol_projections t2.symbol_projections
  in
  { names_to_types; aliases; symbol_projections }
