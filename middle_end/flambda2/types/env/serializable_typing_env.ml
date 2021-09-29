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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  { defined_symbols : Symbol.Set.t;
    code_age_relation : Code_age_relation.t;
    just_after_level : Cached_level.t;
    next_binding_time : Binding_time.t
  }

let defined_symbols t = t.defined_symbols

let code_age_relation t = t.code_age_relation

let just_after_level t = t.just_after_level

let next_binding_time t = t.next_binding_time

let create ~defined_symbols ~code_age_relation ~just_after_level
    ~next_binding_time =
  { defined_symbols; code_age_relation; just_after_level; next_binding_time }

let [@ocamlformat "disable"] print ppf
    { defined_symbols; code_age_relation; just_after_level;
      next_binding_time = _ } =
  Format.fprintf ppf
    "@[<hov 1>(\
        @[<hov 1>(defined_symbols@ %a)@]@ \
        @[<hov 1>(code_age_relation@ %a)@]@ \
        @[<hov 1>(type_equations@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
    Symbol.Set.print defined_symbols
    Code_age_relation.print code_age_relation
    (Name.Map.print (fun ppf (ty, _bt, _mode) -> Type_grammar.print ppf ty))
    (Cached_level.names_to_types just_after_level)
    Aliases.print (Cached_level.aliases just_after_level)

(* CR mshinwell for vlaviron: Shouldn't some of this be in
   [Cached_level.all_ids_for_export]? *)
let all_ids_for_export
    { defined_symbols;
      code_age_relation;
      just_after_level;
      next_binding_time = _
    } =
  let symbols = defined_symbols in
  let code_ids = Code_age_relation.all_code_ids_for_export code_age_relation in
  let ids = Ids_for_export.create ~symbols ~code_ids () in
  let ids =
    Name.Map.fold
      (fun name (typ, _binding_time, _name_mode) ids ->
        Ids_for_export.add_name
          (Ids_for_export.union ids (Type_grammar.all_ids_for_export typ))
          name)
      (Cached_level.names_to_types just_after_level)
      ids
  in
  let ids =
    Ids_for_export.union ids
      (Aliases.all_ids_for_export (Cached_level.aliases just_after_level))
  in
  let ids =
    Variable.Map.fold
      (fun var proj ids ->
        let ids =
          Ids_for_export.union ids (Symbol_projection.all_ids_for_export proj)
        in
        Ids_for_export.add_variable ids var)
      (Cached_level.symbol_projections just_after_level)
      ids
  in
  ids

let apply_renaming
    { defined_symbols; code_age_relation; just_after_level; next_binding_time }
    renaming =
  let defined_symbols =
    Symbol.Set.fold
      (fun sym symbols ->
        Symbol.Set.add (Renaming.apply_symbol renaming sym) symbols)
      defined_symbols Symbol.Set.empty
  in
  let code_age_relation =
    Code_age_relation.apply_renaming code_age_relation renaming
  in
  let just_after_level =
    Cached_level.apply_renaming just_after_level renaming
  in
  { defined_symbols; code_age_relation; just_after_level; next_binding_time }

let merge t1 t2 =
  let defined_symbols =
    Symbol.Set.union t1.defined_symbols t2.defined_symbols
  in
  let code_age_relation =
    Code_age_relation.union t1.code_age_relation t2.code_age_relation
  in
  let just_after_level =
    Cached_level.merge t1.just_after_level t2.just_after_level
  in
  let next_binding_time =
    (* Take the latest one *)
    if Binding_time.strictly_earlier t1.next_binding_time
         ~than:t2.next_binding_time
    then t2.next_binding_time
    else t1.next_binding_time
  in
  { defined_symbols; code_age_relation; just_after_level; next_binding_time }
