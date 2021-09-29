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

module FDT = Function_declaration_type
module PC = Product.Closure_id_indexed
module PV = Product.Var_within_closure_indexed
module TEE = Typing_env.Typing_env_extension

type t =
  { (* CR pchambart: This is exactly a Product with a different kind of fields
       Product should maybe be functorized once more to represent this. Maybe
       not because product also contains a kind which we don't want in that
       case.

       Also the closure_types and closure_var_types products shouldn't have to
       store the kind because it is always the same. *)
    function_decls : FDT.t Closure_id.Map.t;
    closure_types : PC.t;
    closure_var_types : PV.t
  }

let create ~function_decls ~closure_types ~closure_var_types =
  { function_decls; closure_types; closure_var_types }

let [@ocamlformat "disable"] print ppf
      { function_decls; closure_types; closure_var_types; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_types@ %a)@]@ \
      @[<hov 1>(closure_var_types@ %a)@]\
      )@]"
    (Closure_id.Map.print FDT.print) function_decls
    PC.print closure_types
    PV.print closure_var_types

let apply_renaming { function_decls; closure_types; closure_var_types } perm =
  { function_decls =
      Closure_id.Map.map_sharing
        (fun function_decl -> FDT.apply_renaming function_decl perm)
        function_decls;
    closure_types = PC.apply_renaming closure_types perm;
    closure_var_types = PV.apply_renaming closure_var_types perm
  }

let free_names { function_decls; closure_types; closure_var_types } =
  let function_decls_free_names =
    Closure_id.Map.fold
      (fun _closure_id function_decl free_names ->
        Name_occurrences.union free_names (FDT.free_names function_decl))
      function_decls Name_occurrences.empty
  in
  Name_occurrences.union function_decls_free_names
    (Name_occurrences.union
       (PC.free_names closure_types)
       (PV.free_names closure_var_types))

let all_ids_for_export { function_decls; closure_types; closure_var_types } =
  let function_decls_ids =
    Closure_id.Map.fold
      (fun _closure_id function_decl ids ->
        Ids_for_export.union ids (FDT.all_ids_for_export function_decl))
      function_decls Ids_for_export.empty
  in
  Ids_for_export.union function_decls_ids
    (Ids_for_export.union
       (PC.all_ids_for_export closure_types)
       (PV.all_ids_for_export closure_var_types))

let function_decl_types t = t.function_decls

let closure_types t = PC.to_map t.closure_types

let closure_var_types t = PV.to_map t.closure_var_types

let find_function_declaration t closure_id : _ Or_bottom.t =
  match Closure_id.Map.find closure_id t.function_decls with
  | exception Not_found -> Bottom
  | func_decl -> Ok func_decl

let map_function_decl_types { function_decls; closure_types; closure_var_types }
    ~(f : FDT.t -> FDT.t Or_bottom.t) : _ Or_bottom.t =
  (* CR mshinwell: This needs to deal with [closure_types] too. Deferring until
     new approach for [Rec_info] is sorted out. *)
  let bottom = ref false in
  let function_decls =
    Closure_id.Map.map
      (fun function_decl ->
        match f function_decl with
        | Ok function_decl -> function_decl
        | Bottom ->
          bottom := true;
          function_decl)
      function_decls
  in
  if !bottom
  then Bottom
  else
    let t = { function_decls; closure_types; closure_var_types } in
    Ok t

let map_closure_types { function_decls; closure_types; closure_var_types }
    ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t) : _ Or_bottom.t =
  let closure_types = Product.Closure_id_indexed.map_types closure_types ~f in
  Or_bottom.map closure_types ~f:(fun closure_types ->
      { function_decls; closure_types; closure_var_types })

let fields_kind _ = Flambda_kind.value
