(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module T = Type_grammar
module TEE = Typing_env.Typing_env_extension

module Make (Index : Product_intf.Index) = struct
  let [@ocamlformat "disable"] print ppf { components_by_index; kind = _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Type_grammar.print) components_by_index

  let fields_kind t = t.kind

  let create kind components_by_index =
    (* CR mshinwell: Check that the types are all of the same kind *)
    { components_by_index; kind }

  let create_top kind = create kind Index.Map.empty

  let width t =
    Targetint_31_63.Imm.of_int (Index.Map.cardinal t.components_by_index)

  let components t = Index.Map.data t.components_by_index

  let project t index : _ Or_unknown.t =
    match Index.Map.find_opt index t.components_by_index with
    | None -> Unknown
    | Some ty -> Known ty

  let apply_renaming { components_by_index; kind } renaming =
    let components_by_index =
      (* CR-someday mshinwell: some loss of sharing here, potentially *)
      Index.Map.filter_map
        (fun index ty ->
          if Index.remove_on_import index renaming
          then None
          else Some (Type_grammar.apply_renaming ty renaming))
        components_by_index
    in
    { kind; components_by_index }

  let free_names { components_by_index; kind = _ } =
    Index.Map.fold
      (fun _index ty free_names ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      components_by_index Name_occurrences.empty

  let all_ids_for_export { components_by_index; kind = _ } =
    Index.Map.fold
      (fun _index ty ids ->
        Ids_for_export.union (Type_grammar.all_ids_for_export ty) ids)
      components_by_index Ids_for_export.empty

  let map_types ({ components_by_index; kind } as t)
      ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t) : _ Or_bottom.t =
    let found_bottom = ref false in
    let components_by_index' =
      Index.Map.map_sharing
        (fun ty ->
          match f ty with
          | Bottom ->
            found_bottom := true;
            ty
          | Ok ty -> ty)
        components_by_index
    in
    if !found_bottom
    then Bottom
    else if components_by_index == components_by_index'
    then Ok t
    else Ok { components_by_index = components_by_index'; kind }

  let to_map t = t.components_by_index
end

module Closure_id_index = struct
  include Closure_id

  let remove_on_import _ _ = false
end

module Closure_id_indexed = Make (Closure_id_index)

module Var_within_closure_index = struct
  include Var_within_closure

  let remove_on_import var renaming =
    not (Renaming.closure_var_is_used renaming var)
end

module Var_within_closure_indexed = Make (Var_within_closure_index)

module Int_indexed = struct
  (* CR mshinwell: Add [Or_bottom]. However what should [width] return for
     [Bottom]? Maybe we can circumvent that question if removing [Row_like]. *)

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      (Array.to_list t.fields)

  let fields_kind t = t.kind

  let create_from_list kind tys = { kind; fields = Array.of_list tys }

  let create_top kind = { kind; fields = [||] }

  let width t = Targetint_31_63.Imm.of_int (Array.length t.fields)

  let components t = Array.to_list t.fields

  let project t index : _ Or_unknown.t =
    if Array.length t.fields <= index then Unknown else Known t.fields.(index)

  let apply_renaming { kind; fields } perm =
    let fields = Array.copy fields in
    for i = 0 to Array.length fields - 1 do
      fields.(i) <- Type_grammar.apply_renaming fields.(i) perm
    done;
    { kind; fields }

  let free_names t =
    Array.fold_left
      (fun free_names ty ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      Name_occurrences.empty t.fields

  let all_ids_for_export t =
    Array.fold_left
      (fun ids ty ->
        Ids_for_export.union (Type_grammar.all_ids_for_export ty) ids)
      Ids_for_export.empty t.fields

  let map_types t ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t) :
      _ Or_bottom.t =
    let found_bottom = ref false in
    let fields = Array.copy t.fields in
    for i = 0 to Array.length fields - 1 do
      match f fields.(i) with
      | Bottom -> found_bottom := true
      | Ok typ -> fields.(i) <- typ
    done;
    if !found_bottom then Bottom else Ok { t with fields }
end
