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

[@@@ocaml.warning "+a-30-40-41-42"]

module Pattern = struct
  type t =
    | Set_of_closures of Symbol.t Closure_id.Lmap.t
    | Block_like of Symbol.t

  let set_of_closures bound_symbols = Set_of_closures bound_symbols

  let block_like bound_symbol = Block_like bound_symbol

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Set_of_closures bound_symbols ->
      Format.fprintf ppf "@[<hov 1>(Set_of_closures@ %a)@]"
        (Closure_id.Lmap.print Symbol.print) bound_symbols
    | Block_like bound_symbol ->
      Format.fprintf ppf "@[<hov 1>(Block_like@ %a)@]" Symbol.print bound_symbol

  let apply_renaming t renaming =
    match t with
    | Set_of_closures bound_symbols ->
      let bound_symbols' =
        Closure_id.Lmap.map_sharing
          (Renaming.apply_symbol renaming)
          bound_symbols
      in
      if bound_symbols == bound_symbols'
      then t
      else Set_of_closures bound_symbols'
    | Block_like bound_symbol ->
      let bound_symbol' = Renaming.apply_symbol renaming bound_symbol in
      if bound_symbol == bound_symbol' then t else Block_like bound_symbol'

  let free_names t =
    match t with
    | Set_of_closures bound_symbols ->
      Closure_id.Lmap.fold
        (fun _ bound_symbol free_names ->
          Name_occurrences.add_symbol free_names bound_symbol Name_mode.normal)
        bound_symbols Name_occurrences.empty
    | Block_like bound_symbol ->
      Name_occurrences.singleton_symbol bound_symbol Name_mode.normal

  let being_defined t =
    match t with
    | Set_of_closures bound_symbols ->
      bound_symbols |> Closure_id.Lmap.data |> Symbol.Set.of_list
    | Block_like bound_symbol -> Symbol.Set.singleton bound_symbol

  let being_defined_as_list t =
    match t with
    | Set_of_closures bound_symbols -> bound_symbols |> Closure_id.Lmap.data
    | Block_like bound_symbol -> [bound_symbol]

  let all_ids_for_export t = Ids_for_export.create ~symbols:(being_defined t) ()

  let gc_roots t =
    match t with
    | Set_of_closures bound_symbols ->
      (* Avoid repeated root registration by picking only one of the closures.
         It doesn't matter which one is picked. *)
      [List.hd (Closure_id.Lmap.data bound_symbols)]
    | Block_like bound_symbol -> [bound_symbol]
end

type t = Pattern.t list

let empty = []

let check_pattern_list_invariant pattern_list =
  (* Check that there are no repeated bindings of symbols. *)
  let being_defined =
    List.map Pattern.being_defined_as_list pattern_list |> List.concat
  in
  let being_defined_as_set = Symbol.Set.of_list being_defined in
  if List.compare_length_with being_defined
       (Symbol.Set.cardinal being_defined_as_set)
     <> 0
  then
    Misc.fatal_errorf "Duplicate symbols in pattern list:@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print)
      pattern_list

let create pattern_list =
  if Flambda_features.check_invariants ()
  then check_pattern_list_invariant pattern_list;
  pattern_list

let singleton pattern = [pattern]

let to_list t = t

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print) t

let being_defined t = List.map Pattern.being_defined t |> Symbol.Set.union_list

let apply_renaming t renaming =
  List.map (fun pattern -> Pattern.apply_renaming pattern renaming) t

let free_names t = List.map Pattern.free_names t |> Name_occurrences.union_list

let all_ids_for_export t =
  List.map Pattern.all_ids_for_export t |> Ids_for_export.union_list

let concat t1 t2 = t1 @ t2

let gc_roots t = List.map Pattern.gc_roots t |> List.concat
