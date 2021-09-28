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

module Descr = struct
  type 'head t =
    | No_alias of 'head
    | Equals of Simple.t

  let print ~print_head ppf t =
    match t with
    | No_alias head -> print_head ppf head
    | Equals simple ->
      Format.fprintf ppf "@[(@<0>%s=@<0>%s %a)@]" (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Simple.print simple

  let apply_renaming ~apply_renaming_head t renaming =
    if Renaming.is_empty renaming
    then t
    else
      match t with
      | No_alias head ->
        let head' = apply_renaming_head head renaming in
        if head == head' then t else No_alias head'
      | Equals simple ->
        let simple' = Simple.apply_renaming simple renaming in
        if simple == simple' then t else Equals simple'

  let free_names ~free_names_head t =
    match t with
    | No_alias head -> free_names_head head
    | Equals simple ->
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Simple.free_names simple) Name_mode.in_types
end

module WDP = With_delayed_permutation

type 'head t = 'head Descr.t WDP.t Or_unknown_or_bottom.t

let descr ~apply_renaming_head ~free_names_head t : _ Descr.t =
  WDP.descr
    ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
    ~free_names_descr:(Descr.free_names ~free_names_head)
    t

let peek_descr t : _ Descr.t = WDP.peek_descr t

let print ~print_head ~apply_renaming_head ~free_names_head ppf (t : _ t) =
  let colour = Flambda_colours.top_or_bottom_type () in
  match t with
  | Unknown ->
    if Flambda_features.unicode ()
    then
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s" colour
        (Flambda_colours.normal ())
    else Format.fprintf ppf "@<0>%sT@<0>%s" colour (Flambda_colours.normal ())
  | Bottom ->
    if Flambda_features.unicode ()
    then
      Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s" colour
        (Flambda_colours.normal ())
    else Format.fprintf ppf "@<0>%s_|_@<0>%s" colour (Flambda_colours.normal ())
  | Ok wdp ->
    WDP.print ~print_descr:(Descr.print ~print_head)
      ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
      ~free_names_descr:(Descr.free_names ~free_names_head)
      ppf wdp

let create_no_alias head = Ok (WDP.create (Descr.No_alias head))

let create_equals simple = Ok (WDP.create (Descr.Equals simple))

let bottom : _ t = Bottom

let unknown : _ t = Unknown

let create head = Ok (create_no_alias head)

let is_obviously_bottom (t : _ t) =
  match t with Bottom -> true | Unknown | Ok _ -> false

let is_obviously_unknown (t : _ t) =
  match t with Unknown -> true | Bottom | Ok _ -> false

let get_alias_exn ~apply_renaming_head ~free_names_head (t : _ t) =
  match t with
  | Unknown | Bottom -> raise Not_found
  | Ok wdp -> (
    match WDP.peek_descr wdp with
    | No_alias _ -> raise Not_found
    | Equals _ -> (
      match WDP.descr ~apply_renaming_head ~free_names_head wdp with
      | Equals alias -> alias
      | No_alias _ -> assert false))

let apply_coercion ~apply_coercion_head ~apply_renaming_head ~free_names_head
    coercion t : _ Or_bottom.t =
  match descr ~apply_renaming_head ~free_names_head t with
  | Equals simple -> begin
    match Simple.apply_coercion simple coercion with
    | None -> Bottom
    | Some simple -> Ok (create_equals simple)
  end
  | No_alias Unknown -> Ok t
  | No_alias Bottom -> Bottom
  | No_alias (Ok head) ->
    Or_bottom.map (apply_coercion_head head coercion) ~f:(fun head ->
        create head)

let all_ids_for_export ~apply_renaming_head ~free_names_head
    ~all_ids_for_export_head t =
  match descr ~apply_renaming_head ~free_names_head t with
  | No_alias Bottom | No_alias Unknown -> Ids_for_export.empty
  | No_alias (Ok head) -> all_ids_for_export_head head
  | Equals simple -> Ids_for_export.from_simple simple

let apply_renaming ~apply_renaming_head t renaming =
  WDP.apply_renaming
    ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
    t renaming

let free_names ~apply_renaming_head ~free_names_head t =
  WDP.free_names
    ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
    ~free_names_descr:(Descr.free_names ~free_names_head)
    t
