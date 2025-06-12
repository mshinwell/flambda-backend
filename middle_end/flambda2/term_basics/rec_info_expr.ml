(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Int_ids.Rec_info_expr

let rec apply_renaming orig renaming =
  match orig with
  | Const _ -> orig
  | Var dv ->
    let new_dv = Renaming.apply_variable renaming dv in
    if dv == new_dv then orig else var new_dv
  | Succ t ->
    let new_t = apply_renaming t renaming in
    if t == new_t then orig else succ new_t
  | Unroll_to (unroll_depth, t) ->
    let new_t = apply_renaming t renaming in
    if t == new_t then orig else unroll_to unroll_depth new_t

let rec free_names_in_mode t mode =
  match t with
  | Const _ -> Name_occurrences.empty
  | Var dv -> Name_occurrences.singleton_variable dv mode
  | Succ t | Unroll_to (_, t) -> free_names_in_mode t mode

let free_names t = free_names_in_mode t Name_mode.normal

let free_names_in_types t = free_names_in_mode t Name_mode.in_types

let rec ids_for_export = function
  | Const _ -> Ids_for_export.empty
  | Var dv -> Ids_for_export.add_variable Ids_for_export.empty dv
  | Succ t | Unroll_to (_, t) -> ids_for_export t
