(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Int_ids.Variable

let create_with_same_name_as_ident ?user_visible ident : t =
  create ?user_visible (Ident.name ident)

let rename ?append t =
  let name = match append with None -> name t | Some s -> name t ^ s in
  let user_visible = if user_visible t then Some () else None in
  create ?user_visible name

let is_renamed_version_of t t' =
  (* We only keep track of variables renamed with an empty {append} parameter *)
  String.equal (name t) (name t')

let raw_name = name

let unique_name t = name t ^ string_of_int (name_stamp t)
