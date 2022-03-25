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

include Int_ids.Variable

let create_with_same_name_as_ident ?user_visible ident : t =
  create ?user_visible (Ident.name ident)

let rename ?append t =
  let name = match append with None -> name t | Some s -> name t ^ s in
  let user_visible = if user_visible t then Some () else None in
  create ?user_visible name

let raw_name = name

let unique_name t = name t ^ string_of_int (name_stamp t)
