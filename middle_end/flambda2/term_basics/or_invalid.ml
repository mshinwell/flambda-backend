(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Ok of 'a
  | Invalid

let print f ppf t =
  match t with
  | Ok contents -> Format.fprintf ppf "@[(Ok %a)@]" f contents
  | Invalid -> Format.pp_print_string ppf "Invalid"

let map t ~f =
  match t with Ok contents -> Ok (f contents) | Invalid -> Invalid
