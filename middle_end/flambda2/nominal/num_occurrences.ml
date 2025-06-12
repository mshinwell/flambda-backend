(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Zero
  | One
  | More_than_one

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Zero -> Format.fprintf ppf "Zero"
  | One -> Format.fprintf ppf "One"
  | More_than_one -> Format.fprintf ppf "More_than_one"
