(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Non_recursive
  | Recursive

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Non_recursive -> Format.pp_print_string ppf "Non_recursive"
  | Recursive -> Format.pp_print_string ppf "Recursive"

let equal t1 t2 =
  match t1, t2 with
  | Non_recursive, Non_recursive | Recursive, Recursive -> true
  | Non_recursive, Recursive | Recursive, Non_recursive -> false
