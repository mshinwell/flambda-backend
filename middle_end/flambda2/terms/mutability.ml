(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Mutable
  | Immutable
  | Immutable_unique

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Mutable -> Format.pp_print_string ppf "Mutable"
  | Immutable -> Format.pp_print_string ppf "Immutable"
  | Immutable_unique ->
    Format.pp_print_string ppf "Immutable_unique"

let compare t1 t2 =
  match t1, t2 with
  | Mutable, Mutable | Immutable, Immutable | Immutable_unique, Immutable_unique
    ->
    0
  | Mutable, (Immutable | Immutable_unique) -> -1
  | Immutable, Immutable_unique -> -1
  | Immutable, Mutable -> 1
  | Immutable_unique, (Mutable | Immutable) -> 1

let join t1 t2 =
  match t1, t2 with
  | Immutable, Immutable -> Immutable
  | Immutable_unique, Immutable_unique
  | Immutable, Immutable_unique
  | Immutable_unique, Immutable ->
    Immutable_unique
  | Mutable, (Mutable | Immutable | Immutable_unique)
  | (Immutable | Immutable_unique), Mutable ->
    Mutable

let to_asttypes t : Asttypes.mutable_flag =
  match t with
  | Mutable -> Mutable
  | Immutable -> Immutable
  | Immutable_unique -> Immutable

let from_lambda (flag : Lambda.mutable_flag) : t =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable
  | Immutable_unique -> Immutable_unique

let is_mutable t =
  match t with Mutable -> true | Immutable | Immutable_unique -> false
