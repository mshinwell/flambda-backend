(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | No_coeffects
  | Has_coeffects

let [@ocamlformat "disable"] print ppf co =
  match co with
  | No_coeffects -> Format.fprintf ppf "No_coeffects"
  | Has_coeffects -> Format.fprintf ppf "Has_coeffects"

let compare co1 co2 =
  match co1, co2 with
  | No_coeffects, No_coeffects -> 0
  | No_coeffects, Has_coeffects -> -1
  | Has_coeffects, Has_coeffects -> 0
  | Has_coeffects, No_coeffects -> 1

let join co1 co2 =
  match co1, co2 with
  | No_coeffects, No_coeffects -> No_coeffects
  | No_coeffects, Has_coeffects
  | Has_coeffects, Has_coeffects
  | Has_coeffects, No_coeffects ->
    Has_coeffects

let from_lambda (ce : Primitive.coeffects) : t =
  match ce with No_coeffects -> No_coeffects | Has_coeffects -> Has_coeffects
