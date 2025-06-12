(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Delay
  | Strict

let print ppf = function
  | Delay -> Format.fprintf ppf "Delay"
  | Strict -> Format.fprintf ppf "Strict"

let compare placement1 placement2 =
  match placement1, placement2 with
  | Delay, Delay -> 0
  | Delay, Strict -> -1
  | Strict, Strict -> 0
  | Strict, Delay -> 1

let join placement1 placement2 =
  match placement1, placement2 with
  | Delay, Delay -> Delay
  | Delay, Strict | Strict, Strict | Strict, Delay -> Strict
