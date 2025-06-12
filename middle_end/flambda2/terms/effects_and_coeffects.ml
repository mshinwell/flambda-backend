(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = Effects.t * Coeffects.t * Placement.t

let print fmt (eff, coeff, dup) =
  Format.fprintf fmt "%a * %a * %a" Effects.print eff Coeffects.print coeff
    Placement.print dup

let compare (e1, c1, d1) (e2, c2, d2) =
  match Effects.compare e1 e2 with
  | 0 -> (
    match Coeffects.compare c1 c2 with
    | 0 -> Placement.compare d1 d2
    | res -> res)
  | res -> res

(* Some useful constants *)
let pure : t = No_effects, No_coeffects, Strict

let pure_can_be_duplicated : t = No_effects, No_coeffects, Delay

let all : t = Arbitrary_effects, Has_coeffects, Strict

let read : t = No_effects, Has_coeffects, Strict

(* Joining effects, coeffects and placement *)
let join (eff1, coeff1, dup1) (eff2, coeff2, dup2) =
  Effects.join eff1 eff2, Coeffects.join coeff1 coeff2, Placement.join dup1 dup2
