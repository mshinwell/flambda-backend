(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

let simplify_coercion dacc (coercion : Coercion.t) =
  match coercion with
  | Id -> coercion
  | Change_depth { from; to_ } ->
    let from' = Simplify_rec_info_expr.simplify_rec_info_expr dacc from in
    let to_' = Simplify_rec_info_expr.simplify_rec_info_expr dacc to_ in
    if from' == from && to_' == to_
    then coercion
    else Coercion.change_depth ~from:from' ~to_:to_'
