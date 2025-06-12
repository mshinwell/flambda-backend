(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Default
  | Error

let print ppf t =
  match t with
  | Default -> Format.pp_print_string ppf "Default"
  | Error -> Format.pp_print_string ppf "Error"

let equal t1 t2 =
  match t1, t2 with
  | Default, Default | Error, Error -> true
  | Default, Error | Error, Default -> false

let is_default t = match t with Default -> true | Error -> false

let from_lambda (attr : Lambda.poll_attribute) =
  match attr with Default_poll -> Default | Error_poll -> Error

let to_lambda t : Lambda.poll_attribute =
  match t with Default -> Default_poll | Error -> Error_poll
