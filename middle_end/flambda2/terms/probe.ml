(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type desc =
  { name : string;
    enabled_at_init : bool
  }

type t = desc option

let from_lambda (p : Lambda.probe) =
  match p with
  | None -> None
  | Some { name : string; enabled_at_init : bool } ->
    Some { name; enabled_at_init }

let print ppf t =
  match t with
  | None -> Format.pp_print_string ppf "()"
  | Some { name; enabled_at_init } ->
    Format.pp_print_string ppf name;
    if enabled_at_init then Format.pp_print_string ppf "enabled_at_init"
