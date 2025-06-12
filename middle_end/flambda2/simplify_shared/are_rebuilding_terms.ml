(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = bool
(* [true] if rebuilding, [false] if not rebuilding *)

let are_rebuilding = true

let are_not_rebuilding = false

let print ppf t = Format.fprintf ppf "%b" t

let do_rebuild_terms t = t

let do_not_rebuild_terms t = not t
