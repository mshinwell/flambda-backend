(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Do_not_loopify
  | Loopify of Continuation.t

let print ppf = function
  | Do_not_loopify -> Format.fprintf ppf "do_not_loopify"
  | Loopify cont ->
    Format.fprintf ppf "@[<hov 1>(loopify@ %a)@]" Continuation.print cont

let do_not_loopify = Do_not_loopify

let loopify cont = Loopify cont
