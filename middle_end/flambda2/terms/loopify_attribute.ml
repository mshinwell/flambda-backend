(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Always_loopify
  | Never_loopify
  | Already_loopified
  | Default_loopify_and_tailrec
  | Default_loopify_and_not_tailrec

let print ppf = function
  | Always_loopify -> Format.fprintf ppf "Always_loopify"
  | Never_loopify -> Format.fprintf ppf "Never_loopify"
  | Already_loopified -> Format.fprintf ppf "Already_loopified"
  | Default_loopify_and_tailrec ->
    Format.fprintf ppf "Default_loopify_and_tailrec"
  | Default_loopify_and_not_tailrec ->
    Format.fprintf ppf "Default_loopify_and_not_tailrec"

let should_loopify = function
  | Always_loopify | Default_loopify_and_tailrec -> true
  | Never_loopify | Already_loopified | Default_loopify_and_not_tailrec -> false

let was_loopified = function
  | Always_loopify | Already_loopified | Default_loopify_and_tailrec -> true
  | Never_loopify | Default_loopify_and_not_tailrec -> false

let equal t1 t2 =
  match t1, t2 with
  | Always_loopify, Always_loopify
  | Never_loopify, Never_loopify
  | Already_loopified, Already_loopified
  | Default_loopify_and_tailrec, Default_loopify_and_tailrec
  | Default_loopify_and_not_tailrec, Default_loopify_and_not_tailrec ->
    true
  | ( ( Always_loopify | Never_loopify | Already_loopified
      | Default_loopify_and_tailrec | Default_loopify_and_not_tailrec ),
      _ ) ->
    false
