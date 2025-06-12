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

val print : Format.formatter -> t -> unit

val should_loopify : t -> bool

val was_loopified : t -> bool

val equal : t -> t -> bool
