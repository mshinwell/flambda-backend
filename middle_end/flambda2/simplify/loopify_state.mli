(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  | Do_not_loopify
  | Loopify of Continuation.t

val print : Format.formatter -> t -> unit

val do_not_loopify : t

val loopify : Continuation.t -> t
