(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Function declaration (not call site) inlining annotations. *)

type t =
  | Always_inline
  | Available_inline
  | Never_inline
  | Unroll of int
  | Default_inline

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val number_of_unrolls : t -> int

val from_lambda : Lambda.inline_attribute -> t
