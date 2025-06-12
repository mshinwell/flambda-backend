(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Whether an expression can be moved around, including whether it can be
    duplicated *)
type t =
  | Delay
      (** The expression should be placed as late as possible, even if it is
          duplicated *)
  | Strict
      (** The expression must not be moved around (it has non-generative
          effects, or coeffects, or doesn't benefit from being bound later). *)

(** Print function. *)
val print : Format.formatter -> t -> unit

(** Comparison function. *)
val compare : t -> t -> int

(** Join *)
val join : t -> t -> t
