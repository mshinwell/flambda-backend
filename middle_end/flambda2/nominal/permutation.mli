(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Generic module for handling permutations. *)

module Make (N : Container_types.S) : sig
  type t

  val empty : t

  val print : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val apply : t -> N.t -> N.t

  val compose_one : first:t -> N.t -> N.t -> t

  val compose_one_fresh : t -> N.t -> fresh:N.t -> t

  val compose : second:t -> first:t -> t

  val inverse : t -> t
end
