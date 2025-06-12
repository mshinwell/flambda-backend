(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module Id : sig
  type t = int

  val flags : t -> int

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int
end

module Make (E : sig
  type t

  val flags : int

  val print : Format.formatter -> t -> unit

  val hash : t -> int

  val equal : t -> t -> bool
end) : sig
  type t

  val create : unit -> t

  val add : t -> E.t -> Id.t

  val find : t -> Id.t -> E.t
end
