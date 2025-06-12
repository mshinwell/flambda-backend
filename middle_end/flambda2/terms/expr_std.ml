(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Functionality supported by all expression-like modules. *)

module type S = sig
  type t

  val print : Format.formatter -> t -> unit

  include Contains_names.S with type t := t
end

module type S_no_free_names = sig
  type t

  val print : Format.formatter -> t -> unit

  val apply_renaming : t -> Renaming.t -> t
end
