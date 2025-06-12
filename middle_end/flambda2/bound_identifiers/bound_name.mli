(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A [Name] equipped with the extra information required to have it in binding
    position. *)

type t

val create : Name.t -> Name_mode.t -> t

val create_var : Bound_var.t -> t

val create_symbol : Symbol.t -> t

val name : t -> Name.t

val name_mode : t -> Name_mode.t

val is_symbol : t -> bool

val print : Format.formatter -> t -> unit
