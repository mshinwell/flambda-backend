(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** One or more [Continuation]s, in an ordered list, equipped with operations
    that mean such list can be used in binding position within a
    [Name_abstraction] value. *)

type t

val create : Continuation.t list -> t

include Bindable.S with type t := t
