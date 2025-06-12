(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Flag indicating whether terms are being rebuilt during simplification. This
    is not just [bool] to enforce that the setting in [DE] is used everywhere. *)

type t

val print : Format.formatter -> t -> unit

(* CR gbury: the terms/names used for creating values and for inspecting them
   would make more sens if they were swapped. *)
val are_rebuilding : t

val are_not_rebuilding : t

val do_rebuild_terms : t -> bool

val do_not_rebuild_terms : t -> bool
