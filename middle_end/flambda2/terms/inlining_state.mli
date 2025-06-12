(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Abstracts the state used during inlining. *)

type t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val default : round:int -> t

val create : arguments:Inlining_arguments.t -> depth:int -> t

val depth : t -> int

val increment_depth : t -> by:int -> t

val is_depth_exceeded : t -> bool

val meet : t -> t -> t

val with_arguments : Inlining_arguments.t -> t -> t

val arguments : t -> Inlining_arguments.t
