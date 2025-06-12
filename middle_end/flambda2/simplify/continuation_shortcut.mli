(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A continuation shortcut is an anonymous continuation whose body is a
    single [Apply_cont] expression to another continuation. *)

type t

val print : Format.formatter -> t -> unit

val create : params:Bound_parameters.t -> Continuation.t -> Simple.t list -> t

val apply : t -> Simple.t list -> Continuation.t * Simple.t list

val continuation : t -> Continuation.t

val to_alias : t -> Continuation.t option
