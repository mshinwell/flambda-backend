(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** This type represents maps from continuations to (maps from) rewrite ids
    to values. *)
type 'a t = 'a Apply_cont_rewrite_id.Map.t Continuation.Map.t

(** Print function. *)
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** The empty map *)
val empty : 'a t

(** Find the value bound to a pair of a continuation and rewrite id.
    @raise Not_found if either the continuation or rewrite ids are not bound. *)
val find : Continuation.t -> Apply_cont_rewrite_id.t -> 'a t -> 'a

(** Add a binding to the callsite map.
    @raise Misc.Fatal_error if there is a pre-existing binding for the keys. *)
val add : Continuation.t -> Apply_cont_rewrite_id.t -> 'a -> 'a t -> 'a t
