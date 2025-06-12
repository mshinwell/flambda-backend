(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** The identifiers (implicit and explicit function parameters, together with
    return and exception continuations) bound at a lambda in the term language. *)

type t

val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  params:Bound_parameters.t ->
  my_closure:Variable.t ->
  my_region:Variable.t option ->
  my_ghost_region:Variable.t option ->
  my_depth:Variable.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

val params : t -> Bound_parameters.t

val my_closure : t -> Variable.t

val my_region : t -> Variable.t option

val my_ghost_region : t -> Variable.t option

val my_depth : t -> Variable.t

include Bindable.S with type t := t
