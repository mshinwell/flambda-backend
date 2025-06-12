(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** The Flambda representation of a single compilation unit's code. *)

type t

val print : Format.formatter -> t -> unit

val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  toplevel_my_region:Variable.t ->
  toplevel_my_ghost_region:Variable.t ->
  body:Flambda.Expr.t ->
  module_symbol:Symbol.t ->
  used_value_slots:Value_slot.Set.t Or_unknown.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

val toplevel_my_region : t -> Variable.t

val toplevel_my_ghost_region : t -> Variable.t

val module_symbol : t -> Symbol.t

val used_value_slots : t -> Value_slot.Set.t Or_unknown.t

val with_used_value_slots : t -> Value_slot.Set.t -> t

val body : t -> Flambda.Expr.t
