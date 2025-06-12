(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Dataflow analysis.

    This module aims mainly at tracking uses of variables (other things may be
    added later on), with the aim of:

    - removing unused parameters of *recursive* continuations;

    - moving allocations out of the hot path of recursive continuations (e.g.
    the allocation of a float that was unboxed by the simplifier). *)

(** Analyze the uses. *)
val analyze :
  ?speculative:bool ->
  ?print_name:string ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  code_age_relation:Code_age_relation.t ->
  used_value_slots:Name_occurrences.t Or_unknown.t ->
  code_ids_to_never_delete:Code_id.Set.t ->
  specialization_map:Continuation.t Continuation_callsite_map.t ->
  Flow_types.Acc.t ->
  Flow_types.Flow_result.t

(** [true] iff the mutable unboxing pass actually did unbox things *)
val did_perform_mutable_unboxing : Flow_types.Flow_result.t -> bool
