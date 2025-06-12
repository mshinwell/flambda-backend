(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** An internal type for the data_flow graph *)
type t

(** Printing function *)
val print : Format.formatter -> t -> unit

(** Create the data flow graph *)
val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  code_age_relation:Code_age_relation.t ->
  used_value_slots:Name_occurrences.t Or_unknown.t ->
  code_ids_to_never_delete:Code_id.Set.t ->
  Flow_types.Continuation_info.t Continuation.Map.t ->
  t

(** Run the required names analysis *)
val required_names : t -> Flow_types.Data_flow_result.t
