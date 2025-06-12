(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val create :
  dom:Dominator_graph.alias_map ->
  dom_graph:Dominator_graph.t ->
  source_info:Flow_types.Acc.t ->
  control_flow_graph:Control_flow_graph.t ->
  required_names:Name.Set.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

val make_result : t -> Flow_types.Mutable_unboxing_result.t * Variable.Set.t

val pp_node : t -> Format.formatter -> Continuation.t -> unit
