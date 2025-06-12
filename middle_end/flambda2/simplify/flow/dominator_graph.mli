(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module G : Strongly_connected_components.S with module Id := Variable

(** An internal type for the data_flow graph *)
type t =
  { required_names : Name.Set.t;
    params_kind : Flambda_kind.With_subkind.t Variable.Map.t;
    graph : G.directed_graph;
    dominator_roots : Variable.Set.t
        (* variables that are dominated only by themselves, usually because a
           constant or a symbol can flow to that variable, and thus that
           variable cannot be dominated by another variable. *)
  }

type alias_map = Variable.t Variable.Map.t

(** Create the data flow graph *)
val create :
  required_names:Name.Set.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  Flow_types.Continuation_info.t Continuation.Map.t ->
  t

val dominator_analysis : t -> alias_map

val aliases_kind : t -> alias_map -> Flambda_kind.t Variable.Map.t

module Dot : sig
  (** Printing function *)
  val print :
    ctx:int ->
    print_name:string ->
    doms:Variable.t Variable.Map.t ->
    Format.formatter ->
    t ->
    unit
end
