(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type result

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result

val has_use : result -> Code_id_or_name.t -> bool

val field_used :
  result -> Code_id_or_name.t -> Global_flow_graph.Field.t -> bool

(** Color of node when producing the graph as a .dot *)
val print_color : result -> Code_id_or_name.t -> string
