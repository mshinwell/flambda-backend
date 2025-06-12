(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

val make_decision :
  inlining_arguments:Inlining_arguments.t ->
  inline:Inline_attribute.t ->
  stub:bool ->
  cost_metrics:Cost_metrics.t ->
  is_a_functor:bool ->
  recursive:Recursive.t ->
  Function_decl_inlining_decision_type.t
