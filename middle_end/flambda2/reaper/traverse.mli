(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type result =
  { holed : Rev_expr.t;
    deps : Global_flow_graph.graph;
    kinds : Flambda_kind.t Name.Map.t;
    fixed_arity_continuations : Continuation.Set.t;
    continuation_info : Traverse_acc.continuation_info Continuation.Map.t
  }

val run : Flambda_unit.t -> result
