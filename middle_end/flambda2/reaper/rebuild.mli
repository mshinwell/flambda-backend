(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type result =
  { body : Flambda.Expr.t;
    free_names : Name_occurrences.t;
    all_code : Code.t Code_id.Map.t;
    slot_offsets : Slot_offsets.t
  }

val rebuild :
  continuation_info:Traverse_acc.continuation_info Continuation.Map.t ->
  fixed_arity_continuations:Continuation.Set.t ->
  Flambda_kind.t Name.Map.t ->
  Dep_solver.result ->
  (Code_id.t -> Code_metadata.t) ->
  Rev_expr.t ->
  result
