(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type continuation_handler =
  { handler : Flambda.Continuation_handler.t;
    free_names : Name_occurrences.t
  }

type continuation_handlers =
  { handlers : Flambda.Continuation_handler.t Continuation.Lmap.t;
    free_names : Name_occurrences.t
  }

type t =
  { expr : Flambda.expr;
    free_names : Name_occurrences.t
  }

val create_let : Bound_pattern.t -> Flambda.named -> body:t -> t

val create_continuation_handler :
  Bound_parameters.t ->
  handler:t ->
  is_exn_handler:bool ->
  is_cold:bool ->
  continuation_handler

val create_continuation_handlers :
  continuation_handler Continuation.Lmap.t -> continuation_handlers

val create_non_recursive_let_cont :
  Continuation.t -> continuation_handler -> body:t -> t

val create_recursive_let_cont :
  invariant_params:Bound_parameters.t ->
  continuation_handler Continuation.Lmap.t ->
  body:t ->
  t

val from_expr : expr:Flambda.expr -> free_names:Name_occurrences.t -> t
