(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val zero : t

val from_size : Code_size.t -> t

val size : t -> Code_size.t

val removed : t -> Removed_operations.t

val print : Format.formatter -> t -> unit

val ( + ) : t -> t -> t

type code_characteristics =
  { cost_metrics : t;
    params_arity : int
  }

val set_of_closures :
  find_code_characteristics:(Code_id.t -> code_characteristics) ->
  Set_of_closures.t ->
  t

val increase_due_to_let_expr :
  is_phantom:bool -> cost_metrics_of_defining_expr:t -> t

val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t

val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t

val notify_added : code_size:Code_size.t -> t -> t

val notify_removed : operation:Removed_operations.t -> t -> t

val evaluate : args:Inlining_arguments.t -> t -> float

val equal : t -> t -> bool
