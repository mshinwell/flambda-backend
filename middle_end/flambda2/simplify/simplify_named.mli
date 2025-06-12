(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of the right-hand sides of [Let] bindings. *)

val simplify_named :
  Downwards_acc.t ->
  Bound_pattern.t ->
  Flambda.Named.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Simplify_named_result.t Or_invalid.t * Removed_operations.t
