(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type primitive_transform_result = private
  | Primitive of Lambda.primitive * Lambda.lambda list * Lambda.scoped_location
  | Transformed of Lambda.lambda

val rec_catch_for_while_loop :
  Lambda_to_flambda_env.t ->
  Lambda.lambda ->
  Lambda.lambda ->
  Lambda_to_flambda_env.t * Lambda.lambda

val rec_catch_for_for_loop :
  Lambda_to_flambda_env.t ->
  Lambda.scoped_location ->
  Ident.t ->
  Lambda.debug_uid ->
  Lambda.lambda ->
  Lambda.lambda ->
  Asttypes.direction_flag ->
  Lambda.lambda ->
  Lambda_to_flambda_env.t * Lambda.lambda

val switch_for_if_then_else :
  cond:Lambda.lambda ->
  ifso:Lambda.lambda ->
  ifnot:Lambda.lambda ->
  kind:Lambda.layout ->
  Lambda.lambda

val transform_primitive :
  Lambda_to_flambda_env.t ->
  Lambda.primitive ->
  (* CR mshinwell: consider [Ident.t list] instead for the arguments. *)
  Lambda.lambda list ->
  Lambda.scoped_location ->
  Lambda_to_flambda_env.t * primitive_transform_result
