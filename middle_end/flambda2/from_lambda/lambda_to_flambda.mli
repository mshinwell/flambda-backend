(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Conversion from Lambda to Flambda. *)

val lambda_to_flambda :
  mode:'mode Flambda_features.mode ->
  big_endian:bool ->
  cmx_loader:Flambda_cmx.loader ->
  compilation_unit:Compilation_unit.t ->
  module_block_size_in_words:int ->
  Lambda.lambda ->
  'mode Closure_conversion.close_program_result
