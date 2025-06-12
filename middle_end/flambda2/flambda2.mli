(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Translate Lambda code to Cmm using Flambda 2. *)

(** This function is not currently re-entrant. *)
val lambda_to_cmm :
  ppf_dump:Format.formatter ->
  prefixname:string ->
  keep_symbol_tables:bool ->
  Lambda.program ->
  Cmm.phrase list

val get_module_info :
  Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option
