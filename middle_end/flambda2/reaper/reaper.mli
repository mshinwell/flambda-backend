(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

val run :
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  Flambda_unit.t ->
  Flambda_unit.t * Name_occurrences.t * Exported_code.t * Slot_offsets.t
