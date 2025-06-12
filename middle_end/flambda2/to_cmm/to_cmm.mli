(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* Translate Flambda compilation units into Cmm *)

(** Translate a compilation unit. *)
val unit :
  offsets:Exported_offsets.t ->
  all_code:Exported_code.t ->
  reachable_names:Name_occurrences.t ->
  Flambda_unit.t ->
  Cmm.phrase list
