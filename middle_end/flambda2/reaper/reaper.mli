(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Reaper for non-LTO mode, single compilation unit with .cmx file emission. *)
val run :
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  Flambda_unit.t ->
  Flambda_unit.t * Name_occurrences.t * Exported_code.t * Slot_offsets.t

(** Reaper for LTO mode *)

type 'a rebuild_mode =
  | Normal_pipeline of 'a
  | Starting_from_reaper

val traverse : Flambda_unit.t -> Traverse.result

val dep_solver : Traverse.result -> Dep_solver.result

val rebuild :
  unit rebuild_mode ->
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  Flambda_unit.t ->
  Traverse.result ->
  Dep_solver.result ->
  Flambda_unit.t
  * Name_occurrences.t
  * Exported_code.t rebuild_mode
  * Slot_offsets.t
