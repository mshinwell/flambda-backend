(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Contents of middle-end-specific portion of .cmx files when using Flambda. *)

type t

type raw

val to_raw : t -> raw * Oxcaml_utils.File_sections.t

val from_raw : sections:Oxcaml_utils.File_sections.t -> raw -> t

val create :
  final_typing_env:Flambda2_types.Typing_env.Serializable.t ->
  all_code:Exported_code.t ->
  exported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  t

val import_typing_env_and_code :
  t -> Flambda2_types.Typing_env.Serializable.t * Exported_code.t

val exported_offsets : t -> Exported_offsets.t

val with_exported_offsets : t -> Exported_offsets.t -> t

(** Aggregate several cmx into one for packs *)
val merge : t option -> t option -> t option

(** For ocamlobjinfo *)
val print :
  print_typing_env:bool ->
  print_code:bool ->
  print_offsets:bool ->
  Format.formatter ->
  t ->
  unit
