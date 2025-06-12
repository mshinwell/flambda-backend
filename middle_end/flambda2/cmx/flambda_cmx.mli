(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Dumping and restoring of simplification environment information to and from
    .cmx files. *)

type loader

val create_loader :
  get_module_info:(Compilation_unit.t -> Flambda_cmx_format.t option) -> loader

val get_imported_names : loader -> unit -> Name.Set.t

val get_imported_code : loader -> unit -> Exported_code.t

val load_cmx_file_contents :
  loader ->
  Compilation_unit.t ->
  Flambda2_types.Typing_env.Serializable.t option

val load_symbol_approx :
  loader -> Symbol.t -> Code_or_metadata.t Value_approximation.t

val prepare_cmx_file_contents :
  final_typing_env:Flambda2_types.Typing_env.t option ->
  module_symbol:Symbol.t ->
  used_value_slots:Value_slot.Set.t ->
  exported_offsets:Exported_offsets.t ->
  Exported_code.t ->
  Name_occurrences.t * Flambda_cmx_format.t option

val prepare_cmx_from_approx :
  approxs:Code_or_metadata.t Value_approximation.t Symbol.Map.t ->
  module_symbol:Symbol.t ->
  exported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  Exported_code.t ->
  Name_occurrences.t * Flambda_cmx_format.t option
