(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

type raw

include Contains_ids.S with type t := t

val apply_renaming : Code_id.t Code_id.Map.t -> Renaming.t -> t -> t

val print : Format.formatter -> t -> unit

val print_view : Format.formatter -> t -> unit

val empty : t

val free_names : t -> Name_occurrences.t

val add_code : keep_code:(Code_id.t -> bool) -> Code.t Code_id.Map.t -> t -> t

val mark_as_imported : t -> t

val merge : t -> t -> t

val mem : Code_id.t -> t -> bool

(** This function raises an exception if the code ID is unbound. *)
val find_exn : t -> Code_id.t -> Code_or_metadata.t

(** This function is only really for use in unusual cases where there needs to
    be special handling if a code ID is unbound (see comment in the .ml file) *)
val find : t -> Code_id.t -> Code_or_metadata.t option

val remove_unreachable : reachable_names:Name_occurrences.t -> t -> t

val remove_unused_value_slots_from_result_types_and_shortcut_aliases :
  used_value_slots:Value_slot.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  t ->
  t

val iter_code : t -> f:(Code.t -> unit) -> unit

val from_raw : sections:Oxcaml_utils.File_sections.t -> raw -> t

val to_raw : add_section:(Obj.t -> int) -> t -> raw

val map_raw_index : (int -> int) -> raw -> raw
