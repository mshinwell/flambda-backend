(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Management of cached free names. *)

type 'descr t =
  { descr : 'descr;
    mutable free_names : Name_occurrences.t option
  }

val create : 'descr -> 'descr t

val descr : 'descr t -> 'descr

val print :
  print_descr:(Format.formatter -> 'descr -> unit) ->
  Format.formatter ->
  'descr t ->
  unit

val apply_renaming :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Renaming.t ->
  'descr t

val free_names :
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Name_occurrences.t

val free_names_no_cache :
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Name_occurrences.t

val remove_unused_value_slots_and_shortcut_aliases :
  remove_unused_value_slots_and_shortcut_aliases_descr:
    ('descr ->
    used_value_slots:Value_slot.Set.t ->
    canonicalise:(Simple.t -> Simple.t) ->
    'descr) ->
  'descr t ->
  used_value_slots:Value_slot.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  'descr t

val project_variables_out :
  free_names_descr:('descr -> Name_occurrences.t) ->
  to_project:Variable.Set.t ->
  project_descr:('descr -> 'descr) ->
  'descr t ->
  'descr t
