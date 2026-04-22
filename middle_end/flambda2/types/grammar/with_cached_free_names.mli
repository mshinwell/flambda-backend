(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A wrapper that caches free names and delays renamings.

    Each [t] represents a ['descr] together with a renaming that should be
    applied to it to yield the actual value being represented. Renamings applied
    via [apply_renaming] are accumulated lazily rather than traversing the descr
    immediately; they are forced only when the descr is required (typically via
    [descr]). *)

type 'descr t

val create : 'descr -> 'descr t

val descr :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) -> 'descr t -> 'descr

(** [peek_descr t] returns the underlying [descr] without forcing any delayed
    renaming. The returned value may therefore be stale; in particular, any
    [Simple.t] or sub-term contained within it has not yet had the delayed
    renaming applied. This is only safe to use when the caller is looking at
    information that is not altered by renaming (for example the top-level
    constructor tag of a sum type). *)
val peek_descr : 'descr t -> 'descr

(** [peek_delayed_renaming t] returns the renaming that is pending on [t].
    Together with [peek_descr], this allows manual forcing of specific fields
    without having to provide an [apply_renaming_descr]. *)
val peek_delayed_renaming : _ t -> Renaming.t

val print :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
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
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
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
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  to_project:Variable.Set.t ->
  project_descr:('descr -> 'descr) ->
  'descr t ->
  'descr t
