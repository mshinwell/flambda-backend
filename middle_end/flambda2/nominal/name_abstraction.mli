(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type [('bindable, 'term) t] is the equivalent of an atom abstraction
    construction "[bindable]term" in nominal sets. *)
type ('bindable, 'term) t

(** Creation of an abstraction (constant-time operation). *)
val create : 'bindable -> 'term -> ('bindable, 'term) t

(** Concretion of an abstraction at a fresh bindable. *)
val pattern_match :
  freshen_bindable:('bindable -> 'bindable) ->
  swap_bindable:('bindable -> guaranteed_fresh:'bindable -> Renaming.t) ->
  apply_renaming_term:('term -> Renaming.t -> 'term) ->
  ('bindable, 'term) t ->
  f:('bindable -> 'term -> 'a) ->
  'a

(** Concretion of a pair of abstractions at the same fresh bindable. *)
val pattern_match_pair :
  freshen_bindable:('bindable -> 'bindable) ->
  swap_bindable:('bindable -> guaranteed_fresh:'bindable -> Renaming.t) ->
  apply_renaming_term:('term -> Renaming.t -> 'term) ->
  ('bindable, 'term) t ->
  ('bindable, 'term) t ->
  f:('bindable -> 'term -> 'term -> 'a) ->
  'a

val print :
  print_bindable:(Format.formatter -> 'bindable -> unit) ->
  print_term:(Format.formatter -> 'term -> unit) ->
  freshen_bindable:('bindable -> 'bindable) ->
  swap_bindable:('bindable -> guaranteed_fresh:'bindable -> Renaming.t) ->
  apply_renaming_term:('term -> Renaming.t -> 'term) ->
  Format.formatter ->
  ('bindable, 'term) t ->
  unit

val free_names :
  free_names_bindable:('bindable -> Name_occurrences.t) ->
  free_names_term:('term -> Name_occurrences.t) ->
  ('bindable, 'term) t ->
  Name_occurrences.t

val apply_renaming :
  apply_renaming_bindable:('bindable -> Renaming.t -> 'bindable) ->
  apply_renaming_term:('term -> Renaming.t -> 'term) ->
  ('bindable, 'term) t ->
  Renaming.t ->
  ('bindable, 'term) t

val all_ids_for_export :
  all_ids_for_export_bindable:('bindable -> Ids_for_export.t) ->
  all_ids_for_export_term:('term -> Ids_for_export.t) ->
  ('bindable, 'term) t ->
  Ids_for_export.t
