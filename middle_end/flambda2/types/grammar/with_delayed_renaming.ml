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

[@@@ocaml.warning "+a-30-40-41-42"]

type free_names =
  | Not_computed
  | Not_computed_but_pending_renaming of Renaming.t
  | Computed of Name_occurrences.t

type 'descr t =
  { mutable descr : 'descr;
    mutable delayed_permutation : Renaming.t;
    mutable free_names : free_names
  }

let create descr =
  { descr; delayed_permutation = Renaming.empty; free_names = Not_computed }

let peek_descr t = t.descr

let[@inline always] descr ~apply_renaming_descr t =
  if Renaming.is_empty t.delayed_permutation
  then t.descr
  else
    let descr = apply_renaming_descr t.descr t.delayed_permutation in
    t.descr <- descr;
    let free_names =
      match t.free_names with
      | Not_computed -> Not_computed_but_pending_renaming t.delayed_permutation
      | Not_computed_but_pending_renaming renaming ->
        Not_computed_but_pending_renaming
          (Renaming.compose ~second:t.delayed_permutation ~first:renaming)
      | Computed free_names ->
        Computed
          (Name_occurrences.apply_renaming free_names t.delayed_permutation)
    in
    t.delayed_permutation <- Renaming.empty;
    t.free_names <- free_names;
    descr

let apply_renaming t perm =
  let delayed_permutation =
    Renaming.compose ~second:perm ~first:t.delayed_permutation
  in
  { t with delayed_permutation }

let[@inline always] free_names ~apply_renaming_descr ~free_names_descr t =
  let descr = descr ~apply_renaming_descr t in
  assert (Renaming.is_empty t.delayed_permutation);
  match t.free_names with
  | Not_computed ->
    let free_names = free_names_descr descr in
    t.free_names <- Computed free_names;
    free_names
  | Not_computed_but_pending_renaming renaming ->
    let free_names =
      Name_occurrences.apply_renaming (free_names_descr descr) renaming
    in
    t.free_names <- Computed free_names;
    free_names
  | Computed free_names -> free_names

let print ~print_descr ~apply_renaming_descr ppf t =
  print_descr ppf (descr ~apply_renaming_descr t)
