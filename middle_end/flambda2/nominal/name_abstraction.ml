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

[@@@ocaml.warning "+a-30-40-41-42"]

type ('bindable, 'term) t = 'bindable * 'term

let create bindable term = bindable, term

let[@inline always] pattern_match ~freshen_bindable ~swap_bindable
    ~apply_renaming_term (bindable, term) ~f =
  let fresh_bindable = freshen_bindable bindable in
  let renaming = swap_bindable bindable ~guaranteed_fresh:fresh_bindable in
  let fresh_term = apply_renaming_term term renaming in
  f fresh_bindable fresh_term

let[@inline always] pattern_match_pair ~freshen_bindable ~swap_bindable
    ~apply_renaming_term (bindable0, term0) (bindable1, term1) ~f =
  let fresh_bindable = freshen_bindable bindable0 in
  let renaming0 = swap_bindable bindable0 ~guaranteed_fresh:fresh_bindable in
  let renaming1 = swap_bindable bindable1 ~guaranteed_fresh:fresh_bindable in
  let fresh_term0 = apply_renaming_term term0 renaming0 in
  let fresh_term1 = apply_renaming_term term1 renaming1 in
  f fresh_bindable fresh_term0 fresh_term1

let print ~print_bindable ~print_term ~freshen_bindable ~swap_bindable
    ~apply_renaming_term ppf t =
  pattern_match ~freshen_bindable ~swap_bindable ~apply_renaming_term t
    ~f:(fun bindable term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>[%s%a%s@<1>%s]@ %a@]"
        (Flambda_colours.name_abstraction ())
        (Flambda_colours.normal ())
        print_bindable bindable
        (Flambda_colours.name_abstraction ())
        (Flambda_colours.normal ())
        print_term term)

let[@inline always] apply_renaming ~apply_renaming_bindable ~apply_renaming_term
    ((bindable, term) as t) renaming =
  if Renaming.is_empty renaming
  then t
  else
    let bindable = apply_renaming_bindable bindable renaming in
    let term = apply_renaming_term term renaming in
    bindable, term

let[@inline always] free_names ~free_names_bindable ~free_names_term
    (bindable, term) =
  Name_occurrences.diff (free_names_term term) (free_names_bindable bindable)

let[@inline always] all_ids_for_export ~all_ids_for_export_bindable
    ~all_ids_for_export_term (bindable, term) =
  Ids_for_export.union
    (all_ids_for_export_bindable bindable)
    (all_ids_for_export_term term)
