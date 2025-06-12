(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module type Term = sig
  type t

  val apply_renaming : t -> Renaming.t -> t

  include Contains_ids.S with type t := t
end

type ('bindable, 'term) t = 'bindable * 'term

let[@inline always] pattern_match (type bindable)
    (module Bindable : Bindable.S with type t = bindable) (bindable, term)
    ~apply_renaming_to_term ~f =
  let fresh_bindable = Bindable.rename bindable in
  let renaming = Bindable.renaming bindable ~guaranteed_fresh:fresh_bindable in
  let fresh_term = apply_renaming_to_term term renaming in
  f fresh_bindable fresh_term

let[@inline always] pattern_match_for_printing bindable_impl
    ((bindable, term) as t) ~apply_renaming_to_term ~f =
  if Flambda_features.freshen_when_printing ()
  then pattern_match bindable_impl t ~apply_renaming_to_term ~f
  else f bindable term

let[@inline always] pattern_match_pair (type bindable)
    (module Bindable : Bindable.S with type t = bindable) (bindable0, term0)
    (bindable1, term1) ~apply_renaming_to_term ~f =
  let fresh_bindable = Bindable.rename bindable0 in
  let renaming0 =
    Bindable.renaming bindable0 ~guaranteed_fresh:fresh_bindable
  in
  let renaming1 =
    Bindable.renaming bindable1 ~guaranteed_fresh:fresh_bindable
  in
  let fresh_term0 = apply_renaming_to_term term0 renaming0 in
  let fresh_term1 = apply_renaming_to_term term1 renaming1 in
  f fresh_bindable fresh_term0 fresh_term1

let apply_renaming (type bindable)
    (module Bindable : Bindable.S with type t = bindable)
    ((bindable, term) as t) renaming ~apply_renaming_to_term =
  if Renaming.is_identity renaming
  then t
  else
    let bindable' = Bindable.apply_renaming bindable renaming in
    let term' = apply_renaming_to_term term renaming in
    if bindable == bindable' && term == term' then t else bindable', term'

let free_names (type bindable)
    (module Bindable : Bindable.S with type t = bindable) (bindable, term)
    ~free_names_of_term =
  Name_occurrences.diff (free_names_of_term term)
    ~without:(Bindable.free_names bindable)

let ids_for_export (type bindable)
    (module Bindable : Bindable.S with type t = bindable) (bindable, term)
    ~ids_for_export_of_term =
  Ids_for_export.union
    (Bindable.ids_for_export bindable)
    (ids_for_export_of_term term)

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type nonrec t = (Bindable.t, Term.t) t

  let create bindable term = bindable, term

  let[@inline always] pattern_match (bindable, term) ~f =
    pattern_match
      (module Bindable)
      (bindable, term) ~f ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] pattern_match_for_printing (bindable, term) ~f =
    pattern_match_for_printing
      (module Bindable)
      (bindable, term) ~f ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] pattern_match_pair (bindable0, term0) (bindable1, term1)
      ~f =
    pattern_match_pair
      (module Bindable)
      (bindable0, term0) (bindable1, term1) ~f
      ~apply_renaming_to_term:Term.apply_renaming

  let apply_renaming t renaming =
    apply_renaming
      (module Bindable)
      t renaming ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] ( let<> ) t f =
    pattern_match t ~f:(fun bindable term -> f (bindable, term))

  let ids_for_export t =
    ids_for_export
      (module Bindable)
      t ~ids_for_export_of_term:Term.ids_for_export
end
[@@inline always]
