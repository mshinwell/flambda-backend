(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type effects_and_coeffects_classification =
  | Pure
  | Effect
  | Coeffect_only

let classify_by_effects_and_coeffects effs =
  (* See the comments on type [classification] in the .mli. *)
  match (effs : Effects_and_coeffects.t) with
  | Arbitrary_effects, (Has_coeffects | No_coeffects)
  | Only_generative_effects _, (Has_coeffects | No_coeffects) ->
    Effect
  | No_effects, Has_coeffects -> Coeffect_only
  | No_effects, No_coeffects -> Pure

type let_binding_classification =
  | Regular
  | Drop_defining_expr
  | May_inline

let classify_let_binding var
    ~(effects_and_coeffects_of_defining_expr : Effects_and_coeffects.t)
    ~(num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t) =
  match Variable.Map.find var num_normal_occurrences_of_bound_vars with
  | exception Not_found -> Regular
  | Zero -> begin
    match
      classify_by_effects_and_coeffects effects_and_coeffects_of_defining_expr
    with
    | Coeffect_only | Pure -> Drop_defining_expr
    | Effect ->
      Regular
      (* Could be May_inline technically, but it doesn't matter since it can
         only be flushed by the env. *)
  end
  | One ->
    (* Any defining expression used exactly once is considered for inlining at
       this stage. The environment is going to handle the details of preserving
       the effects and coeffects ordering (if inlining without reordering is
       impossible then the expressions will be bound at some safe place
       instead).

       Whether inlining of _effectful_ expressions _actually occurs_ depends on
       the context. Currently this is very restricted, see comments in
       [To_cmm_primitive]. *)
    May_inline
  | More_than_one -> Regular

type continuation_handler_classification =
  | Regular
  | May_inline

let cont_is_known_to_have_exactly_one_occurrence k (num : _ Or_unknown.t) =
  match num with
  | Unknown -> false
  | Known num -> (
    match (num : Num_occurrences.t) with
    | One -> true
    | More_than_one -> false
    | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k)

let classify_continuation_handler k handler ~num_free_occurrences
    ~is_applied_with_traps : continuation_handler_classification =
  if (not (Continuation_handler.is_exn_handler handler))
     && (not is_applied_with_traps)
     && cont_is_known_to_have_exactly_one_occurrence k num_free_occurrences
  then May_inline
  else Regular
