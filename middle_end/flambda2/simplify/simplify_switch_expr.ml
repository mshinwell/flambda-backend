(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let rebuild_arm uacc arm (action, use_id, arity)
    (new_let_conts, arms, identity_arms, not_arms, returning_one_name_arms) =
  let action =
    Simplify_common.clear_demoted_trap_action_and_patch_unused_exn_bucket uacc
      action
  in
  match
    EB.add_wrapper_for_switch_arm uacc action ~use_id
      (Flambda_arity.With_subkinds.of_arity arity)
  with
  | Apply_cont action -> (
    let action =
      (* First try to absorb any [Apply_cont] expression that forms the entirety
         of the arm's action (via an intermediate zero-arity continuation
         without trap action) into the [Switch] expression itself. *)
      if not (Apply_cont.is_goto action)
      then Some action
      else
        let cont = Apply_cont.continuation action in
        let check_handler ~handler ~action =
          match RE.to_apply_cont handler with
          | Some action -> Some action
          | None -> Some action
        in
        match UE.find_continuation (UA.uenv uacc) cont with
        | Linearly_used_and_inlinable
            { handler;
              free_names_of_handler = _;
              params;
              cost_metrics_of_handler = _
            } ->
          assert (List.length params = 0);
          check_handler ~handler ~action
        | Non_inlinable_zero_arity { handler = Known handler } ->
          check_handler ~handler ~action
        | Non_inlinable_zero_arity { handler = Unknown } -> Some action
        | Unreachable _ -> None
        | Non_inlinable_non_zero_arity _
        | Toplevel_or_function_return_or_exn_continuation _ ->
          Misc.fatal_errorf
            "Inconsistency for %a between [Apply_cont.is_goto] and \
             continuation environment in [UA]:@ %a"
            Continuation.print cont UA.print uacc
    in
    match action with
    | None ->
      (* The destination is unreachable; delete the [Switch] arm. *)
      new_let_conts, arms, identity_arms, not_arms, returning_one_name_arms
    | Some action -> (
      let normal_case ~identity_arms ~not_arms ~returning_one_name_arms =
        let arms = Targetint_31_63.Map.add arm action arms in
        new_let_conts, arms, identity_arms, not_arms, returning_one_name_arms
      in
      (* Now check to see if the arm is of a form that might mean the whole
         [Switch] is either the identity or a boolean NOT. *)
      match Apply_cont.to_one_arg_without_trap_action action with
      | None -> normal_case ~identity_arms ~not_arms ~returning_one_name_arms
      | Some arg ->
        (* CR-someday mshinwell: Maybe this check should be generalised
         * e.g. to detect
         *   | 0 -> apply_cont k x y 1
         *   | 1 -> apply_cont k x y 0
         *)
        let[@inline always] const arg =
          match Reg_width_const.descr arg with
          | Tagged_immediate arg ->
            if Targetint_31_63.equal arm arg
            then
              let identity_arms =
                Targetint_31_63.Map.add arm action identity_arms
              in
              normal_case ~identity_arms ~not_arms ~returning_one_name_arms
            else if Targetint_31_63.equal arm Targetint_31_63.bool_true
                    && Targetint_31_63.equal arg Targetint_31_63.bool_false
                    || Targetint_31_63.equal arm Targetint_31_63.bool_false
                       && Targetint_31_63.equal arg Targetint_31_63.bool_true
            then
              let not_arms = Targetint_31_63.Map.add arm action not_arms in
              normal_case ~identity_arms ~not_arms ~returning_one_name_arms
            else normal_case ~identity_arms ~not_arms ~returning_one_name_arms
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            normal_case ~identity_arms ~not_arms ~returning_one_name_arms
        in
        Simple.pattern_match arg ~const ~name:(fun name ~coercion ->
            if Coercion.is_id coercion
            then
              let returning_one_name_arms =
                Targetint_31_63.Map.add arm name returning_one_name_arms
              in
              normal_case ~identity_arms ~not_arms ~returning_one_name_arms
            else normal_case ~identity_arms ~not_arms ~returning_one_name_arms))
    )
  | New_wrapper
      (new_cont, new_handler, free_names_of_handler, cost_metrics_handler) ->
    let new_let_cont =
      new_cont, new_handler, free_names_of_handler, cost_metrics_handler
    in
    let new_let_conts = new_let_cont :: new_let_conts in
    let action = Apply_cont.goto new_cont in
    let arms = Targetint_31_63.Map.add arm action arms in
    new_let_conts, arms, identity_arms, not_arms, returning_one_name_arms

let rebuild_switch ~simplify_let dacc ~arms ~scrutinee ~scrutinee_ty uacc
    ~after_rebuild =
  let new_let_conts, arms, identity_arms, not_arms, returning_one_name_arms =
    Targetint_31_63.Map.fold (rebuild_arm uacc) arms
      ( [],
        Targetint_31_63.Map.empty,
        Targetint_31_63.Map.empty,
        Targetint_31_63.Map.empty,
        Targetint_31_63.Map.empty )
  in
  let switch_is_identity =
    let arm_discrs = Targetint_31_63.Map.keys arms in
    let identity_arms_discrs = Targetint_31_63.Map.keys identity_arms in
    if not (Targetint_31_63.Set.equal arm_discrs identity_arms_discrs)
    then None
    else
      Targetint_31_63.Map.data identity_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let switch_is_boolean_not =
    let arm_discrs = Targetint_31_63.Map.keys arms in
    let not_arms_discrs = Targetint_31_63.Map.keys not_arms in
    if (not (Targetint_31_63.Set.equal arm_discrs Targetint_31_63.all_bools))
       || not (Targetint_31_63.Set.equal arm_discrs not_arms_discrs)
    then None
    else
      Targetint_31_63.Map.data not_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let switch_is_logical_or =
    let arm_discrs = Targetint_31_63.Map.keys arms in
    if not (Targetint_31_63.Set.equal arm_discrs Targetint_31_63.all_bools)
    then None
    else
      match
        Targetint_31_63.Map.find Targetint_31_63.bool_false
          returning_one_name_arms
      with
      | exception Not_found -> None
      | name -> (
        match
          Targetint_31_63.Map.find Targetint_31_63.bool_true identity_arms
        with
        | exception Not_found -> None
        | _action -> (
          match
            Targetint_31_63.Map.data arms
            |> List.map Apply_cont.continuation
            |> Continuation.Set.of_list |> Continuation.Set.get_singleton
          with
          | None -> None
          | Some dest -> Some (name, dest)))
  in
  let create_tagged_scrutinee uacc dest ~make_body =
    (* A problem with using [simplify_let] below is that the continuation [dest]
       might have [Apply_cont_rewrite]s in the environment, left over from the
       simplification of the existing uses. We must clear these to avoid a
       lookup failure for our new [Apply_cont] when [Simplify_apply_cont] tries
       to rewrite the use. There is no need for the rewrites anyway; they have
       already been applied. It is of course necessary to restore any rewrite
       before returning [uacc], since the surrounding context may need it.

       Likewise, we need to clear the continuation uses environment for [dest]
       in [dacc], since our new [Apply_cont] might not match the original uses
       (e.g. if a parameter has been removed). *)
    let rewrite = UE.find_apply_cont_rewrite (UA.uenv uacc) dest in
    let uacc =
      UA.map_uenv uacc ~f:(fun uenv -> UE.delete_apply_cont_rewrite uenv dest)
    in
    let dacc = DA.delete_continuation_uses dacc dest in
    let bound_to = Variable.create "tagged_scrutinee" in
    let body = make_body ~tagged_scrutinee:(Simple.var bound_to) in
    let bound_to = Bound_var.create bound_to NM.normal in
    let defining_expr =
      Named.create_prim
        (Unary (Box_number (Untagged_immediate, Heap), scrutinee))
        Debuginfo.none
    in
    let let_expr =
      Let.create
        (Bound_pattern.singleton bound_to)
        defining_expr ~body ~free_names_of_body:Unknown
    in
    simplify_let dacc let_expr ~down_to_up:(fun _dacc ~rebuild ->
        rebuild uacc ~after_rebuild:(fun expr uacc ->
            let uacc =
              match rewrite with
              | None -> uacc
              | Some rewrite ->
                UA.map_uenv uacc ~f:(fun uenv ->
                    UE.add_apply_cont_rewrite uenv dest rewrite)
            in
            expr, uacc))
  in
  (* CR mshinwell: use this function in the one above *)
  let simplify_intermediate_let_expr uacc dest let_expr =
    let rewrite = UE.find_apply_cont_rewrite (UA.uenv uacc) dest in
    let uacc =
      UA.map_uenv uacc ~f:(fun uenv -> UE.delete_apply_cont_rewrite uenv dest)
    in
    let dacc = DA.delete_continuation_uses dacc dest in
    simplify_let dacc let_expr ~down_to_up:(fun _dacc ~rebuild ->
        rebuild uacc ~after_rebuild:(fun expr uacc ->
            let uacc =
              match rewrite with
              | None -> uacc
              | Some rewrite ->
                UA.map_uenv uacc ~f:(fun uenv ->
                    UE.add_apply_cont_rewrite uenv dest rewrite)
            in
            expr, uacc))
  in
  let body, uacc =
    if Targetint_31_63.Map.cardinal arms < 1
    then
      let uacc = UA.notify_removed ~operation:Removed_operations.branch uacc in
      RE.create_invalid Zero_switch_arms, uacc
    else
      let dbg = Debuginfo.none in
      match switch_is_identity with
      | Some dest ->
        let uacc =
          UA.notify_removed ~operation:Removed_operations.branch uacc
        in
        let make_body ~tagged_scrutinee =
          (* No need to increment the cost_metrics inside
             [create_tagged_scrutinee] as it will call simplify over the result
             of [make_body]. *)
          Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg
          |> Expr.create_apply_cont
        in
        create_tagged_scrutinee uacc dest ~make_body
      | None -> (
        match switch_is_boolean_not with
        | Some dest ->
          let uacc =
            UA.notify_removed ~operation:Removed_operations.branch uacc
          in
          let make_body ~tagged_scrutinee =
            let not_scrutinee = Variable.create "not_scrutinee" in
            let not_scrutinee' = Simple.var not_scrutinee in
            let do_lognot =
              Named.create_prim
                (P.Unary (Boolean_not, tagged_scrutinee))
                Debuginfo.none
            in
            let bound =
              VB.create not_scrutinee NM.normal |> Bound_pattern.singleton
            in
            let body =
              Apply_cont.create dest ~args:[not_scrutinee'] ~dbg
              |> Expr.create_apply_cont
            in
            Let.create bound do_lognot ~body ~free_names_of_body:Unknown
            |> Expr.create_let
          in
          create_tagged_scrutinee uacc dest ~make_body
        | None -> (
          match switch_is_logical_or with
          | Some (name, dest) ->
            let uacc =
              UA.notify_removed ~operation:Removed_operations.branch uacc
            in
            let let_expr =
              let untagged_name = Variable.create "untagged_name" in
              let untagged_name' = Simple.var untagged_name in
              let tagged_logor_result = Variable.create "tagged_logor_result" in
              let tagged_logor_result' = Simple.var tagged_logor_result in
              let logor_result = Variable.create "logor_result" in
              let logor_result' = Simple.var logor_result in
              let do_untagging =
                Named.create_prim
                  (P.Unary (Unbox_number Untagged_immediate, Simple.name name))
                  Debuginfo.none
              in
              let do_logor =
                Named.create_prim
                  (P.Binary
                     (Int_arith (Naked_immediate, Or), scrutinee, untagged_name'))
                  Debuginfo.none
              in
              let do_tagging =
                Named.create_prim
                  (P.Unary (Box_number (Untagged_immediate, Heap), logor_result'))
                  Debuginfo.none
              in
              let body =
                Apply_cont.create dest ~args:[tagged_logor_result'] ~dbg
                |> Expr.create_apply_cont
              in
              let body =
                Let.create
                  (VB.create tagged_logor_result NM.normal
                  |> Bound_pattern.singleton)
                  do_tagging ~body ~free_names_of_body:Unknown
                |> Expr.create_let
              in
              let body =
                Let.create
                  (VB.create logor_result NM.normal |> Bound_pattern.singleton)
                  do_logor ~body ~free_names_of_body:Unknown
                |> Expr.create_let
              in
              Let.create
                (VB.create untagged_name NM.normal |> Bound_pattern.singleton)
                do_untagging ~body ~free_names_of_body:Unknown
            in
            simplify_intermediate_let_expr uacc dest let_expr
          | None ->
            (* In that case, even though some branches were removed by simplify
               we should not count them in the number of removed operations:
               these branches wouldn't have been taken during execution
               anyway. *)
            let expr, uacc = EB.create_switch uacc ~scrutinee ~arms in
            if Flambda_features.check_invariants ()
               && Simple.is_const scrutinee
               && Targetint_31_63.Map.cardinal arms > 1
            then
              Misc.fatal_errorf
                "[Switch] with constant scrutinee (type: %a) should have been \
                 simplified away:@ %a"
                T.print scrutinee_ty
                (RE.print (UA.are_rebuilding_terms uacc))
                expr;
            expr, uacc))
  in
  let bind_let_cont (uacc, body)
      (new_cont, new_handler, free_names_of_handler, cost_metrics_of_handler) =
    let free_names_of_body = UA.name_occurrences uacc in
    let expr =
      RE.create_non_recursive_let_cont
        (UA.are_rebuilding_terms uacc)
        new_cont new_handler ~body ~free_names_of_body
    in
    let name_occurrences =
      Name_occurrences.remove_continuation
        (Name_occurrences.union free_names_of_body free_names_of_handler)
        new_cont
    in
    let uacc =
      UA.with_name_occurrences uacc ~name_occurrences
      |> UA.add_cost_metrics
           (Cost_metrics.increase_due_to_let_cont_non_recursive
              ~cost_metrics_of_handler)
    in
    uacc, expr
  in
  let uacc, expr =
    ListLabels.fold_left new_let_conts ~init:(uacc, body) ~f:bind_let_cont
  in
  after_rebuild expr uacc

let find_cse_simple dacc prim =
  match P.Eligible_for_cse.create prim with
  | None -> None (* Constant *)
  | Some with_fixed_value -> (
    match DE.find_cse (DA.denv dacc) with_fixed_value with
    | None -> None
    | Some simple -> (
      match
        TE.get_canonical_simple_exn (DA.typing_env dacc) simple
          ~min_name_mode:NM.normal ~name_mode_of_existing_simple:NM.normal
      with
      | exception Not_found -> None
      | simple -> Some simple))

let check_cse_environment dacc ~scrutinee ~arms =
  (* When the switch is an identity or a NOT, the expression is rewritten to
     remove the switch during the upwards pass. The switch is replaced by either
     a tagging or a boolean NOT and a tagging. The result of the tagging can be
     a variable for which dependencies are not tracked by data_flow the usual
     way during simplification of lets. This could be benign, if a new
     expression to compute it was always introduced here, because there is
     already a dependency registered on the scrutinee. But if CSE replaces it by
     a variable that is only used here, we can create a real new dependency that
     didn't exist before. For example: *)
  (*
   *   let untagged = untag x
   *   apply_cont k untagged (cse_arg tag(untagged) = x)
   *   where k x cse_param =
   *     switch x
   *     | 0 -> apply_cont k2 0
   *     | 1 -> apply_cont k2 1
   *
   *  would be rewritten to:
   *
   *   let untagged = untag x
   *   apply_cont k untagged (cse_arg tag(untagged) = x)
   *   where k x cse_param =
   *     let tagged = tag x
   *     apply_cont k2 tagged
   *
   * And with CSE:
   *
   *   let untagged = untag x
   *   apply_cont k untagged (cse_arg tag(untagged) = x)
   *   where k x cse_param =
   *     apply_cont k2 cse_param
   *)
  (* If the tracking were not done properly, cse_param could be considered dead
     and removed from the parameters of continuation k.

     We solve this by always looking for a tagged version of the scrutinee in
     the CSE environment and registering it as a required variable like the
     scrutinee. If it is not available, no problem can occur.

     A similar situation arises for simplification to logical OR and AND, where
     untagging of the free variables of the switch arm actions may be
     necessary. *)
  let dacc =
    match
      find_cse_simple dacc
        (Unary (Box_number (Untagged_immediate, Heap), scrutinee))
    with
    | None -> dacc
    | Some tagged_scrutinee -> (
      let dacc =
        DA.map_data_flow dacc
          ~f:
            (Data_flow.add_used_in_current_handler
               (Simple.free_names tagged_scrutinee))
      in
      match find_cse_simple dacc (Unary (Boolean_not, tagged_scrutinee)) with
      | None -> dacc
      | Some not_scrutinee ->
        DA.map_data_flow dacc
          ~f:
            (Data_flow.add_used_in_current_handler
               (Simple.free_names not_scrutinee)))
  in
  Targetint_31_63.Map.fold
    (fun _discr (action, _rewrite_id, _kind) dacc ->
      Name_occurrences.fold_names (Apply_cont.free_names action) ~init:dacc
        ~f:(fun dacc name ->
          let ty = TE.find (DA.typing_env dacc) name None in
          match
            T.prove_is_always_tagging_of_simple (DA.typing_env dacc)
              ~min_name_mode:NM.normal ty
          with
          | Unknown | Invalid -> dacc
          | Proved untagged_name ->
            DA.map_data_flow dacc
              ~f:
                (Data_flow.add_used_in_current_handler
                   (Simple.free_names untagged_name))))
    arms dacc

let simplify_arm ~typing_env_at_use ~scrutinee_ty arm action (arms, dacc) =
  let shape =
    let imm = Targetint_31_63.int (Targetint_31_63.to_targetint arm) in
    T.this_naked_immediate imm
  in
  match T.meet typing_env_at_use scrutinee_ty shape with
  | Bottom -> arms, dacc
  | Ok (_meet_ty, env_extension) -> (
    let env_at_use =
      TE.add_env_extension typing_env_at_use env_extension
      |> DE.with_typing_env (DA.denv dacc)
    in
    let args = AC.args action in
    let use_kind =
      Simplify_common.apply_cont_use_kind ~context:Switch_branch action
    in
    match args with
    | [] ->
      let dacc, rewrite_id =
        DA.record_continuation_use dacc (AC.continuation action) use_kind
          ~env_at_use ~arg_types:[]
      in
      let dacc =
        DA.map_data_flow dacc
          ~f:
            (Data_flow.add_apply_cont_args
               (Apply_cont.continuation action)
               (List.map Simple.free_names args))
      in
      let arms = Targetint_31_63.Map.add arm (action, rewrite_id, []) arms in
      arms, dacc
    | _ :: _ ->
      let { S.simples = args; simple_tys = arg_types } =
        S.simplify_simples dacc args
      in
      let dacc, rewrite_id =
        DA.record_continuation_use dacc (AC.continuation action) use_kind
          ~env_at_use ~arg_types
      in
      let arity = List.map T.kind arg_types in
      let action = Apply_cont.update_args action ~args in
      let dacc =
        DA.map_data_flow dacc
          ~f:
            (Data_flow.add_apply_cont_args
               (Apply_cont.continuation action)
               (List.map Simple.free_names args))
      in
      let arms = Targetint_31_63.Map.add arm (action, rewrite_id, arity) arms in
      arms, dacc)

let simplify_switch ~simplify_let dacc switch ~down_to_up =
  let scrutinee = Switch.scrutinee switch in
  let scrutinee_ty =
    S.simplify_simple dacc scrutinee ~min_name_mode:NM.normal
  in
  let scrutinee = T.get_alias_exn scrutinee_ty in
  let typing_env_at_use = DA.typing_env dacc in
  let arms, dacc =
    Targetint_31_63.Map.fold
      (simplify_arm ~typing_env_at_use ~scrutinee_ty)
      (Switch.arms switch)
      (Targetint_31_63.Map.empty, dacc)
  in
  let dacc = check_cse_environment dacc ~scrutinee ~arms in
  let dacc =
    if Targetint_31_63.Map.cardinal arms <= 1
    then dacc
    else
      DA.map_data_flow dacc
        ~f:(Data_flow.add_used_in_current_handler (Simple.free_names scrutinee))
  in
  down_to_up dacc
    ~rebuild:(rebuild_switch ~simplify_let dacc ~arms ~scrutinee ~scrutinee_ty)
