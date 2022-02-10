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

(* CR-someday mshinwell: Unused closure variables should be deleted prior to
   simplification of sets of closures, taking the used-var-in-closures set from
   the previous round. *)

(* CR-someday mshinwell: We could consider making the functions in this file
   tail recursive, although it probably isn't necessary, as excessive levels of
   nesting of functions seems unlikely. *)

let function_decl_type old_code_id ?new_code_id rec_info =
  let code_id = Option.value new_code_id ~default:old_code_id in
  Or_unknown_or_bottom.Ok (T.Function_type.create code_id ~rec_info)

module Context_for_multiple_sets_of_closures : sig
  (* This module deals with a sub-problem of the problem of simplifying multiple
     possibly-recursive sets of closures, namely determining typing and
     contextual information that is the same no matter which set of closures in
     a given recursive group is being simplified. *)

  type t

  val create :
    dacc_prior_to_sets:DA.t ->
    simplify_toplevel:Simplify_common.simplify_toplevel ->
    all_sets_of_closures:Set_of_closures.t list ->
    closure_bound_names_all_sets:Bound_name.t Closure_id.Map.t list ->
    closure_element_types_all_sets:T.t Var_within_closure.Map.t list ->
    t

  val dacc_inside_functions : t -> DA.t

  val dacc_prior_to_sets : t -> DA.t

  (* This map only contains entries for functions where we definitely have the
     code (not just the metadata). *)
  val old_to_new_code_ids_all_sets : t -> Code_id.t Code_id.Map.t

  val closure_bound_names_inside_functions_all_sets :
    t -> Bound_name.t Closure_id.Map.t list

  val closure_bound_names_inside_functions_exactly_one_set :
    t -> Bound_name.t Closure_id.Map.t

  val simplify_toplevel : t -> Simplify_common.simplify_toplevel

  val previously_free_depth_variables : t -> Variable.Set.t
end = struct
  type t =
    { dacc_prior_to_sets : DA.t;
      simplify_toplevel : Simplify_common.simplify_toplevel;
      dacc_inside_functions : DA.t;
      closure_bound_names_inside_functions_all_sets :
        Bound_name.t Closure_id.Map.t list;
      old_to_new_code_ids_all_sets : Code_id.t Code_id.Map.t;
      previously_free_depth_variables : Variable.Set.t
    }

  let simplify_toplevel t = t.simplify_toplevel

  let dacc_prior_to_sets t = t.dacc_prior_to_sets

  let dacc_inside_functions t = t.dacc_inside_functions

  let old_to_new_code_ids_all_sets t = t.old_to_new_code_ids_all_sets

  let closure_bound_names_inside_functions_all_sets t =
    t.closure_bound_names_inside_functions_all_sets

  let closure_bound_names_inside_functions_exactly_one_set t =
    match t.closure_bound_names_inside_functions_all_sets with
    | [closure_bound_names_inside] -> closure_bound_names_inside
    | [] | _ :: _ :: _ ->
      Misc.fatal_error "Only one set of closures was expected"

  let previously_free_depth_variables t = t.previously_free_depth_variables

  let compute_closure_element_types_inside_function ~env_prior_to_sets
      ~env_inside_function ~closure_element_types ~degraded_closure_vars =
    Var_within_closure.Map.fold
      (fun clos_var type_prior_to_sets
           (env_inside_function, types_inside_function) ->
        let var = Variable.create "clos_var" in
        let env_inside_function =
          let var = Bound_var.create var NM.in_types in
          TE.add_definition env_inside_function (Bound_name.var var) K.value
        in
        let type_prior_to_sets =
          (* See comment below about [degraded_closure_vars]. *)
          if Var_within_closure.Set.mem clos_var degraded_closure_vars
          then T.any_value
          else type_prior_to_sets
        in
        let env_extension =
          T.make_suitable_for_environment env_prior_to_sets
            (Everything_not_in env_inside_function)
            [Name.var var, type_prior_to_sets]
        in
        let env_inside_function =
          TE.add_env_extension_with_extra_variables env_inside_function
            env_extension
        in
        let types_inside_function =
          Var_within_closure.Map.add clos_var
            (T.alias_type_of K.value (Simple.var var))
            types_inside_function
        in
        env_inside_function, types_inside_function)
      closure_element_types
      (env_inside_function, Var_within_closure.Map.empty)

  let compute_closure_types_inside_functions ~denv ~all_sets_of_closures
      ~closure_bound_names_all_sets
      ~closure_element_types_inside_functions_all_sets
      ~old_to_new_code_ids_all_sets =
    let closure_bound_names_all_sets_inside =
      (* When not lifting (i.e. the bound names are variables), we used to
         create a fresh set of irrelevant variables, since the let-bound names
         are not in scope for the closure definition(s). However instead we now
         actually use the same names, which will be bound at [In_types] mode,
         and suitably captured by a name abstraction if they escape via function
         result types. *)
      closure_bound_names_all_sets
    in
    let closure_types_via_aliases_all_sets =
      List.map
        (fun closure_bound_names_inside ->
          Closure_id.Map.map
            (fun name ->
              T.alias_type_of K.value (Simple.name (Bound_name.to_name name)))
            closure_bound_names_inside)
        closure_bound_names_all_sets_inside
    in
    let closure_types_inside_functions =
      List.map2
        (fun set_of_closures
             (closure_types_via_aliases, closure_element_types_inside_function) ->
          let function_decls = Set_of_closures.function_decls set_of_closures in
          let all_function_decls_in_set =
            Closure_id.Map.map
              (fun old_code_id ->
                let code_or_metadata = DE.find_code_exn denv old_code_id in
                let new_code_id =
                  match code_or_metadata with
                  | Code_present _ ->
                    Code_id.Map.find old_code_id old_to_new_code_ids_all_sets
                  | Metadata_only _ -> old_code_id
                in
                let rec_info =
                  (* From inside their own bodies, every function in the set
                     currently being defined has an unknown recursion depth *)
                  T.unknown K.rec_info
                in
                let code_metadata =
                  code_or_metadata |> Code_or_metadata.code_metadata
                in
                let dbg = Code_metadata.dbg code_metadata in
                let dbg_including_inlining_stack =
                  DE.add_inlined_debuginfo denv dbg
                in
                Inlining_report.record_decision
                  (At_function_declaration
                     { code_id = Code_id.export old_code_id;
                       pass = Before_simplify { dbg_including_inlining_stack };
                       decision = Code_metadata.inlining_decision code_metadata
                     })
                  ~dbg;
                function_decl_type old_code_id ~new_code_id rec_info)
              (Function_declarations.funs function_decls)
          in
          Closure_id.Map.mapi
            (fun closure_id _function_decl ->
              T.exactly_this_closure closure_id ~all_function_decls_in_set
                ~all_closures_in_set:closure_types_via_aliases
                ~all_closure_vars_in_set:closure_element_types_inside_function
                (Known (Set_of_closures.alloc_mode set_of_closures)))
            all_function_decls_in_set)
        all_sets_of_closures
        (List.combine closure_types_via_aliases_all_sets
           closure_element_types_inside_functions_all_sets)
    in
    closure_bound_names_all_sets_inside, closure_types_inside_functions

  let bind_closure_types_inside_functions denv_inside_functions
      ~closure_bound_names_inside_functions_all_sets
      ~closure_types_inside_functions_all_sets =
    let denv_inside_functions =
      List.fold_left
        (fun denv closure_bound_names_inside ->
          Closure_id.Map.fold
            (fun _closure_id bound_name denv ->
              let name = Bound_name.name bound_name in
              let irrelevant = not (Bound_name.is_symbol bound_name) in
              let bound_name =
                Bound_name.create name
                  (if irrelevant then NM.in_types else NM.normal)
              in
              DE.define_name denv bound_name K.value)
            closure_bound_names_inside denv)
        denv_inside_functions closure_bound_names_inside_functions_all_sets
    in
    List.fold_left2
      (fun denv closure_bound_names_inside_functions_one_set
           closure_types_inside_functions_one_set ->
        Closure_id.Map.fold
          (fun closure_id closure_type denv ->
            match
              Closure_id.Map.find closure_id
                closure_bound_names_inside_functions_one_set
            with
            | exception Not_found ->
              Misc.fatal_errorf
                "No closure name for closure ID %a.@ \
                 closure_bound_names_inside_functions_one_set = %a."
                Closure_id.print closure_id
                (Closure_id.Map.print Bound_name.print)
                closure_bound_names_inside_functions_one_set
            | bound_name ->
              DE.add_equation_on_name denv
                (Bound_name.name bound_name)
                closure_type)
          closure_types_inside_functions_one_set denv)
      denv_inside_functions closure_bound_names_inside_functions_all_sets
      closure_types_inside_functions_all_sets

  let compute_old_to_new_code_ids_all_sets denv ~all_sets_of_closures =
    List.fold_left
      (fun old_to_new_code_ids_all_sets set_of_closures ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        Closure_id.Map.fold
          (fun _ old_code_id old_to_new_code_ids ->
            match DE.find_code_exn denv old_code_id with
            | Code_present _ ->
              let new_code_id = Code_id.rename old_code_id in
              Code_id.Map.add old_code_id new_code_id old_to_new_code_ids
            | Metadata_only _ -> old_to_new_code_ids)
          (Function_declarations.funs function_decls)
          old_to_new_code_ids_all_sets)
      Code_id.Map.empty all_sets_of_closures

  let bind_existing_code_to_new_code_ids denv ~old_to_new_code_ids_all_sets =
    Code_id.Map.fold
      (fun old_code_id new_code_id denv ->
        match DE.find_code_exn denv old_code_id with
        | Code_present code ->
          let code =
            code
            |> Code.with_newer_version_of (Some old_code_id)
            |> Code.with_code_id new_code_id
          in
          DE.define_code denv ~code_id:new_code_id ~code
        | Metadata_only _ -> denv)
      old_to_new_code_ids_all_sets denv

  let create ~dacc_prior_to_sets ~simplify_toplevel ~all_sets_of_closures
      ~closure_bound_names_all_sets ~closure_element_types_all_sets =
    let denv = DA.denv dacc_prior_to_sets in
    let denv_inside_functions =
      denv |> DE.enter_set_of_closures |> DE.increment_continuation_scope_twice
      (* Even if we are not rebuilding terms we should always rebuild them for
         local functions. The type of a function is dependent on its term and
         not knowing it prohibits us from inlining it. *)
      |> DE.set_rebuild_terms
    in
    (* We collect a set of "degraded closure variables" whose types involve
       imported variables from missing .cmx files. Since we don't know the kind
       of these variables, we can't run the code below that checks if they might
       need binding as "never inline" depth variables. Instead we will treat the
       whole closure variable as having [Unknown] type. *)
    let degraded_closure_vars = ref Var_within_closure.Set.empty in
    let free_depth_variables =
      List.concat_map
        (fun closure_element_types ->
          Var_within_closure.Map.mapi
            (fun closure_var ty ->
              let vars = TE.free_names_transitive (DE.typing_env denv) ty in
              Name_occurrences.fold_variables vars ~init:Variable.Set.empty
                ~f:(fun free_depth_variables var ->
                  let ty_opt =
                    TE.find_or_missing
                      (DE.typing_env denv_inside_functions)
                      (Name.var var)
                  in
                  match ty_opt with
                  | None ->
                    degraded_closure_vars
                      := Var_within_closure.Set.add closure_var
                           !degraded_closure_vars;
                    free_depth_variables
                  | Some ty -> (
                    match T.kind ty with
                    | Rec_info -> Variable.Set.add var free_depth_variables
                    | Value | Naked_number _ | Region -> free_depth_variables)))
            closure_element_types
          |> Var_within_closure.Map.data)
        closure_element_types_all_sets
      |> Variable.Set.union_list
    in
    (* Pretend that any depth variables appearing free in the closure elements
       are bound to "never inline anything" in the function. This causes them to
       be skipped over by [make_suitable_for_environment], thus avoiding dealing
       with in-types depth variables ending up in terms. *)
    (* CR-someday lmaurer: It would be better to propagate depth variables into
       closures properly, as this would allow things like unrolling [Seq.map]
       where the recursive call goes through a closure. For the moment, we often
       just stop unrolling cold in that situation. (It's important that we use
       [Rec_info_expr.do_not_inline] here so that we don't start unrolling,
       since without propagating the rec info into the closure, we don't know
       when to stop unrolling.)

       mshinwell: Leo and I have discussed allowing In_types variables in
       closures, which should cover this case, if we allowed such variables to
       be of kinds other than [Value]. *)
    let denv_inside_functions =
      Variable.Set.fold
        (fun dv env_inside_functions ->
          let name = Name.var dv in
          DE.add_equation_on_name env_inside_functions name
            (T.this_rec_info Rec_info_expr.do_not_inline))
        free_depth_variables denv_inside_functions
    in
    let ( env_inside_functions,
          closure_element_types_all_sets_inside_functions_rev ) =
      List.fold_left
        (fun ( env_inside_functions,
               closure_element_types_all_sets_inside_functions_rev )
             closure_element_types ->
          let env_inside_functions, closure_element_types_inside_function =
            compute_closure_element_types_inside_function
              ~env_prior_to_sets:(DE.typing_env denv)
              ~env_inside_function:env_inside_functions ~closure_element_types
              ~degraded_closure_vars:!degraded_closure_vars
          in
          ( env_inside_functions,
            closure_element_types_inside_function
            :: closure_element_types_all_sets_inside_functions_rev ))
        (DE.typing_env denv_inside_functions, [])
        closure_element_types_all_sets
    in
    let closure_element_types_inside_functions_all_sets =
      List.rev closure_element_types_all_sets_inside_functions_rev
    in
    let old_to_new_code_ids_all_sets =
      compute_old_to_new_code_ids_all_sets denv ~all_sets_of_closures
    in
    let ( closure_bound_names_inside_functions_all_sets,
          closure_types_inside_functions_all_sets ) =
      compute_closure_types_inside_functions ~denv ~all_sets_of_closures
        ~closure_bound_names_all_sets
        ~closure_element_types_inside_functions_all_sets
        ~old_to_new_code_ids_all_sets
    in
    let dacc_inside_functions =
      env_inside_functions
      |> DE.with_typing_env denv_inside_functions
      |> bind_existing_code_to_new_code_ids ~old_to_new_code_ids_all_sets
      |> bind_closure_types_inside_functions
           ~closure_bound_names_inside_functions_all_sets
           ~closure_types_inside_functions_all_sets
      |> DA.with_denv dacc_prior_to_sets
    in
    { dacc_prior_to_sets;
      dacc_inside_functions;
      closure_bound_names_inside_functions_all_sets;
      old_to_new_code_ids_all_sets;
      simplify_toplevel;
      previously_free_depth_variables = free_depth_variables
    }
end

module C = Context_for_multiple_sets_of_closures

let dacc_inside_function context ~used_closure_vars ~shareable_constants
    ~closure_offsets ~params ~my_closure ~my_depth closure_id
    ~closure_bound_names_inside_function ~inlining_arguments =
  let dacc =
    DA.map_denv (C.dacc_inside_functions context) ~f:(fun denv ->
        let denv = DE.add_parameters_with_unknown_types denv params in
        let denv = DE.set_inlining_arguments inlining_arguments denv in
        let denv =
          match
            Closure_id.Map.find closure_id closure_bound_names_inside_function
          with
          | exception Not_found ->
            Misc.fatal_errorf
              "No closure name for closure ID %a.@ \
               closure_bound_names_inside_function = %a."
              Closure_id.print closure_id
              (Closure_id.Map.print Bound_name.print)
              closure_bound_names_inside_function
          | name ->
            let name = Bound_name.name name in
            DE.add_variable denv
              (Bound_var.create my_closure NM.normal)
              (T.alias_type_of K.value (Simple.name name))
        in
        let denv =
          let my_depth = Bound_var.create my_depth Name_mode.normal in
          DE.add_variable denv my_depth (T.unknown K.rec_info)
        in
        denv)
  in
  dacc
  |> DA.with_shareable_constants ~shareable_constants
  |> DA.with_used_closure_vars ~used_closure_vars
  |> DA.with_closure_offsets ~closure_offsets

type simplify_function_result =
  { new_code_id : Code_id.t;
    code : Rebuilt_static_const.t option;
    dacc_after_body : DA.t option;
    uacc_after_upwards_traversal : UA.t option;
    lifted_consts_this_function : LCS.t
  }

let simplify_function0 context ~used_closure_vars ~shareable_constants
    ~closure_offsets closure_id code_id code
    ~closure_bound_names_inside_function code_age_relation
    ~lifted_consts_prev_functions =
  let inlining_arguments_from_denv =
    C.dacc_prior_to_sets context |> DA.denv |> DE.inlining_arguments
  in
  (* Compute the set of inlining_arguments used to define this function by
     taking the "least powerful" set between the one set in the environment and
     the one used to define the symbol previously. This way, functions that were
     imported from a foreign compilation unit after inlining will still be
     considered with a set of inlining arguments coherent with the one used to
     compile the current file when inlining. *)
  let inlining_arguments =
    Inlining_arguments.meet
      (Code.inlining_arguments code)
      inlining_arguments_from_denv
  in
  let result_arity = Code.result_arity code in
  let return_cont_params =
    List.mapi
      (fun i kind_with_subkind ->
        BP.create
          (Variable.create ("result" ^ string_of_int i))
          kind_with_subkind)
      result_arity
  in
  let ( params,
        params_and_body,
        dacc_after_body,
        free_names_of_code,
        return_cont_uses,
        uacc_after_upwards_traversal ) =
    Function_params_and_body.pattern_match (Code.params_and_body code)
      ~f:(fun
           ~return_continuation
           ~exn_continuation
           params
           ~body
           ~my_closure
           ~is_my_closure_used:_
           ~my_depth
           ~free_names_of_body:_
         ->
        let dacc =
          dacc_inside_function context ~used_closure_vars ~shareable_constants
            ~closure_offsets ~params ~my_closure ~my_depth closure_id
            ~closure_bound_names_inside_function ~inlining_arguments
        in
        if not (DA.no_lifted_constants dacc)
        then
          Misc.fatal_errorf "Did not expect lifted constants in [dacc]:@ %a"
            DA.print dacc;
        let dacc =
          DA.map_denv dacc ~f:(fun denv ->
              denv
              |> DE.enter_closure code_id ~return_continuation ~exn_continuation
              |> DE.map_typing_env ~f:(fun typing_env ->
                     let code_age_relation =
                       T.Code_age_relation.union
                         (TE.code_age_relation typing_env)
                         code_age_relation
                     in
                     TE.with_code_age_relation typing_env code_age_relation)
              |> fun denv ->
              DE.add_parameters_with_unknown_types denv return_cont_params
              |> fun denv ->
              (* Lifted constants from previous functions in the set get put
                 into the environment for subsequent functions. *)
              LCS.add_to_denv denv lifted_consts_prev_functions)
        in
        assert (not (DE.at_unit_toplevel (DA.denv dacc)));
        match
          C.simplify_toplevel context dacc body ~return_continuation
            ~exn_continuation ~return_arity:(Code.result_arity code)
            ~return_cont_scope:Scope.initial
            ~exn_cont_scope:(Scope.next Scope.initial)
        with
        | body, uacc ->
          let dacc_after_body = UA.creation_dacc uacc in
          let return_cont_uses =
            CUE.get_continuation_uses
              (DA.continuation_uses_env dacc_after_body)
              return_continuation
          in
          let free_names_of_body = UA.name_occurrences uacc in
          let params_and_body =
            RE.Function_params_and_body.create ~free_names_of_body
              ~return_continuation ~exn_continuation params ~body ~my_closure
              ~my_depth
          in
          (* Free names of the code = free names of the body minus the return
             and exception continuations, the parameters and the [my_closure]
             variable. *)
          let free_names_of_code =
            Name_occurrences.remove_continuation free_names_of_body
              return_continuation
          in
          let free_names_of_code =
            Name_occurrences.remove_continuation free_names_of_code
              exn_continuation
          in
          let free_names_of_code =
            Name_occurrences.remove_var free_names_of_code my_closure
          in
          let free_names_of_code =
            Name_occurrences.remove_var free_names_of_code my_depth
          in
          let free_names_of_code =
            Name_occurrences.diff free_names_of_code (BP.List.free_names params)
          in
          let free_names_of_code =
            Name_occurrences.diff free_names_of_code
              (Name_occurrences.create_variables
                 (C.previously_free_depth_variables context)
                 NM.normal)
          in
          if not
               (Name_occurrences.no_variables free_names_of_code
               && Name_occurrences.no_continuations free_names_of_code)
          then
            Misc.fatal_errorf
              "Unexpected free name(s):@ %a@ in:@ \n\
               %a@ \n\
               Simplified version:@ fun %a %a %a ->@ \n\
              \  %a" Name_occurrences.print free_names_of_code Code_id.print
              code_id BP.List.print params Variable.print my_closure
              Variable.print my_depth
              (RE.print (UA.are_rebuilding_terms uacc))
              body;
          ( params,
            params_and_body,
            dacc_after_body,
            free_names_of_code,
            return_cont_uses,
            uacc )
        | exception Misc.Fatal_error ->
          let bt = Printexc.get_raw_backtrace () in
          Format.eprintf
            "\n\
             %sContext is:%s simplifying function with closure ID %a,@ params \
             %a,@ return continuation %a,@ exn continuation %a,@ my_closure \
             %a,@ body:@ %a@ with downwards accumulator:@ %a\n"
            (Flambda_colours.error ())
            (Flambda_colours.normal ())
            Closure_id.print closure_id Bound_parameter.List.print params
            Continuation.print return_continuation Continuation.print
            exn_continuation Variable.print my_closure Expr.print body DA.print
            dacc;
          Printexc.raise_with_backtrace Misc.Fatal_error bt)
  in
  let cost_metrics = UA.cost_metrics uacc_after_upwards_traversal in
  let old_code_id = code_id in
  let new_code_id =
    Code_id.Map.find old_code_id (C.old_to_new_code_ids_all_sets context)
  in
  let inlining_decision =
    let decision =
      Function_decl_inlining_decision.make_decision ~inlining_arguments
        ~inline:(Code.inline code) ~stub:(Code.stub code) ~cost_metrics
        ~is_a_functor:(Code.is_a_functor code) ~recursive:(Code.recursive code)
    in
    let dbg =
      DE.add_inlined_debuginfo (DA.denv dacc_after_body) (Code.dbg code)
    in
    Inlining_report.record_decision
      (At_function_declaration
         { code_id = Code_id.export code_id; pass = After_simplify; decision })
      ~dbg;
    decision
  in
  let lifted_consts_this_function =
    (* Subtle point: [uacc_after_upwards_traversal] must be used to retrieve all
       of the lifted constants generated during the simplification of the
       function, not [dacc_after_body]. The reason for this is that sometimes
       the constants in [DA] are cleared (but remembered) and reinstated
       afterwards, for example at a [Let_cont]. It follows that if the turning
       point where the downwards traversal turns into an upwards traversal is in
       such a context, not all of the constants may currently be present in
       [DA]. *)
    UA.lifted_constants uacc_after_upwards_traversal
  in
  let is_a_functor = Code.is_a_functor code in
  let result_types =
    if not (Flambda_features.function_result_types ~is_a_functor)
    then Result_types.create_unknown ~params ~result_arity
    else
      match return_cont_uses with
      | None -> Result_types.create_bottom ~params ~result_arity
      | Some uses -> (
        (* CR mshinwell: Should we meet the result types with the arity? Also at
           applications? *)
        let code_age_relation_after_function =
          TE.code_age_relation (DA.typing_env dacc_after_body)
        in
        let env_at_fork_plus_params =
          (* We use [C.dacc_inside_functions] not [C.dacc_prior_to_sets] to
             ensure that the environment contains bindings for any symbols being
             defined by the set of closures. *)
          DE.add_parameters_with_unknown_types
            (DA.denv (C.dacc_inside_functions context))
            return_cont_params
        in
        let join =
          let consts_lifted_during_body =
            LCS.union lifted_consts_prev_functions lifted_consts_this_function
          in
          Join_points.compute_handler_env
            ~unknown_if_defined_at_or_later_than:
              (DE.get_continuation_scope env_at_fork_plus_params)
            uses ~params:return_cont_params ~env_at_fork_plus_params
            ~consts_lifted_during_body
            ~code_age_relation_after_body:code_age_relation_after_function
        in
        match join with
        | No_uses -> assert false (* should have been caught above *)
        | Uses { handler_env; _ } ->
          let params_and_results =
            BP.List.var_set (params @ return_cont_params)
          in
          let typing_env = DE.typing_env handler_env in
          let results_and_types =
            List.map
              (fun result ->
                let ty =
                  TE.find typing_env (BP.name result)
                    (Some (K.With_subkind.kind (BP.kind result)))
                in
                BP.name result, ty)
              return_cont_params
          in
          let env_extension =
            T.make_suitable_for_environment typing_env
              (All_variables_except params_and_results) results_and_types
          in
          Result_types.create ~params ~results:return_cont_params env_extension)
  in
  let code =
    Rebuilt_static_const.create_code
      (DA.are_rebuilding_terms dacc_after_body)
      new_code_id ~params_and_body
      ~free_names_of_params_and_body:free_names_of_code
      ~newer_version_of:(Some old_code_id)
      ~params_arity:(Code.params_arity code)
      ~num_trailing_local_params:(Code.num_trailing_local_params code)
      ~result_arity ~result_types
      ~contains_no_escaping_local_allocs:
        (Code.contains_no_escaping_local_allocs code)
      ~stub:(Code.stub code) ~inline:(Code.inline code)
      ~is_a_functor:(Code.is_a_functor code) ~recursive:(Code.recursive code)
      ~cost_metrics ~inlining_arguments ~dbg:(Code.dbg code)
      ~is_tupled:(Code.is_tupled code)
      ~is_my_closure_used:(Code.is_my_closure_used code)
      ~inlining_decision
  in
  { new_code_id;
    code = Some code;
    dacc_after_body = Some dacc_after_body;
    uacc_after_upwards_traversal = Some uacc_after_upwards_traversal;
    lifted_consts_this_function
  }

let simplify_function context ~used_closure_vars ~shareable_constants
    ~closure_offsets closure_id code_id ~closure_bound_names_inside_function
    code_age_relation ~lifted_consts_prev_functions =
  match DE.find_code_exn (DA.denv (C.dacc_prior_to_sets context)) code_id with
  | Code_present code ->
    simplify_function0 context ~used_closure_vars ~shareable_constants
      ~closure_offsets closure_id code_id code
      ~closure_bound_names_inside_function code_age_relation
      ~lifted_consts_prev_functions
  | Metadata_only _ ->
    (* No new code ID is created in this case: there is no function body to be
       simplified and all other code metadata will remain the same. *)
    { new_code_id = code_id;
      code = None;
      dacc_after_body = None;
      uacc_after_upwards_traversal = None;
      lifted_consts_this_function = LCS.empty
    }

type simplify_set_of_closures0_result =
  { set_of_closures : Flambda.Set_of_closures.t;
    code : Rebuilt_static_const.t Code_id.Lmap.t;
    dacc : Downwards_acc.t
  }

let simplify_set_of_closures0 context set_of_closures ~closure_bound_names
    ~closure_bound_names_inside ~closure_elements ~closure_element_types =
  let dacc = C.dacc_prior_to_sets context in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let all_function_decls_in_set =
    Function_declarations.funs_in_order function_decls
  in
  if not (DA.no_lifted_constants dacc)
  then
    Misc.fatal_errorf "Did not expect lifted constants in [dacc]:@ %a" DA.print
      dacc;
  let ( all_function_decls_in_set,
        code,
        fun_types,
        code_age_relation,
        used_closure_vars,
        shareable_constants,
        closure_offsets,
        lifted_consts,
        code_ids_to_remember ) =
    Closure_id.Lmap.fold
      (fun closure_id old_code_id
           ( result_function_decls_in_set,
             code,
             fun_types,
             code_age_relation,
             used_closure_vars,
             shareable_constants,
             closure_offsets,
             lifted_consts_prev_functions,
             code_ids_to_remember ) ->
        let { new_code_id;
              code = new_code;
              dacc_after_body;
              uacc_after_upwards_traversal;
              lifted_consts_this_function
            } =
          simplify_function context ~used_closure_vars ~shareable_constants
            ~closure_offsets closure_id old_code_id
            ~closure_bound_names_inside_function:closure_bound_names_inside
            code_age_relation ~lifted_consts_prev_functions
        in
        let function_type =
          let rec_info =
            (* This is the intrinsic type of the function as seen outside its
               own scope, so its [Rec_info] needs to say its depth is zero *)
            T.this_rec_info Rec_info_expr.initial
          in
          function_decl_type new_code_id rec_info
        in
        let result_function_decls_in_set =
          (closure_id, new_code_id) :: result_function_decls_in_set
        in
        let code =
          match new_code with
          | None -> code
          | Some new_code -> (new_code_id, new_code) :: code
        in
        let fun_types = Closure_id.Map.add closure_id function_type fun_types in
        let lifted_consts_prev_functions =
          LCS.union lifted_consts_this_function lifted_consts_prev_functions
        in
        let code_age_relation =
          match dacc_after_body with
          | None -> code_age_relation
          | Some dacc_after_body ->
            TE.code_age_relation (DA.typing_env dacc_after_body)
        in
        let code_ids_to_remember =
          match dacc_after_body with
          | None -> code_ids_to_remember
          | Some dacc_after_body ->
            Code_id.Set.union code_ids_to_remember
              (DA.code_ids_to_remember dacc_after_body)
        in
        let used_closure_vars =
          match uacc_after_upwards_traversal with
          | None -> used_closure_vars
          | Some uacc_after_upwards_traversal ->
            UA.used_closure_vars uacc_after_upwards_traversal
        in
        let shareable_constants =
          match uacc_after_upwards_traversal with
          | None -> shareable_constants
          | Some uacc_after_upwards_traversal ->
            UA.shareable_constants uacc_after_upwards_traversal
        in
        let closure_offsets =
          match uacc_after_upwards_traversal with
          | None -> closure_offsets
          | Some uacc_after_upwards_traversal ->
            UA.closure_offsets uacc_after_upwards_traversal
        in
        ( result_function_decls_in_set,
          code,
          fun_types,
          code_age_relation,
          used_closure_vars,
          shareable_constants,
          closure_offsets,
          lifted_consts_prev_functions,
          code_ids_to_remember ))
      all_function_decls_in_set
      ( [],
        [],
        Closure_id.Map.empty,
        TE.code_age_relation (DA.typing_env dacc),
        DA.used_closure_vars dacc,
        DA.shareable_constants dacc,
        DA.closure_offsets dacc,
        LCS.empty,
        DA.code_ids_to_remember dacc )
  in
  let dacc =
    DA.add_to_lifted_constant_accumulator dacc lifted_consts
    |> DA.with_used_closure_vars ~used_closure_vars
    |> DA.with_shareable_constants ~shareable_constants
    |> DA.with_closure_offsets ~closure_offsets
    |> DA.with_code_ids_to_remember ~code_ids_to_remember
  in
  let code_ids_to_remember_this_set =
    List.fold_left
      (fun code_ids (_closure_id, code_id) -> Code_id.Set.add code_id code_ids)
      Code_id.Set.empty all_function_decls_in_set
  in
  let dacc = DA.add_code_ids_to_remember dacc code_ids_to_remember_this_set in
  let all_function_decls_in_set =
    Closure_id.Lmap.of_list (List.rev all_function_decls_in_set)
  in
  let code = Code_id.Lmap.of_list (List.rev code) in
  let closure_types_by_bound_name =
    let closure_types_via_aliases =
      Closure_id.Map.map
        (fun name ->
          T.alias_type_of K.value (Simple.name (Bound_name.name name)))
        closure_bound_names
    in
    Closure_id.Map.fold
      (fun closure_id _function_decl_type closure_types ->
        match Closure_id.Map.find closure_id closure_bound_names with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for closure ID %a"
            Closure_id.print closure_id
        | bound_name ->
          let closure_type =
            T.exactly_this_closure closure_id
              ~all_function_decls_in_set:fun_types
              ~all_closures_in_set:closure_types_via_aliases
              ~all_closure_vars_in_set:closure_element_types
              (Known (Set_of_closures.alloc_mode set_of_closures))
          in
          Bound_name.Map.add bound_name closure_type closure_types)
      fun_types Bound_name.Map.empty
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
        denv
        |> DE.map_typing_env ~f:(fun typing_env ->
               TE.with_code_age_relation typing_env code_age_relation)
        |> Closure_id.Map.fold
             (fun _closure_id bound_name denv ->
               DE.define_name_if_undefined denv bound_name K.value)
             closure_bound_names
        |> fun denv ->
        LCS.add_to_denv denv lifted_consts
        |> Bound_name.Map.fold
             (fun bound_name closure_type denv ->
               let bound_name = Bound_name.to_name bound_name in
               DE.add_equation_on_name denv bound_name closure_type)
             closure_types_by_bound_name)
  in
  let set_of_closures =
    Function_declarations.create all_function_decls_in_set
    |> Set_of_closures.create ~closure_elements
         (Set_of_closures.alloc_mode set_of_closures)
  in
  { set_of_closures; code; dacc }

let introduce_code dacc code =
  Code_id.Lmap.bindings code
  |> List.map (fun (code_id, code) -> LC.create_code code_id code)
  |> LCS.singleton_list_of_constants_order_does_not_matter
  |> DA.add_to_lifted_constant_accumulator ~also_add_to_env:() dacc

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
    ~closure_bound_vars set_of_closures ~closure_elements ~symbol_projections
    ~simplify_toplevel =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let closure_symbols =
    Closure_id.Lmap.mapi
      (fun closure_id _func_decl ->
        let name =
          closure_id |> Closure_id.rename |> Closure_id.to_string
          |> Linkage_name.create
        in
        Symbol.create (Compilation_unit.get_current_exn ()) name)
      (Function_declarations.funs_in_order function_decls)
  in
  let closure_symbols_map =
    Closure_id.Lmap.bindings closure_symbols |> Closure_id.Map.of_list
  in
  let closure_bound_names =
    Closure_id.Map.map Bound_name.symbol closure_symbols_map
  in
  let closure_element_types =
    Var_within_closure.Map.map
      (fun closure_element ->
        Simple.pattern_match closure_element
          ~const:(fun _ -> T.alias_type_of K.value closure_element)
          ~name:(fun name ~coercion ->
            Name.pattern_match name
              ~var:(fun var ->
                match Variable.Map.find var closure_bound_vars_inverse with
                | exception Not_found ->
                  assert (DE.mem_variable (DA.denv dacc) var);
                  T.alias_type_of K.value closure_element
                | closure_id ->
                  let closure_symbol =
                    Closure_id.Map.find closure_id closure_symbols_map
                  in
                  let simple =
                    Simple.with_coercion (Simple.symbol closure_symbol) coercion
                  in
                  T.alias_type_of K.value simple)
              ~symbol:(fun _sym -> T.alias_type_of K.value closure_element)))
      closure_elements
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_toplevel
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~closure_element_types_all_sets:[closure_element_types]
  in
  let closure_bound_names_inside =
    C.closure_bound_names_inside_functions_exactly_one_set context
  in
  let { set_of_closures; code; dacc } =
    simplify_set_of_closures0 context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~closure_elements ~closure_element_types
  in
  let closure_symbols_set =
    Symbol.Set.of_list (Closure_id.Lmap.data closure_symbols)
  in
  assert (
    Symbol.Set.cardinal closure_symbols_set
    = Closure_id.Map.cardinal closure_symbols_map);
  let denv = DA.denv dacc in
  let closure_symbols_with_types =
    Closure_id.Map.map
      (fun symbol ->
        let typ = DE.find_symbol denv symbol in
        symbol, typ)
      closure_symbols_map
    |> Closure_id.Map.to_seq |> Closure_id.Lmap.of_seq
  in
  let dacc = introduce_code dacc code in
  let set_of_closures_lifted_constant =
    LC.create_set_of_closures denv ~closure_symbols_with_types
      ~symbol_projections
      (Rebuilt_static_const.create_set_of_closures
         (DE.are_rebuilding_terms denv)
         set_of_closures)
  in
  let dacc =
    DA.add_to_lifted_constant_accumulator ~also_add_to_env:() dacc
      (LCS.singleton set_of_closures_lifted_constant)
  in
  let denv, bindings =
    Closure_id.Map.fold
      (fun closure_id bound_var (denv, bindings) ->
        match Closure_id.Map.find closure_id closure_symbols_map with
        | exception Not_found ->
          Misc.fatal_errorf "No closure symbol for closure ID %a"
            Closure_id.print closure_id
        | closure_symbol ->
          let denv =
            let simple = Simple.symbol closure_symbol in
            let typ = T.alias_type_of K.value simple in
            DE.add_variable denv bound_var typ
          in
          let bindings = Bound_var.Map.add bound_var closure_symbol bindings in
          denv, bindings)
      closure_bound_vars
      (DA.denv dacc, Bound_var.Map.empty)
  in
  Simplify_named_result.have_lifted_set_of_closures (DA.with_denv dacc denv)
    bindings
    ~original_defining_expr:(Named.create_set_of_closures set_of_closures)

let simplify_non_lifted_set_of_closures0 dacc bound_vars ~closure_bound_vars
    set_of_closures ~closure_elements ~closure_element_types ~simplify_toplevel
    =
  let closure_bound_names =
    Closure_id.Map.map Bound_name.var closure_bound_vars
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_toplevel
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~closure_element_types_all_sets:[closure_element_types]
  in
  let closure_bound_names_inside =
    C.closure_bound_names_inside_functions_exactly_one_set context
  in
  let { set_of_closures; code; dacc } =
    simplify_set_of_closures0 context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~closure_elements ~closure_element_types
  in
  let dacc = introduce_code dacc code in
  let defining_expr =
    let named = Named.create_set_of_closures set_of_closures in
    let find_code_characteristics code_id =
      let env = Downwards_acc.denv dacc in
      let code_metadata =
        DE.find_code_exn env code_id |> Code_or_metadata.code_metadata
      in
      Cost_metrics.
        { cost_metrics = Code_metadata.cost_metrics code_metadata;
          params_arity = List.length (Code_metadata.params_arity code_metadata)
        }
    in
    Simplified_named.reachable_with_known_free_names ~find_code_characteristics
      (Named.create_set_of_closures set_of_closures)
      ~free_names:(Named.free_names named) ~try_reify:false
  in
  Simplify_named_result.have_simplified_to_single_term dacc bound_vars
    defining_expr
    ~original_defining_expr:(Named.create_set_of_closures set_of_closures)

type lifting_decision_result =
  { can_lift : bool;
    closure_elements : Simple.t Var_within_closure.Map.t;
    closure_element_types : T.t Var_within_closure.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let type_closure_elements_and_make_lifting_decision_for_one_set dacc
    ~name_mode_of_bound_vars ~closure_bound_vars_inverse set_of_closures =
  (* By computing the types of the closure elements, attempt to show that the
     set of closures can be lifted, and hence statically allocated. Note that
     simplifying the bodies of the functions won't change the set-of-closures'
     eligibility for lifting. That this is so follows from the fact that closure
     elements cannot be deleted without a global analysis, as an inlined
     function's body may reference them out of scope of the closure
     declaration. *)
  let closure_elements, closure_element_types, symbol_projections =
    Var_within_closure.Map.fold
      (fun closure_var env_entry
           (closure_elements, closure_element_types, symbol_projections) ->
        let env_entry, ty, symbol_projections =
          let ty =
            S.simplify_simple dacc env_entry
              ~min_name_mode:name_mode_of_bound_vars
          in
          let simple = T.get_alias_exn ty in
          (* Note down separately if [simple] remains a variable and is known to
             be equal to a projection from a symbol. *)
          let symbol_projections =
            Simple.pattern_match' simple
              ~const:(fun _ -> symbol_projections)
              ~symbol:(fun _ ~coercion:_ -> symbol_projections)
              ~var:(fun var ~coercion:_ ->
                (* [var] will already be canonical, as we require for the symbol
                   projections map. *)
                match DE.find_symbol_projection (DA.denv dacc) var with
                | None -> symbol_projections
                | Some proj -> Variable.Map.add var proj symbol_projections)
          in
          simple, ty, symbol_projections
        in
        let closure_elements =
          Var_within_closure.Map.add closure_var env_entry closure_elements
        in
        let closure_element_types =
          Var_within_closure.Map.add closure_var ty closure_element_types
        in
        closure_elements, closure_element_types, symbol_projections)
      (Set_of_closures.closure_elements set_of_closures)
      ( Var_within_closure.Map.empty,
        Var_within_closure.Map.empty,
        Variable.Map.empty )
  in
  let can_lift_coercion coercion =
    Name_occurrences.no_variables (Coercion.free_names coercion)
  in
  (* Note that [closure_bound_vars_inverse] doesn't need to include variables
     binding closures in other mutually-recursive sets, since if we get here in
     the case where we are considering lifting a set that has not been lifted
     before, there are never any other mutually-recursive sets ([Named.t] does
     not allow them). *)
  let can_lift =
    Name_mode.is_normal name_mode_of_bound_vars
    && Var_within_closure.Map.for_all
         (fun _ simple ->
           can_lift_coercion (Simple.coercion simple)
           && Simple.pattern_match' simple
                ~const:(fun _ -> true)
                ~symbol:(fun _ ~coercion:_ -> true)
                ~var:(fun var ~coercion:_ ->
                  (* Variables that are not the closure bound vars, including
                     ones bound to symbol projections (since such projections
                     might be from inconstant symbols that were lifted [Local]
                     allocations), in the definition of the set of closures will
                     currently prevent lifting if the allocation mode is [Local]
                     and we cannot show that such variables never hold
                     locally-allocated blocks (pointers to which from
                     statically-allocated blocks are forbidden). Also see
                     comment in types/reify.ml. *)
                  Variable.Map.mem var closure_bound_vars_inverse
                  (* the closure bound vars will be replaced by symbols upon
                     lifting *)
                  || (match Set_of_closures.alloc_mode set_of_closures with
                     | Local ->
                       T.never_holds_locally_allocated_values
                         (DA.typing_env dacc) var K.value
                     | Heap -> true)
                     && (DE.is_defined_at_toplevel (DA.denv dacc) var
                        (* If [var] is known to be a symbol projection, it
                           doesn't matter if it isn't in scope at the place
                           where we will eventually insert the "let symbol", as
                           the binding to the projection from the relevant
                           symbol can always be rematerialised. *)
                        || Variable.Map.mem var symbol_projections)))
         closure_elements
  in
  { can_lift; closure_elements; closure_element_types; symbol_projections }

let type_closure_elements_for_previously_lifted_set dacc
    ~name_mode_of_bound_vars set_of_closures =
  type_closure_elements_and_make_lifting_decision_for_one_set dacc
    ~name_mode_of_bound_vars ~closure_bound_vars_inverse:Variable.Map.empty
    set_of_closures

let simplify_non_lifted_set_of_closures dacc (bound_vars : Bound_pattern.t)
    set_of_closures =
  let closure_bound_vars = Bound_pattern.must_be_set_of_closures bound_vars in
  (* CR-someday mshinwell: This should probably be handled differently, but will
     require some threading through *)
  let name_mode_of_bound_vars = Bound_pattern.name_mode bound_vars in
  let closure_bound_vars, closure_bound_vars_inverse =
    List.fold_left2
      (fun (closure_bound_vars, closure_bound_vars_inverse) closure_id var ->
        ( Closure_id.Map.add closure_id var closure_bound_vars,
          Variable.Map.add (Bound_var.var var) closure_id
            closure_bound_vars_inverse ))
      (Closure_id.Map.empty, Variable.Map.empty)
      (Set_of_closures.function_decls set_of_closures
      |> Function_declarations.funs_in_order |> Closure_id.Lmap.keys)
      closure_bound_vars
  in
  let { can_lift; closure_elements; closure_element_types; symbol_projections }
      =
    type_closure_elements_and_make_lifting_decision_for_one_set dacc
      ~name_mode_of_bound_vars ~closure_bound_vars_inverse set_of_closures
  in
  if can_lift
  then
    simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
      ~closure_bound_vars set_of_closures ~closure_elements ~symbol_projections
  else
    simplify_non_lifted_set_of_closures0 dacc bound_vars ~closure_bound_vars
      set_of_closures ~closure_elements ~closure_element_types

let simplify_lifted_set_of_closures0 context ~closure_symbols
    ~closure_bound_names_inside ~closure_elements ~closure_element_types
    set_of_closures =
  let closure_bound_names =
    Closure_id.Lmap.map Bound_name.symbol closure_symbols
    |> Closure_id.Lmap.bindings |> Closure_id.Map.of_list
  in
  let { set_of_closures; code; dacc } =
    simplify_set_of_closures0 context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~closure_elements ~closure_element_types
  in
  let dacc = introduce_code dacc code in
  let code_patterns =
    Code_id.Lmap.keys code |> List.map Bound_symbols.Pattern.code
  in
  let set_of_closures_pattern =
    Bound_symbols.Pattern.set_of_closures closure_symbols
  in
  let bound_symbols =
    set_of_closures_pattern :: code_patterns |> Bound_symbols.create
  in
  let code_static_consts = Code_id.Lmap.data code in
  let set_of_closures_static_const =
    Rebuilt_static_const.create_set_of_closures
      (DA.are_rebuilding_terms dacc)
      set_of_closures
  in
  let static_consts =
    set_of_closures_static_const :: code_static_consts
    |> Rebuilt_static_const.Group.create
  in
  bound_symbols, static_consts, dacc

module List = struct
  include List

  let rec fold_left3 f accu l1 l2 l3 =
    match l1, l2, l3 with
    | [], [], [] -> accu
    | a1 :: l1, a2 :: l2, a3 :: l3 -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
    | _, _, _ -> invalid_arg "List.fold_left3"
end

let simplify_lifted_sets_of_closures dacc ~all_sets_of_closures_and_symbols
    ~closure_bound_names_all_sets ~simplify_toplevel =
  let all_sets_of_closures = List.map snd all_sets_of_closures_and_symbols in
  let closure_elements_and_types_all_sets =
    List.map
      (fun set_of_closures ->
        let { can_lift = _;
              closure_elements;
              closure_element_types;
              symbol_projections = _
            } =
          type_closure_elements_for_previously_lifted_set dacc
            ~name_mode_of_bound_vars:Name_mode.normal set_of_closures
        in
        closure_elements, closure_element_types)
      all_sets_of_closures
  in
  let closure_element_types_all_sets =
    List.map snd closure_elements_and_types_all_sets
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_toplevel ~all_sets_of_closures
      ~closure_bound_names_all_sets ~closure_element_types_all_sets
  in
  let closure_bound_names_inside_functions_all_sets =
    C.closure_bound_names_inside_functions_all_sets context
  in
  List.fold_left3
    (fun (patterns_acc, static_consts_acc, dacc)
         (closure_symbols, set_of_closures) closure_bound_names_inside
         (closure_elements, closure_element_types) ->
      let patterns, static_consts, dacc =
        if Set_of_closures.is_empty set_of_closures
        then
          let bound_symbols =
            Bound_symbols.create
              [Bound_symbols.Pattern.set_of_closures closure_symbols]
          in
          let static_consts =
            Rebuilt_static_const.Group.create
              [ Rebuilt_static_const.create_set_of_closures
                  (DA.are_rebuilding_terms dacc)
                  set_of_closures ]
          in
          bound_symbols, static_consts, dacc
        else
          simplify_lifted_set_of_closures0 context ~closure_symbols
            ~closure_bound_names_inside ~closure_elements ~closure_element_types
            set_of_closures
      in
      (* The order doesn't matter here -- see comment in [Simplify_static_const]
         where this function is called from. *)
      let static_const_group =
        Rebuilt_static_const.Group.concat static_consts static_consts_acc
      in
      Bound_symbols.concat patterns patterns_acc, static_const_group, dacc)
    (Bound_symbols.empty, Rebuilt_static_const.Group.empty, dacc)
    all_sets_of_closures_and_symbols
    closure_bound_names_inside_functions_all_sets
    closure_elements_and_types_all_sets
