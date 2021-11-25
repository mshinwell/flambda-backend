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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import
module CSE = Common_subexpression_elimination
module K = Flambda_kind
module BP = Bound_parameter
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

type resolver = Compilation_unit.t -> Flambda2_types.Typing_env.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

type t0 =
  { round : int;
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
    inlining_state : Inlining_state.t;
    float_const_prop : bool;
    at_unit_toplevel : bool;
    unit_toplevel_return_continuation : Continuation.t;
    unit_toplevel_exn_continuation : Continuation.t;
    symbols_currently_being_defined : Symbol.Set.t;
    variables_defined_at_toplevel : Variable.Set.t;
    cse : CSE.t;
    do_not_rebuild_terms : bool;
    closure_info : Closure_info.t;
    get_imported_code : unit -> Exported_code.t;
    all_code : Code.t Code_id.Map.t
  }

type t = TE.t * t0

let print_debuginfo ppf dbg =
  if Debuginfo.is_none dbg
  then Format.pp_print_string ppf "None"
  else Debuginfo.print_compact ppf dbg

let [@ocamlformat "disable"] print ppf (typing_env, { round;
                inlined_debuginfo; can_inline;
                inlining_state; float_const_prop;
                at_unit_toplevel; unit_toplevel_exn_continuation;
                symbols_currently_being_defined;
                variables_defined_at_toplevel; cse;
                do_not_rebuild_terms; closure_info;
                unit_toplevel_return_continuation; all_code;
                get_imported_code = _;
              }) =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(round@ %d)@]@ \
      @[<hov 1>(typing_env@ %a)@]@ \
      @[<hov 1>(inlined_debuginfo@ %a)@]@ \
      @[<hov 1>(can_inline@ %b)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      @[<hov 1>(float_const_prop@ %b)@]@ \
      @[<hov 1>(at_unit_toplevel@ %b)@]@ \
      @[<hov 1>(unit_toplevel_return_continuation@ %a)@]@ \
      @[<hov 1>(unit_toplevel_exn_continuation@ %a)@]@ \
      @[<hov 1>(symbols_currently_being_defined@ %a)@]@ \
      @[<hov 1>(variables_defined_at_toplevel@ %a)@]@ \
      @[<hov 1>(cse@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(do_not_rebuild_terms@ %b)@]@ \
      @[<hov 1>(closure_info@ %a)@]@ \
      @[<hov 1>(all_code@ %a)@]\
      )@]"
    round
    TE.print typing_env
    print_debuginfo inlined_debuginfo
    can_inline
    Inlining_state.print inlining_state
    float_const_prop
    at_unit_toplevel
    Continuation.print unit_toplevel_return_continuation
    Continuation.print unit_toplevel_exn_continuation
    Symbol.Set.print symbols_currently_being_defined
    Variable.Set.print variables_defined_at_toplevel
    CSE.print cse
    do_not_rebuild_terms
    Closure_info.print closure_info
    (Code_id.Map.print Code.print) all_code

let create ~round ~(resolver : resolver)
    ~(get_imported_names : get_imported_names)
    ~(get_imported_code : get_imported_code) ~float_const_prop
    ~unit_toplevel_exn_continuation ~unit_toplevel_return_continuation =
  let typing_env = TE.create ~resolver ~get_imported_names in
  let t0 =
    { round;
      inlined_debuginfo = Debuginfo.none;
      can_inline = true;
      inlining_state = Inlining_state.default ~round;
      float_const_prop;
      at_unit_toplevel = true;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      symbols_currently_being_defined = Symbol.Set.empty;
      variables_defined_at_toplevel = Variable.Set.empty;
      cse = CSE.empty;
      do_not_rebuild_terms = false;
      closure_info = Closure_info.not_in_a_closure;
      all_code = Code_id.Map.empty;
      get_imported_code
    }
  in
  typing_env, t0

let resolver (typing_env, _) = TE.resolver typing_env

let typing_env (typing_env, _) = typing_env

let round (_, t0) = t0.round

let get_continuation_scope_level (typing_env, _) = TE.current_scope typing_env

let can_inline (_, t0) = t0.can_inline

let float_const_prop (_, t0) = t0.float_const_prop

let unit_toplevel_exn_continuation (_, t0) = t0.unit_toplevel_exn_continuation

let unit_toplevel_return_continuation (_, t0) =
  t0.unit_toplevel_return_continuation

let at_unit_toplevel (_, t0) = t0.at_unit_toplevel

let set_not_at_unit_toplevel (typing_env, t0) =
  typing_env, { t0 with at_unit_toplevel = false }

let set_at_unit_toplevel_state (typing_env, t0) at_unit_toplevel =
  typing_env, { t0 with at_unit_toplevel }

let is_defined_at_toplevel (_, t0) var =
  Variable.Set.mem var t0.variables_defined_at_toplevel

let get_inlining_state (_, t0) = t0.inlining_state

let set_inlining_state (typing_env, t0) inlining_state =
  typing_env, { t0 with inlining_state }

(* CR mshinwell: remove "_level" *)
let increment_continuation_scope_level (typing_env, t0) =
  TE.increment_scope typing_env, t0

let increment_continuation_scope_level_twice t =
  increment_continuation_scope_level (increment_continuation_scope_level t)

let now_defining_symbol ((typing_env, t0) as t) symbol =
  if Symbol.Set.mem symbol t0.symbols_currently_being_defined
  then
    Misc.fatal_errorf "Already defining symbol %a:@ %a" Symbol.print symbol
      print t;
  let symbols_currently_being_defined =
    Symbol.Set.add symbol t0.symbols_currently_being_defined
  in
  typing_env, { t0 with symbols_currently_being_defined }

let no_longer_defining_symbol ((typing_env, t0) as t) symbol =
  if not (Symbol.Set.mem symbol t0.symbols_currently_being_defined)
  then
    Misc.fatal_errorf "Not currently defining symbol %a:@ %a" Symbol.print
      symbol print t;
  let symbols_currently_being_defined =
    Symbol.Set.remove symbol t0.symbols_currently_being_defined
  in
  typing_env, { t0 with symbols_currently_being_defined }

let symbol_is_currently_being_defined (_, t0) symbol =
  Symbol.Set.mem symbol t0.symbols_currently_being_defined

let symbols_currently_being_defined (_, t0) = t0.symbols_currently_being_defined

let enter_set_of_closures
    ( typing_env,
      { round;
        inlined_debuginfo = _;
        can_inline;
        inlining_state;
        float_const_prop;
        at_unit_toplevel = _;
        unit_toplevel_return_continuation;
        unit_toplevel_exn_continuation;
        symbols_currently_being_defined;
        variables_defined_at_toplevel;
        cse = _;
        do_not_rebuild_terms;
        closure_info = _;
        get_imported_code;
        all_code
      } ) =
  let typing_env = TE.closure_env typing_env in
  let t0 =
    { round;
      inlined_debuginfo = Debuginfo.none;
      can_inline;
      inlining_state;
      float_const_prop;
      at_unit_toplevel = false;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      symbols_currently_being_defined;
      variables_defined_at_toplevel;
      cse = CSE.empty;
      do_not_rebuild_terms;
      closure_info = Closure_info.in_a_set_of_closures;
      get_imported_code;
      all_code
    }
  in
  typing_env, t0

let define_variable (typing_env, t0) var kind =
  let typing_env =
    let var = Bound_name.var var in
    TE.add_definition typing_env var kind
  in
  let t0 =
    if t0.at_unit_toplevel
    then
      { t0 with
        variables_defined_at_toplevel =
          Variable.Set.add (Bound_var.var var) t0.variables_defined_at_toplevel
      }
    else t0
  in
  typing_env, t0

let add_name (typing_env, t0) name ty =
  let typing_env =
    TE.add_equation
      (TE.add_definition typing_env name (T.kind ty))
      (Bound_name.name name) ty
  in
  let t0 =
    Name.pattern_match (Bound_name.name name)
      ~var:(fun var ->
        if t0.at_unit_toplevel
        then
          { t0 with
            variables_defined_at_toplevel =
              Variable.Set.add var t0.variables_defined_at_toplevel
          }
        else t0)
      ~symbol:(fun _ -> t0)
  in
  typing_env, t0

let add_variable0 typing_env t0 var ty ~at_unit_toplevel =
  let typing_env =
    let var' = Bound_name.var var in
    TE.add_equation
      (TE.add_definition typing_env var' (T.kind ty))
      (Name.var (Bound_var.var var))
      ty
  in
  let t0 =
    if at_unit_toplevel
    then
      { t0 with
        variables_defined_at_toplevel =
          Variable.Set.add (Bound_var.var var) t0.variables_defined_at_toplevel
      }
    else t0
  in
  typing_env, t0

let add_variable (typing_env, t0) var ty =
  add_variable0 typing_env t0 var ty ~at_unit_toplevel:t0.at_unit_toplevel

let add_equation_on_variable (typing_env, t0) var ty =
  let typing_env = TE.add_equation typing_env (Name.var var) ty in
  typing_env, t0

let mem_name (typing_env, _) name = TE.mem typing_env name

let mem_variable (typing_env, _) var = TE.mem typing_env (Name.var var)

let define_symbol (typing_env, t0) sym kind =
  let typing_env =
    let sym = Bound_name.create (Name.symbol sym) Name_mode.normal in
    TE.add_definition typing_env sym kind
  in
  typing_env, t0

let define_symbol_if_undefined ((typing_env, _) as t) sym kind =
  if TE.mem typing_env (Name.symbol sym) then t else define_symbol t sym kind

let add_symbol (typing_env, t0) sym ty =
  let typing_env =
    let sym = Name.symbol sym in
    let sym' = Bound_name.create sym Name_mode.normal in
    TE.add_equation (TE.add_definition typing_env sym' (T.kind ty)) sym ty
  in
  typing_env, t0

let add_equation_on_symbol (typing_env, t0) sym ty =
  let typing_env =
    let sym = Name.symbol sym in
    TE.add_equation typing_env sym ty
  in
  typing_env, t0

let mem_symbol t sym = mem_name t (Name.symbol sym)

let find_symbol t sym = TE.find (typing_env t) (Name.symbol sym) (Some K.value)

let add_symbol_projection (typing_env, t0) var proj =
  let typing_env = TE.add_symbol_projection typing_env var proj in
  typing_env, t0

let find_symbol_projection (typing_env, _) var =
  TE.find_symbol_projection typing_env var

let define_name (typing_env, t0) name kind =
  let typing_env = TE.add_definition typing_env name kind in
  let t0 =
    Name.pattern_match (Bound_name.name name)
      ~var:(fun var ->
        if t0.at_unit_toplevel
        then
          { t0 with
            variables_defined_at_toplevel =
              Variable.Set.add var t0.variables_defined_at_toplevel
          }
        else t0)
      ~symbol:(fun _ -> t0)
  in
  typing_env, t0

let define_name_if_undefined ((typing_env, _) as t) name kind =
  if TE.mem typing_env (Bound_name.to_name name)
  then t
  else define_name t name kind

let add_equation_on_name (typing_env, t0) name ty =
  let typing_env = TE.add_equation typing_env name ty in
  typing_env, t0

(* let add_symbol_if_not_defined t sym ty = let name = Name.symbol sym in if
   TE.mem t.typing_env name then t else add_symbol t sym ty *)

let define_parameters t ~params =
  List.fold_left
    (fun t param ->
      let var = Bound_var.create (BP.var param) Name_mode.normal in
      define_variable t var (K.With_subkind.kind (BP.kind param)))
    t params

let define_parameters_as_bottom t ~params =
  List.fold_left
    (fun t param ->
      let var = Bound_var.create (BP.var param) Name_mode.normal in
      let kind = K.With_subkind.kind (BP.kind param) in
      let t = define_variable t var kind in
      add_equation_on_variable t (BP.var param) (T.bottom kind))
    t params

let add_parameters ?at_unit_toplevel ((_, t0) as t) params ~param_types =
  if List.compare_lengths params param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      param_types;
  let at_unit_toplevel =
    Option.value at_unit_toplevel ~default:t0.at_unit_toplevel
  in
  List.fold_left2
    (fun (typing_env, t0) param param_type ->
      let var = Bound_var.create (BP.var param) Name_mode.normal in
      add_variable0 typing_env t0 var param_type ~at_unit_toplevel)
    t params param_types

let add_parameters_with_unknown_types' ?at_unit_toplevel t params =
  let param_types =
    ListLabels.map params ~f:(fun param ->
        T.unknown_with_subkind (BP.kind param))
  in
  add_parameters ?at_unit_toplevel t params ~param_types, param_types

let add_parameters_with_unknown_types ?at_unit_toplevel t params =
  fst (add_parameters_with_unknown_types' ?at_unit_toplevel t params)

let mark_parameters_as_toplevel (typing_env, t0) params =
  let variables_defined_at_toplevel =
    Variable.Set.union t0.variables_defined_at_toplevel (BP.List.var_set params)
  in
  typing_env, { t0 with variables_defined_at_toplevel }

let add_variable_and_extend_typing_environment (typing_env, t0) var ty
    env_extension =
  (* This is a combined operation to reduce allocation. *)
  let typing_env =
    let var' = Bound_name.var var in
    TE.add_equation
      (TE.add_definition typing_env var' (T.kind ty))
      (Name.var (Bound_var.var var))
      ty
  in
  let t0 =
    if t0.at_unit_toplevel
    then
      { t0 with
        variables_defined_at_toplevel =
          Variable.Set.add (Bound_var.var var) t0.variables_defined_at_toplevel
      }
    else t0
  in
  let typing_env = TE.add_env_extension typing_env env_extension in
  typing_env, t0

let with_typing_env (_, t0) typing_env = typing_env, t0

let[@inline always] map_typing_env (typing_env, t0) ~f =
  (f [@inlined hint]) typing_env, t0

let check_variable_is_bound ((typing_env, _) as t) var =
  if not (TE.mem typing_env (Name.var var))
  then
    Misc.fatal_errorf "Unbound variable %a in environment:@ %a" Variable.print
      var print t

let check_symbol_is_bound ((typing_env, _) as t) sym =
  if not (TE.mem typing_env (Name.symbol sym))
  then
    Misc.fatal_errorf "Unbound symbol %a in environment:@ %a" Symbol.print sym
      print t

let check_name_is_bound ((typing_env, _) as t) name =
  if not (TE.mem typing_env name)
  then
    Misc.fatal_errorf "Unbound name %a in environment:@ %a" Name.print name
      print t

let check_simple_is_bound t (simple : Simple.t) =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> check_name_is_bound t name)
    ~const:(fun _ -> ())

let mem_code (_, t0) id =
  Code_id.Map.mem id t0.all_code
  || Exported_code.mem id (t0.get_imported_code ())

let find_code_exn (_, t0) id =
  match Code_id.Map.find id t0.all_code with
  | exception Not_found -> Exported_code.find_exn (t0.get_imported_code ()) id
  | code -> Code_or_metadata.create code

let check_code_id_is_bound t code_id =
  if not (mem_code t code_id)
  then
    Misc.fatal_errorf "Unbound code ID %a in environment:@ %a" Code_id.print
      code_id print t

let define_code (typing_env, t0) ~code_id ~code =
  if not
       (Code_id.in_compilation_unit code_id
          (Compilation_unit.get_current_exn ()))
  then
    Misc.fatal_errorf "Cannot define code ID %a as it is from another unit:@ %a"
      Code_id.print code_id Code.print code;
  if not (Code_id.equal code_id (Code.code_id code))
  then
    Misc.fatal_errorf "Code ID %a does not match code ID in@ %a" Code_id.print
      code_id Code.print code;
  let typing_env =
    TE.add_to_code_age_relation typing_env ~new_code_id:code_id
      ~old_code_id:(Code.newer_version_of code)
  in
  let all_code = Code_id.Map.add code_id code t0.all_code in
  typing_env, { t0 with all_code }

let set_inlined_debuginfo (typing_env, t0) dbg =
  typing_env, { t0 with inlined_debuginfo = dbg }

let get_inlined_debuginfo (_, t0) = t0.inlined_debuginfo

let add_inlined_debuginfo' (_, t0) dbg =
  Debuginfo.inline t0.inlined_debuginfo dbg

let add_inlined_debuginfo ((typing_env, t0) as t) dbg =
  typing_env, { t0 with inlined_debuginfo = add_inlined_debuginfo' t dbg }

let disable_function_inlining (typing_env, t0) =
  typing_env, { t0 with can_inline = false }

let cse (_, t0) = t0.cse

let add_cse ((typing_env, t0) as t) prim ~bound_to =
  let scope = get_continuation_scope_level t in
  let cse = CSE.add t0.cse prim ~bound_to scope in
  typing_env, { t0 with cse }

let find_cse (_, t0) prim = CSE.find t0.cse prim

let with_cse (typing_env, t0) cse = typing_env, { t0 with cse }

let set_do_not_rebuild_terms_and_disable_inlining (typing_env, t0) =
  typing_env, { t0 with do_not_rebuild_terms = true; can_inline = false }

let set_rebuild_terms (typing_env, t0) =
  typing_env, { t0 with do_not_rebuild_terms = false }

type are_rebuilding_terms = bool

let are_rebuilding_terms (_, t0) = not t0.do_not_rebuild_terms

let are_rebuilding_terms_to_bool are_rebuilding = are_rebuilding

let enter_closure code_id ~return_continuation ~exn_continuation (typing_env, t0)
    =
  ( typing_env,
    { t0 with
      closure_info =
        Closure_info.in_a_closure code_id ~return_continuation ~exn_continuation
    } )

let closure_info (_, t0) = t0.closure_info

let inlining_arguments (_, { inlining_state; _ }) =
  Inlining_state.arguments inlining_state

let set_inlining_arguments arguments (typing_env, t0) =
  ( typing_env,
    { t0 with
      inlining_state = Inlining_state.with_arguments arguments t0.inlining_state
    } )

let enter_inlined_apply ~called_code ~apply (typing_env, t0) =
  let arguments =
    Inlining_state.arguments t0.inlining_state
    |> Inlining_arguments.meet (Code.inlining_arguments called_code)
    |> Inlining_arguments.meet (Apply.inlining_arguments apply)
  in
  let t0 =
    { t0 with
      inlined_debuginfo = Apply.dbg apply;
      inlining_state =
        t0.inlining_state |> Inlining_state.increment_depth
        |> Inlining_state.with_arguments arguments
    }
  in
  typing_env, t0

let generate_phantom_lets t =
  Flambda_features.debug ()
  && Flambda_features.Expert.phantom_lets ()
  (* It would be a waste of time generating phantom lets when not rebuilding
     terms, since they have no effect on cost metrics. *)
  && are_rebuilding_terms t
