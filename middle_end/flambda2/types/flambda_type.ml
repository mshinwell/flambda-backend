(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Decide on doc or non-doc comments in here. There are some
   modules which aren't exposed in the interface but probably require
   documentation. *)

(* CR mshinwell: Remove when warning 60 fixed *)
[@@@ocaml.warning "-60"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
include Type_grammar

type flambda_type = t

let meet env t1 t2 : _ Or_bottom.t =
  let meet_env = Typing_env.Meet_env.create env in
  meet meet_env t1 t2

let join ?bound_name central_env ~left_env ~left_ty ~right_env ~right_ty =
  let join_env = Typing_env.Join_env.create central_env ~left_env ~right_env in
  match join ?bound_name join_env left_ty right_ty with
  | Unknown -> unknown_like left_ty
  | Known ty -> ty

let arity_of_list ts = Flambda_arity.create (List.map kind ts)

type typing_env = Typing_env.t

type typing_env_extension = Typing_env.Typing_env_extension.t

module Typing_env_extension = Typing_env.Typing_env_extension

let cut_and_n_way_join = Typing_env_level_join.cut_and_n_way_join

type 'a type_accessor = Typing_env.t -> 'a

let unknown_types_from_arity arity = List.map (fun kind -> unknown kind) arity

let rec unknown_with_descr (descr : Flambda_kind.With_subkind.descr) =
  match descr with
  | Any_value -> any_value ()
  | Naked_number Naked_immediate -> any_naked_immediate ()
  | Naked_number Naked_float -> any_naked_float ()
  | Naked_number Naked_int32 -> any_naked_int32 ()
  | Naked_number Naked_int64 -> any_naked_int64 ()
  | Naked_number Naked_nativeint -> any_naked_nativeint ()
  | Boxed_float -> any_boxed_float ()
  | Boxed_int32 -> any_boxed_int32 ()
  | Boxed_int64 -> any_boxed_int64 ()
  | Boxed_nativeint -> any_boxed_nativeint ()
  | Tagged_immediate -> any_tagged_immediate ()
  | Rec_info -> any_rec_info ()
  | Block { tag; fields } ->
    assert (not (Tag.equal tag Tag.double_array_tag));
    immutable_block ~is_unique:false tag ~field_kind:Flambda_kind.value
      ~fields:(List.map unknown_with_descr fields)
  | Float_block { num_fields } ->
    immutable_block ~is_unique:false Tag.double_array_tag
      ~field_kind:Flambda_kind.naked_float
      ~fields:(List.init num_fields (fun _ -> any_naked_float ()))

let unknown_with_subkind kind =
  unknown_with_descr (Flambda_kind.With_subkind.descr kind)

let unknown_types_from_arity_with_subkinds arity =
  List.map (fun kind -> unknown_with_subkind kind) arity

let bottom_types_from_arity arity = List.map (fun kind -> bottom kind) arity

type to_lift =
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : var_or_symbol_or_tagged_immediate list
      }
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint_32_64.t

type reification_result =
  | Lift of to_lift
  | Lift_set_of_closures of
      { closure_id : Closure_id.t;
        function_decls : Function_declaration_type.T0.t Closure_id.Map.t;
        closure_vars : Simple.t Var_within_closure.Map.t
      }
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

(* CR mshinwell: Think more to identify all the cases that should be in this
   function. *)
let reify ?allowed_if_free_vars_defined_in ?additional_free_var_criterion
    ?disallowed_free_vars ?(allow_unique = false) env ~min_name_mode t :
    reification_result =
  let var_allowed var =
    match allowed_if_free_vars_defined_in with
    | None -> false
    | Some allowed_if_free_vars_defined_in -> (
      Typing_env.mem ~min_name_mode allowed_if_free_vars_defined_in
        (Name.var var)
      && begin
           match additional_free_var_criterion with
           | None -> true
           | Some criterion -> criterion var
         end
      &&
      match disallowed_free_vars with
      | None -> true
      | Some disallowed_free_vars ->
        not (Variable.Set.mem var disallowed_free_vars))
  in
  let canonical_simple =
    match
      Typing_env.get_alias_then_canonical_simple_exn env ~min_name_mode t
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match canonical_simple with
  | Some canonical_simple when Simple.is_symbol canonical_simple ->
    (* Don't lift things that are already bound to symbols. Apart from anything
       else, this could cause aliases between symbols, which are currently
       forbidden (every symbol has the same binding time). *)
    Cannot_reify
  | canonical_simple_opt -> (
    let try_canonical_simple () =
      match canonical_simple_opt with
      | None -> Cannot_reify
      | Some canonical_simple -> Simple canonical_simple
    in
    match expand_head t env with
    | Const const -> Simple (Simple.const_from_descr const)
    | Value (Ok (Variant blocks_imms)) -> (
      if blocks_imms.is_unique && not allow_unique
      then try_canonical_simple ()
      else
        match blocks_imms.blocks, blocks_imms.immediates with
        | Known blocks, Known imms ->
          if is_bottom env imms
          then
            match Row_like.For_blocks.get_singleton blocks with
            | None -> try_canonical_simple ()
            | Some ((tag, size), field_types) -> (
              assert (
                Targetint_31_63.Imm.equal size
                  (Product.Int_indexed.width field_types));
              (* CR mshinwell: Could recognise other things, e.g. tagged
                 immediates and float arrays, supported by [Static_part]. *)
              match Tag.Scannable.of_tag tag with
              | None -> try_canonical_simple ()
              | Some tag ->
                let field_types = Product.Int_indexed.components field_types in
                let vars_or_symbols_or_tagged_immediates =
                  List.filter_map
                    (fun field_type : var_or_symbol_or_tagged_immediate option ->
                      match
                        (* CR mshinwell: Change this to a function
                           [prove_equals_to_simple]? *)
                        prove_equals_to_var_or_symbol_or_tagged_immediate env
                          field_type
                      with
                      | Proved (_, coercion) when not (Coercion.is_id coercion)
                        ->
                        (* CR-someday lmaurer: Support lifting things whose
                           fields have coercions. *)
                        None
                      | Proved (Var var, _) ->
                        if var_allowed var then Some (Var var) else None
                      | Proved (Symbol sym, _) -> Some (Symbol sym)
                      | Proved (Tagged_immediate imm, _) ->
                        Some (Tagged_immediate imm)
                      (* CR mshinwell: [Invalid] should propagate up *)
                      | Unknown | Invalid -> None)
                    field_types
                in
                if List.compare_lengths field_types
                     vars_or_symbols_or_tagged_immediates
                   = 0
                then
                  Lift
                    (Immutable_block
                       { tag;
                         is_unique = blocks_imms.is_unique;
                         fields = vars_or_symbols_or_tagged_immediates
                       })
                else try_canonical_simple ())
          else if Row_like.For_blocks.is_bottom blocks
          then
            match prove_naked_immediates env imms with
            | Proved imms -> begin
              match Targetint_31_63.Set.get_singleton imms with
              | None -> try_canonical_simple ()
              | Some imm ->
                Simple (Simple.const (Reg_width_const.tagged_immediate imm))
            end
            | Unknown -> try_canonical_simple ()
            | Invalid -> Invalid
          else try_canonical_simple ()
        | _, _ -> try_canonical_simple ())
    | Value (Ok (Closures closures)) -> begin
      (* CR mshinwell: Here and above, move to separate function. *)
      match
        Row_like.For_closures_entry_by_set_of_closures_contents.get_singleton
          closures.by_closure_id
      with
      | None -> try_canonical_simple ()
      | Some ((closure_id, contents), closures_entry) ->
        (* CR mshinwell: What about if there were multiple entries in the
           row-like structure for the same closure ID? This is ruled out by
           [get_singleton] at the moment. We should probably choose the best
           entry from the [Row_like] structure. *)
        let closure_ids = Set_of_closures_contents.closures contents in
        (* CR mshinwell: Should probably check
           [Set_of_closures_contents.closure_vars contents]? *)
        if not (Closure_id.Set.mem closure_id closure_ids)
        then
          Misc.fatal_errorf
            "Closure ID %a expected in set-of-closures-contents in closure \
             type@ %a"
            Closure_id.print closure_id print t;
        (* Format.eprintf "Reifying closure %a@.Contents:@.%a@.Entry:@.%a@.Typing env:@.%a@.Backtrace:@.%s@."
         *   Closure_id.print closure_id
         *   Set_of_closures_contents.print contents
         *   Closures_entry.print closures_entry
         *   Typing_env.print env
         *   Printexc.(raw_backtrace_to_string (get_callstack 10)); *)
        let function_decls_with_closure_vars =
          Closure_id.Set.fold
            (fun closure_id function_decls_with_closure_vars ->
              match
                Closures_entry.find_function_declaration closures_entry
                  closure_id
              with
              | Bottom -> function_decls_with_closure_vars
              | Ok function_decl -> (
                match function_decl with
                | Bottom | Unknown -> function_decls_with_closure_vars
                | Ok t0 ->
                  (* CR mshinwell: We're ignoring [coercion] *)
                  let closure_var_types =
                    Closures_entry.closure_var_types closures_entry
                  in
                  let closure_var_simples =
                    Var_within_closure.Map.filter_map
                      (fun _closure_var closure_var_type ->
                        match
                          prove_equals_to_var_or_symbol_or_tagged_immediate env
                            closure_var_type
                        with
                        | Proved (Var var, coercion) ->
                          if var_allowed var
                          then
                            Some
                              (Simple.with_coercion (Simple.var var) coercion)
                          else None
                        | Proved (Symbol sym, coercion) ->
                          Some
                            (Simple.with_coercion (Simple.symbol sym) coercion)
                        | Proved (Tagged_immediate imm, coercion) ->
                          Some
                            (Simple.with_coercion
                               (Simple.const
                                  (Reg_width_const.tagged_immediate imm))
                               coercion)
                        | Unknown | Invalid -> None)
                      closure_var_types
                  in
                  if Var_within_closure.Map.cardinal closure_var_types
                     <> Var_within_closure.Map.cardinal closure_var_simples
                  then function_decls_with_closure_vars
                  else
                    Closure_id.Map.add closure_id (t0, closure_var_simples)
                      function_decls_with_closure_vars))
            closure_ids Closure_id.Map.empty
        in
        if Closure_id.Set.cardinal closure_ids
           <> Closure_id.Map.cardinal function_decls_with_closure_vars
        then try_canonical_simple ()
        else
          let function_decls =
            Closure_id.Map.map
              (fun (function_decl, _) -> function_decl)
              function_decls_with_closure_vars
          in
          let closure_vars =
            Closure_id.Map.fold
              (fun _closure_id (_function_decl, closure_var_simples)
                   all_closure_vars ->
                Var_within_closure.Map.fold
                  (fun closure_var simple all_closure_vars ->
                    begin
                      match
                        Var_within_closure.Map.find closure_var all_closure_vars
                      with
                      | exception Not_found -> ()
                      | existing_simple ->
                        if not (Simple.equal simple existing_simple)
                        then
                          Misc.fatal_errorf
                            "Disagreement on %a and %a (closure var %a)@ \
                             whilst reifying set-of-closures from:@ %a"
                            Simple.print simple Simple.print existing_simple
                            Var_within_closure.print closure_var print t
                    end;
                    Var_within_closure.Map.add closure_var simple
                      all_closure_vars)
                  closure_var_simples all_closure_vars)
              function_decls_with_closure_vars Var_within_closure.Map.empty
          in
          Lift_set_of_closures { closure_id; function_decls; closure_vars }
    end
    (* CR mshinwell: share code with [prove_equals_tagged_immediates], above *)
    | Naked_immediate (Ok (Is_int scrutinee_ty)) -> begin
      match prove_is_int env scrutinee_ty with
      | Proved true -> Simple Simple.untagged_const_true
      | Proved false -> Simple Simple.untagged_const_false
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
    end
    | Naked_immediate (Ok (Get_tag block_ty)) -> begin
      match prove_tags_must_be_a_block env block_ty with
      | Proved tags -> (
        let is =
          Tag.Set.fold
            (fun tag is -> Targetint_31_63.Set.add (Tag.to_target_imm tag) is)
            tags Targetint_31_63.Set.empty
        in
        match Targetint_31_63.Set.get_singleton is with
        | None -> try_canonical_simple ()
        | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
    end
    | Naked_float (Ok fs) -> begin
      match Float.Set.get_singleton fs with
      | None -> try_canonical_simple ()
      | Some f -> Simple (Simple.const (Reg_width_const.naked_float f))
    end
    | Naked_int32 (Ok ns) -> begin
      match Int32.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int32 n))
    end
    | Naked_int64 (Ok ns) -> begin
      match Int64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int64 n))
    end
    | Naked_nativeint (Ok ns) -> begin
      match Targetint_32_64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_nativeint n))
    end
    | Value (Ok (Boxed_float ty_naked_float)) -> begin
      match prove_naked_floats env ty_naked_float with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved fs -> (
        match Float.Set.get_singleton fs with
        | None -> try_canonical_simple ()
        | Some f -> Lift (Boxed_float f))
    end
    | Value (Ok (Boxed_int32 ty_naked_int32)) -> begin
      match prove_naked_int32s env ty_naked_int32 with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Int32.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int32 n))
    end
    | Value (Ok (Boxed_int64 ty_naked_int64)) -> begin
      match prove_naked_int64s env ty_naked_int64 with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Int64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int64 n))
    end
    | Value (Ok (Boxed_nativeint ty_naked_nativeint)) -> begin
      match prove_naked_nativeints env ty_naked_nativeint with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Targetint_32_64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_nativeint n))
    end
    | Value Bottom
    | Naked_immediate Bottom
    | Naked_float Bottom
    | Naked_int32 Bottom
    | Naked_int64 Bottom
    | Naked_nativeint Bottom ->
      Invalid
    | _ -> try_canonical_simple ())
