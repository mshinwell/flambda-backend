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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Simplify_import

let simplify_make_block ~field_kind tag ~shape
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term _dbg
    ~args_with_tys ~result_var =
  let args, _arg_tys = List.split args_with_tys in
  if List.compare_lengths shape args <> 0
  then
    Misc.fatal_errorf
      "Shape in [Make_block] of different length from argument list:@ %a"
      Named.print original_term;
  let result =
    let typing_env = DA.typing_env dacc in
    List.fold_left2
      (fun (env_extension : _ Or_bottom.t) arg arg_kind : _ Or_bottom.t ->
        let open Or_bottom.Let_syntax in
        let<* env_extension = env_extension in
        Simple.pattern_match' arg
          ~var:(fun _ ~coercion:_ : _ Or_bottom.t ->
            let<* _ty, env_extension' =
              T.meet typing_env
                (T.alias_type_of (K.With_subkind.kind arg_kind) arg)
                (T.unknown_with_subkind arg_kind)
            in
            let<+ env_extension =
              T.Typing_env_extension.meet typing_env env_extension
                env_extension'
            in
            env_extension)
          ~const:(fun _ : _ Or_bottom.t -> Ok env_extension)
          ~symbol:(fun _ ~coercion:_ : _ Or_bottom.t -> Ok env_extension))
      (Or_bottom.Ok TEE.empty) args shape
  in
  match result with
  | Bottom -> SPR.create_invalid dacc
  | Ok env_extension ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.map_typing_env denv ~f:(fun typing_env ->
              TE.add_env_extension typing_env env_extension))
    in
    let ty =
      let fields =
        List.map2
          (fun arg kind_with_subkind ->
            T.alias_type_of (K.With_subkind.kind kind_with_subkind) arg)
          args shape
      in
      let alloc_mode = Or_unknown.Known alloc_mode in
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind alloc_mode ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind alloc_mode ~fields
      | Mutable -> T.mutable_block alloc_mode
    in
    let dacc = DA.add_variable dacc result_var ty in
    (* CR mshinwell: here and in the next function, should we be adding CSE
       equations, like we do for unboxing boxed numbers? (see
       Simplify_unary_primitive) *)
    SPR.create original_term ~try_reify:true dacc

let simplify_make_block_of_floats ~mutable_or_immutable alloc_mode dacc
    ~original_term dbg ~args_with_tys ~result_var =
  let shape = List.map (fun _ -> K.With_subkind.naked_float) args_with_tys in
  simplify_make_block ~field_kind:K.naked_float Tag.double_array_tag ~shape
    ~mutable_or_immutable alloc_mode dacc ~original_term dbg ~args_with_tys
    ~result_var

let simplify_make_array (array_kind : P.Array_kind.t) ~mutable_or_immutable
    alloc_mode dacc ~original_term:_ dbg ~args_with_tys ~result_var =
  let args, tys = List.split args_with_tys in
  let length =
    match Targetint_31_63.Imm.of_int_option (List.length args) with
    | Some ti -> T.this_tagged_immediate (Targetint_31_63.int ti)
    | None -> T.unknown K.value
  in
  let element_kind =
    (* Remember that the element subkinds cannot in general be deduced from the
       types of the array members, it must be obtained from the array kind
       annotations that came via [Lambda]. *)
    P.Array_kind.element_kind array_kind
  in
  let initial_element_type = T.unknown_with_subkind element_kind in
  let typing_env = DA.typing_env dacc in
  let found_bottom = ref false in
  let env_extension =
    List.fold_left
      (fun resulting_env_extension element_type ->
        match T.meet typing_env initial_element_type element_type with
        | Bottom ->
          found_bottom := true;
          resulting_env_extension
        | Ok (_, env_extension) -> (
          match TEE.meet typing_env resulting_env_extension env_extension with
          | Bottom ->
            found_bottom := true;
            resulting_env_extension
          | Ok env_extension -> env_extension))
      TEE.empty tys
  in
  if !found_bottom
  then Simplify_primitive_result.create_invalid dacc
  else
    let ty = T.array_of_length ~element_kind:(Known element_kind) ~length in
    let named =
      Named.create_prim
        (Variadic
           (Make_array (array_kind, mutable_or_immutable, alloc_mode), args))
        dbg
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.add_variable_and_extend_typing_environment denv result_var ty
            env_extension)
    in
    Simplify_primitive_result.create named ~try_reify:true dacc

let simplify_variadic_primitive dacc original_prim (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Make_block (Values (tag, shape), mutable_or_immutable, alloc_mode) ->
      let tag = Tag.Scannable.to_tag tag in
      simplify_make_block ~field_kind:K.value tag ~shape ~mutable_or_immutable
        alloc_mode
    | Make_block (Naked_floats, mutable_or_immutable, alloc_mode) ->
      simplify_make_block_of_floats ~mutable_or_immutable alloc_mode
    | Make_array (array_kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_array array_kind ~mutable_or_immutable alloc_mode
  in
  simplifier dacc ~original_term dbg ~args_with_tys ~result_var
