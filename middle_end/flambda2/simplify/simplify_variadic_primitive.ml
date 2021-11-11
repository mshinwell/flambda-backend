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

let simplify_make_block_of_values dacc _prim dbg tag ~shape
    ~(mutable_or_immutable : Mutability.t) args_with_tys ~result_var =
  let denv = DA.denv dacc in
  let args, _arg_tys = List.split args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.invalid (), env_extension, args, dacc
  in
  if List.compare_lengths shape args <> 0
  then
    (* CR mshinwell: improve message *)
    Misc.fatal_errorf
      "GC value_kind indications in [Make_block] don't match up 1:1 with \
       arguments: %a"
      Simple.List.print args;
  (* CR mshinwell: This could probably be done more neatly. *)
  let found_bottom = ref false in
  let fields =
    List.map2
      (fun ((arg : Simple.t), arg_ty) _block_of_values_kind ->
        (* CR mshinwell: There should be a meet against a skeleton type computed
           from [block_of_values_kind]. *)
        if T.is_bottom (DE.typing_env denv) arg_ty then found_bottom := true;
        Simple.pattern_match arg
          ~const:(fun _ -> arg_ty)
          ~name:(fun _ ~coercion:_ -> T.alias_type_of K.value arg))
      args_with_tys shape
  in
  if !found_bottom
  then invalid ()
  else begin
    assert (List.compare_lengths fields shape = 0);
    let term : Named.t =
      Named.create_prim
        (Variadic (Make_block (Values (tag, shape), mutable_or_immutable), args))
        dbg
    in
    let tag = Tag.Scannable.to_tag tag in
    let ty =
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
      | Mutable -> T.any_value
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable term, env_extension, args, dacc
  end

let simplify_make_block_of_floats dacc _prim dbg
    ~(mutable_or_immutable : Mutability.t) args_with_tys ~result_var =
  let denv = DA.denv dacc in
  let args = List.map fst args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.invalid (), env_extension, args, dacc
  in
  (* CR mshinwell: This could probably be done more neatly. *)
  let found_bottom = ref false in
  let fields =
    List.map
      (fun ((arg : Simple.t), arg_ty) ->
        (* CR gbury: we should review all similar pieces of code in the file and
           aim to remove the T.is_bottom checks (kind of like #336 did in the
           simplifier). *)
        if T.is_bottom (DE.typing_env denv) arg_ty then found_bottom := true;
        Simple.pattern_match arg
          ~const:(fun _ -> arg_ty)
          ~name:(fun _ ~coercion:_ -> T.alias_type_of K.naked_float arg))
      args_with_tys
  in
  if !found_bottom
  then invalid ()
  else
    let term : Named.t =
      Named.create_prim
        (Variadic (Make_block (Naked_floats, mutable_or_immutable), args))
        dbg
    in
    let tag = Tag.double_array_tag in
    let ty =
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.naked_float ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.naked_float ~fields
      | Mutable -> T.any_value
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable term, env_extension, args, dacc

let simplify_make_array dacc prim dbg (array_kind : P.Array_kind.t)
    ~mutable_or_immutable args_with_tys ~result_var =
  match args_with_tys with
  | [] ->
    (* Empty arrays are immutable (and always have tag zero). *)
    let named =
      [Flambda.Static_const_or_code.create_static_const Empty_array]
      |> Flambda.Static_const_group.create |> Named.create_static_consts
    in
    let ty =
      T.array_of_length ~element_kind:Unknown
        ~length:(T.this_tagged_immediate Targetint_31_63.zero)
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable named, env_extension, [], dacc
  | _ :: _ -> (
    let args, tys = List.split args_with_tys in
    let invalid () =
      let ty = T.bottom K.value in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Simplified_named.invalid (), env_extension, args, dacc
    in
    let length =
      match Targetint_31_63.Imm.of_int_option (List.length args) with
      | Some ti -> T.this_tagged_immediate (Targetint_31_63.int ti)
      | None -> T.unknown K.value
    in
    let initial_element_type : _ Or_unknown.t =
      match P.Array_kind.element_kind array_kind with
      | Unknown -> Unknown
      | Known kind -> Known (T.unknown (Flambda_kind.With_subkind.kind kind))
    in
    let typing_env = DA.typing_env dacc in
    let found_bottom = ref false in
    let element_type, env_extension =
      List.fold_left
        (fun ((resulting_element_type : _ Or_unknown.t), resulting_env_extension)
             element_type : (_ Or_unknown.t * _) ->
          match resulting_element_type with
          | Unknown -> Known element_type, resulting_env_extension
          | Known resulting_element_type -> (
            match T.meet typing_env resulting_element_type element_type with
            | Bottom ->
              found_bottom := true;
              Known resulting_element_type, resulting_env_extension
            | Ok (resulting_element_type, env_extension) -> (
              match
                TEE.meet typing_env resulting_env_extension env_extension
              with
              | Bottom ->
                found_bottom := true;
                Known resulting_element_type, resulting_env_extension
              | Ok env_extension -> Known resulting_element_type, env_extension)
            ))
        (initial_element_type, TEE.empty)
        tys
    in
    if !found_bottom
    then invalid ()
    else
      let array_and_element_kind : _ Or_bottom.t =
        (* Ditch element_kind. Use Array_kind in the typing.

           Move Array_kind to its own file.

           Make the Float_array_opt_dynamic constructor have: - not specialised
           - specialised to Immediates - specialised to Values - specialised to
           Naked_floats

           For e.g. array get:

           - change Float_array_opt_dynamic (Specialised NF) -> NF

           - make a fresh var

           - read using array kind NF into there

           - then box the fresh var into the result var, which will get type
           Boxed_float(=fresh_var)

           - need to return these 2 primitives instead of one

           - then the subsequent unboxing will collapse to fresh_var. *)
        match array_kind with
        | Immediates -> Ok (array_kind, K.With_subkind.tagged_immediate)
        | Naked_floats -> Ok (array_kind, K.With_subkind.naked_float)
        | Values -> (
          match element_type with
          | Unknown -> Ok (array_kind, K.With_subkind.any_value)
          | Known element_type -> (
            match T.prove_is_a_tagged_immediate typing_env element_type with
            | Proved () ->
              Ok (P.Array_kind.Immediates, K.With_subkind.tagged_immediate)
            | Unknown -> Ok (array_kind, K.With_subkind.any_value)
            | Invalid | Wrong_kind -> (
              (* Boxed floats are not allowed in arrays of array-kind [Values]
                 if the float array optimisation is enabled. *)
              match T.prove_is_a_boxed_float typing_env element_type with
              | Proved () ->
                if Flambda_features.flat_float_array ()
                then Bottom
                else Ok (array_kind, K.With_subkind.any_value)
              | Unknown -> Ok (array_kind, K.With_subkind.any_value)
              | Invalid | Wrong_kind -> Bottom)))
        | Float_array_opt_dynamic -> (
          (* We should never be able to get here if the float array optimisation
             is enabled (see [Lambda_conversions] and recall that the float
             array optimisation is a configure-time setting). *)
          assert (not (Flambda_features.flat_float_array ()));
          match element_type with
          | Unknown -> Ok (array_kind, K.With_subkind.any_value)
          | Known element_type -> (
            (* Attempt to specialise the array to immediates or naked floats. *)
            match T.prove_is_a_tagged_immediate typing_env element_type with
            | Proved () ->
              Ok (P.Array_kind.Immediates, K.With_subkind.tagged_immediate)
            | Unknown -> Ok (array_kind, K.With_subkind.any_value)
            | Invalid | Wrong_kind -> (
              match T.prove_is_a_boxed_float typing_env element_type with
              | Proved () ->
                (* XXX - needs wrapper *)
                Ok (P.Array_kind.Naked_floats, K.With_subkind.naked_float)
              | Unknown -> Ok (array_kind, Or_unknown.Unknown)
              | Invalid | Wrong_kind -> Bottom)))
      in
      match array_and_element_kind with
      | Bottom -> invalid ()
      | Ok (array_kind, element_kind) ->
        let ty = T.array_of_length ~element_kind ~length in
        let env_extension = TEE.one_equation (Name.var result_var) ty in
        let named =
          Named.create_prim
            (Variadic (Make_array (array_kind, mutable_or_immutable), args))
            dbg
        in
        Simplified_named.reachable named, env_extension, args, dacc)

let simplify_variadic_primitive dacc (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let result_var' = Bound_var.var result_var in
  match prim with
  | Make_block (Values (tag, shape), mutable_or_immutable) ->
    simplify_make_block_of_values dacc prim dbg tag ~shape ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
  | Make_block (Naked_floats, mutable_or_immutable) ->
    simplify_make_block_of_floats dacc prim dbg ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
  | Make_array (array_kind, mutable_or_immutable) ->
    simplify_make_array dacc prim dbg array_kind ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
