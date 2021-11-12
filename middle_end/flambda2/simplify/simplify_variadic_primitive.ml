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

let simplify_make_array dacc dbg (array_kind : P.Array_kind.t)
    ~mutable_or_immutable args_with_tys ~result_var =
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
  let element_kind =
    (* Remember that the element subkinds cannot in general be deduced from the
       types of the array members, it must be obtained from the array kind
       annotations that came via [Lambda]. *)
    P.Array_kind.element_kind array_kind
  in
  let initial_element_type : _ Or_unknown.t =
    match element_kind with
    | Known kind -> (
      match K.With_subkind.descr kind with
      | Tagged_immediate -> Known T.any_tagged_immediate
      | Block _ | Float_block _ -> Known T.any_block
      | Any_value -> Known T.any_value
      | Naked_number _ ->
        Known (T.unknown (Flambda_kind.With_subkind.kind kind))
      | Boxed_float -> Known T.any_boxed_float
      | Boxed_int32 -> Known T.any_boxed_int32
      | Boxed_int64 -> Known T.any_boxed_int64
      | Boxed_nativeint -> Known T.any_boxed_nativeint
      | Rec_info ->
        Misc.fatal_error "Array elements cannot have kind [Rec_info]")
    | Unknown -> (
      (* If there was no array kind annotation but there is at least one array
         member, we can use the kind of such member, but not the subkind. *)
      match tys with [] -> Unknown | ty :: _ -> Known (T.unknown_like ty))
  in
  let typing_env = DA.typing_env dacc in
  let found_bottom = ref false in
  let env_extension =
    match initial_element_type with
    | Unknown -> TEE.empty
    | Known initial_element_type ->
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
  then (
    Format.eprintf
      "FOUND BOTTOM: Make_array %a, element kind %a, init element type %a, \
       args %a\n\
       %!"
      P.Array_kind.print array_kind
      (Or_unknown.print K.With_subkind.print)
      element_kind (Or_unknown.print T.print) initial_element_type
      Simple.List.print args;
    invalid ())
  else
    let ty = T.array_of_length ~element_kind ~length in
    let env_extension =
      TEE.add_or_replace_equation env_extension (Name.var result_var) ty
    in
    let named =
      Named.create_prim
        (Variadic (Make_array (array_kind, mutable_or_immutable), args))
        dbg
    in
    Simplified_named.reachable named, env_extension, args, dacc

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
    simplify_make_array dacc dbg array_kind ~mutable_or_immutable args_with_tys
      ~result_var:result_var'
