(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module Meet_env = Typing_env.Meet_env
module Join_env = Typing_env.Join_env
module ET = Expand_head.Expanded_type
module K = Flambda_kind
module MTC = More_type_creators
module TD = Type_descr
module TG = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level

let ( let* ) x f = Or_bottom.bind x ~f

let ( let+ ) x f = Or_bottom.map x ~f

let add_equation (simple : Simple.t) ty_of_simple env_extension =
  match Simple.must_be_name simple with
  (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
  | Some (name, coercion_from_name_to_simple) ->
    let coercion_from_simple_to_name =
      Coercion.inverse coercion_from_name_to_simple
    in
    let ty_of_name =
      match TG.apply_coercion ty_of_simple coercion_from_simple_to_name with
      | Ok ty -> ty
      | Bottom -> MTC.bottom_like ty_of_simple
    in
    TEE.add_or_replace_equation env_extension name ty_of_name
  | None -> env_extension

let all_aliases_of env simple_opt ~in_env =
  match simple_opt with
  | None -> Aliases.Alias_set.empty
  | Some simple ->
    let simples = TE.aliases_of_simple_allowable_in_types env simple in
    (* Format.eprintf "Aliases of %a are: %a\n%!" Simple.print simple
       Simple.Set.print simples; *)
    Aliases.Alias_set.filter
      ~f:(fun simple -> Typing_env.mem_simple in_env simple)
      simples

type meet_or_join_head_or_unknown_or_bottom_result =
  | Left_head_unchanged
  | Right_head_unchanged
  | New_head of ET.t * TEE.t

exception Bottom_meet

let meet_head_of_kind_naked_float _env t1 t2 : _ Or_bottom.t =
  let t = Float.Set.inter t1 t2 in
  if Float.Set.is_empty t then Bottom else Ok (t, TEE.empty)

let meet_head_of_kind_naked_int32 _env t1 t2 : _ Or_bottom.t =
  let t = Int32.Set.inter t1 t2 in
  if Int32.Set.is_empty t then Bottom else Ok (t, TEE.empty)

let meet_head_of_kind_naked_int64 _env t1 t2 : _ Or_bottom.t =
  let t = Int64.Set.inter t1 t2 in
  if Int64.Set.is_empty t then Bottom else Ok (t, TEE.empty)

let meet_head_of_kind_naked_nativeint _env t1 t2 : _ Or_bottom.t =
  let t = Targetint_32_64.Set.inter t1 t2 in
  if Targetint_32_64.Set.is_empty t then Bottom else Ok (t, TEE.empty)

let meet_head_of_kind_rec_info _env t1 t2 : _ Or_bottom.t =
  (* CR-someday lmaurer: This could be doing things like discovering two depth
     variables are equal *)
  if Rec_info_expr.equal t1 t2 then Ok (t1, TEE.empty) else Bottom

let meet_unknown meet_contents ~contents_is_bottom env
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t) :
    (_ Or_unknown.t * TEE.t) Or_bottom.t =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> Ok (Unknown, TEE.empty)
  (* CR mshinwell: Think about the next two cases more *)
  | Known contents, _ when contents_is_bottom contents -> Bottom
  | _, Known contents when contents_is_bottom contents -> Bottom
  | _, Unknown -> Ok (or_unknown1, TEE.empty)
  | Unknown, _ -> Ok (or_unknown2, TEE.empty)
  | Known contents1, Known contents2 -> (
    let result =
      Or_bottom.map (meet_contents env contents1 contents2)
        ~f:(fun (contents, env_extension) ->
          Or_unknown.Known contents, env_extension)
    in
    match result with
    | Bottom | Ok (Unknown, _) -> result
    | Ok (Known contents, _env_extension) ->
      (* XXX Why isn't [meet_contents] returning bottom? *)
      if contents_is_bottom contents then Bottom else result)

let join_unknown join_contents (env : Join_env.t) (or_unknown1 : _ Or_unknown.t)
    (or_unknown2 : _ Or_unknown.t) : _ Or_unknown.t =
  match or_unknown1, or_unknown2 with
  | _, Unknown | Unknown, _ -> Unknown
  | Known contents1, Known contents2 -> join_contents env contents1 contents2

let rec meet_variant env ~(blocks1 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms1 : TG.t Or_unknown.t)
    ~(blocks2 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms2 : TG.t Or_unknown.t) :
    (TG.Row_like_for_blocks.t Or_unknown.t * TG.t Or_unknown.t * TEE.t)
    Or_bottom.t =
  let blocks =
    meet_unknown meet_row_like_for_blocks
      ~contents_is_bottom:TG.Row_like_for_blocks.is_bottom env blocks1 blocks2
  in
  let blocks : _ Or_bottom.t =
    (* XXX Clean this up *)
    match blocks with
    | Bottom | Ok (Or_unknown.Unknown, _) -> blocks
    | Ok (Or_unknown.Known blocks', _) ->
      if TG.Row_like_for_blocks.is_bottom blocks' then Bottom else blocks
  in
  let imms =
    meet_unknown meet ~contents_is_bottom:TG.is_obviously_bottom env imms1 imms2
  in
  let imms : _ Or_bottom.t =
    match imms with
    | Bottom | Ok (Or_unknown.Unknown, _) -> imms
    | Ok (Or_unknown.Known imms', _) ->
      if TG.is_obviously_bottom imms' then Bottom else imms
  in
  match blocks, imms with
  | Bottom, Bottom -> Bottom
  | Ok (blocks, env_extension), Bottom ->
    let immediates : _ Or_unknown.t = Known TG.bottom_naked_immediate in
    Ok (blocks, immediates, env_extension)
  | Bottom, Ok (immediates, env_extension) ->
    let blocks : _ Or_unknown.t = Known TG.Row_like_for_blocks.bottom in
    Ok (blocks, immediates, env_extension)
  | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
    begin
      match (blocks : _ Or_unknown.t) with
      | Unknown -> ()
      | Known blocks -> assert (not (TG.Row_like_for_blocks.is_bottom blocks))
    end;
    begin
      match (immediates : _ Or_unknown.t) with
      | Unknown -> ()
      | Known imms -> assert (not (TG.is_obviously_bottom imms))
    end;
    let env_extension =
      let env = Meet_env.env env in
      let join_env = Join_env.create env ~left_env:env ~right_env:env in
      join_env_extension join_env env_extension1 env_extension2
    in
    Ok (blocks, immediates, env_extension)

and meet_closures_entry (env : Meet_env.t)
    ({ function_types = function_types1;
       closure_types = closure_types1;
       closure_var_types = closure_var_types1
     } :
      TG.Closures_entry.t)
    ({ function_types = function_types2;
       closure_types = closure_types2;
       closure_var_types = closure_var_types2
     } :
      TG.Closures_entry.t) : (TG.Closures_entry.t * TEE.t) Or_bottom.t =
  let any_bottom = ref false in
  let env_extensions = ref TEE.empty in
  let function_types =
    Closure_id.Map.merge
      (fun _closure_id func_type1 func_type2 ->
        match func_type1, func_type2 with
        | None, None -> None
        | Some func_type, None | None, Some func_type -> Some func_type
        | Some func_type1, Some func_type2 -> (
          match meet_function_type env func_type1 func_type2 with
          | Bottom ->
            any_bottom := true;
            None
          | Ok (func_type, env_extension) ->
            begin
              match meet_env_extension env !env_extensions env_extension with
              | Bottom -> any_bottom := true
              | Ok env_extension -> env_extensions := env_extension
            end;
            Some func_type))
      function_types1 function_types2
  in
  if !any_bottom
  then Bottom
  else
    let* closure_types, env_extension1 =
      meet_product_closure_id_indexed env closure_types1 closure_types2
    in
    let* closure_var_types, env_extension2 =
      meet_product_var_within_closure_indexed env closure_var_types1
        closure_var_types2
    in
    let closures_entry =
      TG.Closures_entry.create ~function_types ~closure_types ~closure_var_types
    in
    let* env_extension =
      meet_env_extension env !env_extensions env_extension1
    in
    let* env_extension = meet_env_extension env env_extension env_extension2 in
    Ok (closures_entry, env_extension)

and meet_function_type (env : Meet_env.t)
    (func_type1 : TG.Function_type.t Or_unknown_or_bottom.t)
    (func_type2 : TG.Function_type.t Or_unknown_or_bottom.t) :
    (TG.Function_type.t Or_unknown_or_bottom.t * TEE.t) Or_bottom.t =
  match func_type1, func_type2 with
  | Bottom, _ | _, Bottom -> Ok (Bottom, TEE.empty)
  | Unknown, t | t, Unknown -> Ok (t, TEE.empty)
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) ->
    let typing_env = Meet_env.env env in
    let* code_id =
      Code_age_relation.meet
        (TE.code_age_relation typing_env)
        ~resolver:(TE.code_age_relation_resolver typing_env)
        code_id1 code_id2
    in
    (* It's possible that [code_id] corresponds to [Deleted] code. In that case,
       any attempt to inline will fail, as the code will not be found in the
       simplifier's environment -- see
       [Simplify_apply_expr.simplify_direct_function_call]. *)
    let* rec_info, extension = meet env rec_info1 rec_info2 in
    let func_type = TG.Function_type.create code_id ~rec_info in
    Ok (Or_unknown_or_bottom.Ok func_type, extension)

and meet_row_like (meet_env : Meet_env.t) t1 t2 : (TG.t * TEE.t) Or_bottom.t =
  let ({ known_tags = known1; other_tags = other1 } : t) = t1 in
  let ({ known_tags = known2; other_tags = other2 } : t) = t2 in
  let env_extension = ref None in
  let need_join =
    (* The returned env_extension is the join of the env_extension produced by
       each non bottom cases. Therefore there is some loss of precision in that
       case and we need to store the one produced for each tag. But when only
       one tag is kept it would be wasteful (but correct) to store it.

       We consider that the result of the meet between t1 and t2 will have only
       one tag when t1 (or t2) has exactly one tag (one that and no 'other'
       cases).

       This is an overapproximation because the result could have only one tag
       for instance if

       t1 = [Tag 1 | Tag 2] and t2 = [Tag 2 | Tag 3], or if

       t1 = [Tag 1 | Tag 2] and t2 = [Tag 1 | Tag 2]

       but the meet between some combinations result in a bottom. *)
    match
      other1, Tag.Map.get_singleton known1, other2, Tag.Map.get_singleton known2
    with
    | Bottom, Some _, _, _ -> false
    | _, _, Bottom, Some _ -> false
    | _ -> true
  in
  let env = Meet_env.env meet_env in
  let join_env = Join_env.create env ~left_env:env ~right_env:env in
  let join_env_extension ext =
    match !env_extension with
    | None -> env_extension := Some ext
    | Some ext2 ->
      assert need_join;
      env_extension := Some (join_env_extension join_env ext2 ext)
  in
  let meet_index i1 i2 : index Or_bottom.t =
    match i1, i2 with
    | Known i1', Known i2' -> if Index.equal i1' i2' then Ok i1 else Bottom
    | Known known, At_least at_least | At_least at_least, Known known ->
      if Index.subset at_least known
      then
        (* [at_least] is included in [known] hence [Known known] is included in
           [At_least at_least], hence [Known known] \inter [At_least at_least] =
           [Known known] *)
        Ok (Known known)
      else Bottom
    | At_least i1', At_least i2' -> Ok (At_least (Index.union i1' i2'))
  in
  let meet_case case1 case2 =
    match meet_index case1.index case2.index with
    | Bottom -> None
    | Ok index -> (
      match Maps_to.meet meet_env case1.maps_to case2.maps_to with
      | Bottom -> None
      | Ok (maps_to, env_extension') -> (
        match
          meet_env_extension meet_env case1.env_extension case2.env_extension
        with
        | Bottom -> None
        | Ok env_extension'' -> (
          match meet_env_extension meet_env env_extension' env_extension'' with
          | Bottom -> None
          | Ok env_extension ->
            join_env_extension env_extension;
            let env_extension =
              if need_join then env_extension else TEE.empty ()
            in
            Some { maps_to; index; env_extension })))
  in
  let meet_knowns_tags case1 case2 : case option =
    match case1, case2 with
    | None, None -> None
    | Some case1, None -> begin
      match other2 with
      | Bottom -> None
      | Ok other_case -> meet_case case1 other_case
    end
    | None, Some case2 -> begin
      match other1 with
      | Bottom -> None
      | Ok other_case -> meet_case other_case case2
    end
    | Some case1, Some case2 -> meet_case case1 case2
  in
  let known_tags =
    Tag.Map.merge
      (fun _tag case1 case2 -> meet_knowns_tags case1 case2)
      known1 known2
  in
  let other_tags : case Or_bottom.t =
    match other1, other2 with
    | Bottom, _ | _, Bottom -> Bottom
    | Ok other1, Ok other2 -> (
      match meet_case other1 other2 with None -> Bottom | Some r -> Ok r)
  in
  let result = { known_tags; other_tags } in
  if is_bottom result
  then Bottom
  else
    let env_extension =
      match !env_extension with
      | None -> assert false (* This should be bottom *)
      | Some ext -> ext
    in
    Ok (result, env_extension)

and meet_row_like_for_blocks env by_tag1 by_tag2 = assert false

and meet_row_like_for_closures env by_closure_id1 by_closure_id2 = assert false

and meet_generic_product :
      'key 'key_map.
      Meet_env.t ->
      components_by_index1:'key_map ->
      components_by_index2:'key_map ->
      union:
        (('key -> TG.t -> TG.t -> TG.t option) ->
        'key_map ->
        'key_map ->
        'key_map) ->
      ('key_map * TEE.t) Or_bottom.t =
 fun env ~components_by_index1 ~components_by_index2 ~union ->
  let any_bottom = ref false in
  let env_extension = ref TEE.empty in
  let components_by_index =
    union
      (fun _index ty1 ty2 ->
        match meet env ty1 ty2 with
        | Ok (ty, env_extension') -> begin
          match meet_env_extension env !env_extension env_extension' with
          | Bottom ->
            any_bottom := true;
            Some (MTC.bottom_like ty1)
          | Ok extension ->
            env_extension := extension;
            Some ty
        end
        | Bottom ->
          any_bottom := true;
          Some (MTC.bottom_like ty1))
      components_by_index1 components_by_index2
  in
  if !any_bottom then Bottom else Ok (components_by_index, !env_extension)

and meet_product_closure_id_indexed env
    ({ closure_id_components_by_index = components_by_index1 } :
      TG.Product.Closure_id_indexed.t)
    ({ closure_id_components_by_index = components_by_index2 } :
      TG.Product.Closure_id_indexed.t) :
    (TG.Product.Closure_id_indexed.t * TEE.t) Or_bottom.t =
  Or_bottom.map
    (meet_generic_product env ~components_by_index1 ~components_by_index2
       ~union:Closure_id.Map.union)
    ~f:(fun (components_by_index, env_extension) ->
      TG.Product.Closure_id_indexed.create components_by_index, env_extension)

and meet_product_var_within_closure_indexed env
    ({ var_within_closure_components_by_index = components_by_index1 } :
      TG.Product.Var_within_closure_indexed.t)
    ({ var_within_closure_components_by_index = components_by_index2 } :
      TG.Product.Var_within_closure_indexed.t) :
    (TG.Product.Var_within_closure_indexed.t * TEE.t) Or_bottom.t =
  Or_bottom.map
    (meet_generic_product env ~components_by_index1 ~components_by_index2
       ~union:Var_within_closure.Map.union)
    ~f:(fun (components_by_index, env_extension) ->
      ( TG.Product.Var_within_closure_indexed.create components_by_index,
        env_extension ))

and meet_int_indexed_product env (prod1 : TG.Product.Int_indexed.t)
    (prod2 : TG.Product.Int_indexed.t) : _ Or_bottom.t =
  if not (K.equal prod1.kind prod2.kind)
  then
    Misc.fatal_errorf
      "meet_int_indexed_product between mismatching kinds %a and %a@." K.print
      prod1.kind K.print prod2.kind;
  let fields1 = prod1.fields in
  let fields2 = prod2.fields in
  let any_bottom = ref false in
  let env_extension = ref TEE.empty in
  let length = max (Array.length fields1) (Array.length fields2) in
  let fields =
    Array.init length (fun index ->
        let get_opt fields =
          if index >= Array.length fields then None else Some fields.(index)
        in
        match get_opt fields1, get_opt fields2 with
        | None, None -> assert false
        | Some t, None | None, Some t -> t
        | Some ty1, Some ty2 -> begin
          match meet env ty1 ty2 with
          | Ok (ty, env_extension') -> begin
            match meet_env_extension env !env_extension env_extension' with
            | Bottom ->
              any_bottom := true;
              MTC.bottom_like ty1
            | Ok extension ->
              env_extension := extension;
              ty
          end
          | Bottom ->
            any_bottom := true;
            MTC.bottom_like ty1
        end)
  in
  if !any_bottom
  then Bottom
  else
    Ok
      ( TG.Product.Int_indexed.create_from_array prod1.kind fields,
        !env_extension )

and meet_head_of_kind_value env (head1 : TG.head_of_kind_value)
    (head2 : TG.head_of_kind_value) : _ Or_bottom.t =
  match head1, head2 with
  | ( Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1 },
      Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2 } )
    ->
    Or_bottom.map (meet_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates, env_extension) ->
        (* Uniqueness tracks whether duplication/lifting is allowed. It must
           always be propagated, both for meet and join. *)
        let is_unique = is_unique1 || is_unique2 in
        ( TG.Head_of_kind_value.create_variant ~is_unique ~blocks ~immediates,
          env_extension ))
  | Boxed_float n1, Boxed_float n2 ->
    Or_bottom.map (meet env n1 n2) ~f:(fun (n, env_extension) ->
        TG.Head_of_kind_value.create_boxed_float n, env_extension)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_bottom.map (meet env n1 n2) ~f:(fun (n, env_extension) ->
        TG.Head_of_kind_value.create_boxed_int32 n, env_extension)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_bottom.map (meet env n1 n2) ~f:(fun (n, env_extension) ->
        TG.Head_of_kind_value.create_boxed_int64 n, env_extension)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_bottom.map (meet env n1 n2) ~f:(fun (n, env_extension) ->
        TG.Head_of_kind_value.create_boxed_nativeint n, env_extension)
  | ( Closures { by_closure_id = by_closure_id1 },
      Closures { by_closure_id = by_closure_id2 } ) ->
    Or_bottom.map (meet_row_like_for_closures env by_closure_id1 by_closure_id2)
      ~f:(fun (by_closure_id, env_extension) ->
        TG.Head_of_kind_value.create_closures by_closure_id, env_extension)
  | String strs1, String strs2 ->
    let strs = String_info.Set.inter strs1 strs2 in
    if String_info.Set.is_empty strs
    then Bottom
    else Or_bottom.Ok (TG.Head_of_kind_value.create_string strs, TEE.empty)
  | Array { length = length1 }, Array { length = length2 } ->
    Or_bottom.map (meet env length1 length2) ~f:(fun (length, env_extension) ->
        TG.Head_of_kind_value.create_array ~length, env_extension)
  | ( ( Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ),
      _ ) ->
    (* CR vlaviron: This assumes that all the different constructors are
       incompatible. This could break very hard for users of Obj. *)
    Bottom

and meet_head_of_kind_naked_immediate env (t1 : TG.head_of_kind_naked_immediate)
    (t2 : TG.head_of_kind_naked_immediate) :
    (TG.head_of_kind_naked_immediate * TEE.t) Or_bottom.t =
  let module I = Targetint_31_63 in
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    let is = I.Set.inter is1 is2 in
    if I.Set.is_empty is
    then Bottom
    else
      Ok (TG.Head_of_kind_naked_immediate.create_naked_immediates is, TEE.empty)
  | Is_int ty1, Is_int ty2 ->
    let+ ty, env_extension = meet env ty1 ty2 in
    TG.Head_of_kind_naked_immediate.create_is_int ty, env_extension
  | Get_tag ty1, Get_tag ty2 ->
    let+ ty, env_extension = meet env ty1 ty2 in
    TG.Head_of_kind_naked_immediate.create_get_tag ty, env_extension
  | Is_int ty, Naked_immediates is_int | Naked_immediates is_int, Is_int ty -> (
    match I.Set.elements is_int with
    | [] -> Bottom
    | [is_int] -> (
      let shape =
        if I.equal is_int I.zero
        then Some MTC.any_block
        else if I.equal is_int I.one
        then Some MTC.any_tagged_immediate
        else None
      in
      match shape with
      | Some shape ->
        Or_bottom.map (meet env ty shape) ~f:(fun (ty, env_extension) ->
            TG.Head_of_kind_naked_immediate.create_is_int ty, env_extension)
      | None -> Bottom)
    | _ :: _ :: _ ->
      (* Note: we're potentially losing precision because the set could end up
         not containing either 0 or 1 or both, but this should be uncommon. *)
      Ok (TG.Head_of_kind_naked_immediate.create_is_int ty, TEE.empty))
  | Get_tag ty, Naked_immediates tags | Naked_immediates tags, Get_tag ty -> (
    let tags =
      I.Set.fold
        (fun tag tags ->
          match Tag.create_from_targetint tag with
          | Some tag -> Tag.Set.add tag tags
          | None -> tags
          (* No blocks exist with this tag *))
        tags Tag.Set.empty
    in
    match MTC.blocks_with_these_tags tags with
    | Known shape ->
      let+ ty, env_extension = meet env ty shape in
      TG.Head_of_kind_naked_immediate.create_get_tag ty, env_extension
    | Unknown ->
      Ok (TG.Head_of_kind_naked_immediate.create_get_tag ty, TEE.empty))
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
    (* We can't return Bottom, as it would be unsound, so we need to either do
       the actual meet with Naked_immediates, or just give up and return one of
       the arguments. *)
    Ok (t1, TEE.empty)

and meet_expanded_head env (expanded1 : ET.t) (expanded2 : ET.t) :
    meet_or_join_head_or_unknown_or_bottom_result =
  match ET.descr expanded1, ET.descr expanded2 with
  | _, Unknown -> Left_head_unchanged
  | Unknown, _ -> Right_head_unchanged
  | Bottom, _ -> Left_head_unchanged
  | _, Bottom -> Right_head_unchanged
  | Ok descr1, Ok descr2 -> (
    let expanded_or_bottom =
      match descr1, descr2 with
      | Value head1, Value head2 ->
        let+ head, env_extension = meet_head_of_kind_value env head1 head2 in
        ET.create_value head, env_extension
      | Naked_immediate head1, Naked_immediate head2 ->
        let+ head, env_extension =
          meet_head_of_kind_naked_immediate env head1 head2
        in
        ET.create_naked_immediate head, env_extension
      | Naked_float head1, Naked_float head2 ->
        let+ head, env_extension =
          meet_head_of_kind_naked_float env head1 head2
        in
        ET.create_naked_float head, env_extension
      | Naked_int32 head1, Naked_int32 head2 ->
        let+ head, env_extension =
          meet_head_of_kind_naked_int32 env head1 head2
        in
        ET.create_naked_int32 head, env_extension
      | Naked_int64 head1, Naked_int64 head2 ->
        let+ head, env_extension =
          meet_head_of_kind_naked_int64 env head1 head2
        in
        ET.create_naked_int64 head, env_extension
      | Naked_nativeint head1, Naked_nativeint head2 ->
        let+ head, env_extension =
          meet_head_of_kind_naked_nativeint env head1 head2
        in
        ET.create_naked_nativeint head, env_extension
      | Rec_info head1, Rec_info head2 ->
        let+ head, env_extension = meet_head_of_kind_rec_info env head1 head2 in
        ET.create_rec_info head, env_extension
      | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ),
          _ ) ->
        assert false
    in
    match expanded_or_bottom with
    | Ok (expanded, env_extension) -> New_head (expanded, env_extension)
    | Bottom -> New_head (ET.bottom_like expanded1, TEE.empty))

and meet0 env (t1 : TG.t) (t2 : TG.t) : TG.t * TEE.t =
  if not (K.equal (TG.kind t1) (TG.kind t2))
  then
    Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a" TG.print t1
      TG.print t2;
  let kind = TG.kind t1 in
  let typing_env = Meet_env.env env in
  let simple1, expanded1, simple2, expanded2 =
    Expand_head.get_canonical_simples_and_expand_heads ~left_env:typing_env
      ~left_ty:t1 ~right_env:typing_env ~right_ty:t2
  in
  match simple1 with
  | None -> begin
    match simple2 with
    | None -> (
      match meet_expanded_head env expanded1 expanded2 with
      | Left_head_unchanged -> t1, TEE.empty
      | Right_head_unchanged -> t2, TEE.empty
      | New_head (expanded, env_extension) -> ET.to_type expanded, env_extension
      )
    | Some simple2 -> (
      match meet_expanded_head env expanded1 expanded2 with
      | Left_head_unchanged ->
        let env_extension =
          add_equation simple2 (ET.to_type expanded1) TEE.empty
        in
        TG.alias_type_of kind simple2, env_extension
      | Right_head_unchanged -> TG.alias_type_of kind simple2, TEE.empty
      | New_head (expanded, env_extension) ->
        (* It makes things easier (to check if the result of [meet] was bottom)
           to not return "=simple" in the bottom case. This is ok because no
           constraint is being dropped; the type cannot be refined any further.
           Same below. (Note that any bottom equation added to [env_extension]
           for [simple2] is of no consequence since the extension will be
           ignored; see the [meet] function below.) *)
        let ty =
          if ET.is_bottom expanded
          then MTC.bottom kind
          else TG.alias_type_of kind simple2
        in
        let env_extension =
          add_equation simple2 (ET.to_type expanded) env_extension
        in
        ty, env_extension)
  end
  | Some simple1 -> (
    match simple2 with
    | None -> (
      match meet_expanded_head env expanded1 expanded2 with
      | Left_head_unchanged -> TG.alias_type_of kind simple1, TEE.empty
      | Right_head_unchanged ->
        let env_extension =
          add_equation simple1 (ET.to_type expanded2) TEE.empty
        in
        TG.alias_type_of kind simple1, env_extension
      | New_head (expanded, env_extension) ->
        let ty =
          if ET.is_bottom expanded
          then MTC.bottom kind
          else TG.alias_type_of kind simple1
        in
        let env_extension =
          env_extension |> add_equation simple1 (ET.to_type expanded)
        in
        ty, env_extension)
    | Some simple2 ->
      if Simple.equal simple1 simple2
         || Meet_env.already_meeting env simple1 simple2
      then
        (* This produces "=simple" for the output rather than a type that might
           need transformation back from an expanded head (as would happen if we
           used the next case). *)
        TG.alias_type_of kind simple1, TEE.empty
      else (
        assert (not (Simple.equal simple1 simple2));
        let env = Meet_env.now_meeting env simple1 simple2 in
        (* In the following cases we may generate equations "pointing the wrong
           way", for example "y : =x" when [y] is the canonical element. This
           doesn't matter, however, because [Typing_env] sorts this out when
           adding equations into an environment. *)
        (* CR mshinwell: May be able to improve efficiency by not doing [meet]
           again (via [TE.add_env_extension]) if we tried here to emit the
           equations the correct way around *)
        match meet_expanded_head env expanded1 expanded2 with
        | Left_head_unchanged ->
          let env_extension =
            add_equation simple2 (TG.alias_type_of kind simple1) TEE.empty
          in
          TG.alias_type_of kind simple1, env_extension
        | Right_head_unchanged ->
          let env_extension =
            add_equation simple1 (TG.alias_type_of kind simple2) TEE.empty
          in
          TG.alias_type_of kind simple2, env_extension
        | New_head (expanded, env_extension) ->
          let env_extension =
            env_extension
            |> add_equation simple1 (ET.to_type expanded)
            |> add_equation simple2 (TG.alias_type_of kind simple1)
          in
          let ty =
            if ET.is_bottom expanded
            then MTC.bottom kind
            else TG.alias_type_of kind simple1
          in
          ty, env_extension))

and meet env (t1 : TG.t) (t2 : TG.t) : (TG.t * TEE.t) Or_bottom.t =
  let t, env_extension = meet0 env t1 t2 in
  if TG.is_obviously_bottom t then Bottom else Ok (t, env_extension)

and meet_env_extension0 env (ext1 : TEE.t) (ext2 : TEE.t) extra_extensions :
    TEE.t =
  (* A symmetrical meet would be hard to implement, as the inner meets can
     produce extra extensions that need to be merged with the result.

     To get around this, we'll suppose that [t2] is smaller than [t1] and add
     equations from [t2] to [t1], along with all extra equations *)
  let equations, extra_extensions =
    Name.Map.fold
      (fun name ty (eqs, extra_extensions) ->
        match Name.Map.find_opt name eqs with
        | None ->
          MTC.check_equation name ty;
          Name.Map.add name ty eqs, extra_extensions
        | Some ty0 -> begin
          match meet env ty0 ty with
          | Bottom -> raise Bottom_meet
          | Ok (ty, new_ext) ->
            MTC.check_equation name ty;
            Name.Map.add (*replace*) name ty eqs, new_ext :: extra_extensions
        end)
      (TEE.to_map ext2)
      (TEE.to_map ext1, extra_extensions)
  in
  let ext = TEE.from_map equations in
  match extra_extensions with
  | [] -> ext
  | new_ext :: extra_extensions ->
    (* CR vlaviron: It's a bad idea to drop the extensions in the general case,
       but since we lack the property that the new extensions are stricter than
       the existing ones we can get into an infinite loop here (see
       flambdatest/unit_test/extension_meet.ml, function test_double_recursion
       for an example).

       This is very uncommon though (it needs recursive types involving at least
       three different names), so for now we still do the meet
       systematically. *)
    meet_env_extension0 env ext new_ext extra_extensions

and meet_env_extension (env : Meet_env.t) t1 t2 : TEE.t Or_bottom.t =
  try Ok (meet_env_extension0 env t1 t2 []) with Bottom_meet -> Bottom

and join_head_of_kind_naked_float _env t1 t2 : _ Or_unknown.t =
  Known (Float.Set.union t1 t2)

and join_head_of_kind_naked_int32 _env t1 t2 : _ Or_unknown.t =
  Known (Int32.Set.union t1 t2)

and join_head_of_kind_naked_int64 _env t1 t2 : _ Or_unknown.t =
  Known (Int64.Set.union t1 t2)

and join_head_of_kind_naked_nativeint _env t1 t2 : _ Or_unknown.t =
  Known (Targetint_32_64.Set.union t1 t2)

and join_head_of_kind_rec_info _env t1 t2 : _ Or_unknown.t =
  if Rec_info_expr.equal t1 t2 then Known t1 else Unknown

and join_variant env ~(blocks1 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms1 : TG.t Or_unknown.t)
    ~(blocks2 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms2 : TG.t Or_unknown.t) :
    (TG.Row_like_for_blocks.t Or_unknown.t * TG.t Or_unknown.t) Or_unknown.t =
  let blocks_join env b1 b2 : _ Or_unknown.t =
    Known (join_row_like_for_blocks env b1 b2)
  in
  let blocks = join_unknown blocks_join env blocks1 blocks2 in
  let imms = join_unknown (join ?bound_name:None) env imms1 imms2 in
  match blocks, imms with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown | Unknown, Known _ | Known _, Known _ ->
    Known (blocks, imms)

(* Note that unlike the [join] function on types, for structures (closures
   entry, row-like, etc.) the return type is [t] (and not [t Or_unknown.t]).
   This simplifies some parts of the code a bit that cannot handle the Unknown
   case gracefully. All join functions for structures can handle [Unknown]
   results from generic [join]s without needing to propagate them. *)
and join_closures_entry env
    ({ function_types = function_types1;
       closure_types = closure_types1;
       closure_var_types = closure_var_types1
     } :
      TG.Closures_entry.t)
    ({ function_types = function_types2;
       closure_types = closure_types2;
       closure_var_types = closure_var_types2
     } :
      TG.Closures_entry.t) : TG.Closures_entry.t =
  let function_types =
    Closure_id.Map.merge
      (fun _closure_id func_type1 func_type2 ->
        match func_type1, func_type2 with
        | None, None
        (* CR mshinwell: Are these next two cases right? Don't we need to do the
           equivalent of make_suitable_for_environment? *)
        | Some _, None
        | None, Some _ ->
          None
        | Some func_type1, Some func_type2 ->
          Some (join_function_type env func_type1 func_type2))
      function_types1 function_types2
  in
  let closure_types =
    join_closure_id_indexed_product env closure_types1 closure_types2
  in
  let closure_var_types =
    join_var_within_closure_indexed_product env closure_var_types1
      closure_var_types2
  in
  TG.Closures_entry.create ~function_types ~closure_types ~closure_var_types

and join_function_type (env : Join_env.t)
    (func_type1 : TG.Function_type.t Or_unknown_or_bottom.t)
    (func_type2 : TG.Function_type.t Or_unknown_or_bottom.t) :
    TG.Function_type.t Or_unknown_or_bottom.t =
  match func_type1, func_type2 with
  | Bottom, func_type | func_type, Bottom -> func_type
  | Unknown, _ | _, Unknown -> Unknown
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) -> (
    let target_typing_env = Join_env.target_join_env env in
    match
      Code_age_relation.join
        ~target_t:(TE.code_age_relation target_typing_env)
        ~resolver:(TE.code_age_relation_resolver target_typing_env)
        (TE.code_age_relation (Join_env.left_join_env env))
        (TE.code_age_relation (Join_env.right_join_env env))
        code_id1 code_id2
    with
    | Unknown -> Unknown
    | Known code_id -> (
      match join env rec_info1 rec_info2 with
      | Known rec_info -> Ok (TG.Function_type.create code_id ~rec_info)
      | Unknown -> Unknown))

and join_row_like (env : Join_env.t) row_like1 row_like2 =
  let ({ known_tags = known1; other_tags = other1 } : t) = t1 in
  let ({ known_tags = known2; other_tags = other2 } : t) = t2 in
  let join_index i1 i2 : index =
    match i1, i2 with
    | Known i1', Known i2' ->
      if Index.equal i1' i2'
      then i1
      else
        (* We can't represent exactly the union, This is the best
           approximation *)
        At_least (Index.inter i1' i2')
    | Known i1', At_least i2'
    | At_least i1', Known i2'
    | At_least i1', At_least i2' ->
      At_least (Index.inter i1' i2')
  in
  let matching_kinds case1 case2 =
    K.equal
      (Maps_to.fields_kind case1.maps_to)
      (Maps_to.fields_kind case2.maps_to)
  in
  let join_case env case1 case2 =
    let index = join_index case1.index case2.index in
    let maps_to = Maps_to.join env case1.maps_to case2.maps_to in
    let env_extension =
      join_env_extension env case1.env_extension case2.env_extension
    in
    { maps_to; index; env_extension }
  in
  let join_knowns_tags case1 case2 : case option =
    (* We assume that if tags are equals, the products will contains values of
       the same kinds. *)
    match case1, case2 with
    | None, None -> None
    | Some case1, None -> begin
      let only_case1 () =
        (* CF Type_descr.join_head_or_unknown_or_bottom, we need to join those
           to ensure that free variables not present in the target env are
           cleaned out of the types. Same bellow *)
        (* CR pchambart: This seams terribly inefficient. *)
        let env =
          Join_env.create
            (Join_env.target_join_env env)
            ~left_env:(Join_env.left_join_env env)
            ~right_env:(Join_env.left_join_env env)
        in
        let case1 = join_case env case1 case1 in
        Some case1
      in
      match other2 with
      | Bottom -> only_case1 ()
      | Ok other_case ->
        if matching_kinds case1 other_case
        then Some (join_case env case1 other_case)
        else (* If kinds don't match, the tags can't match *)
          only_case1 ()
    end
    | None, Some case2 -> begin
      let only_case2 () =
        (* See at the other bottom case *)
        let env =
          Join_env.create
            (Join_env.target_join_env env)
            ~left_env:(Join_env.right_join_env env)
            ~right_env:(Join_env.right_join_env env)
        in
        let case2 = join_case env case2 case2 in
        Some case2
      in
      match other1 with
      | Bottom -> only_case2 ()
      | Ok other_case ->
        if matching_kinds other_case case2
        then Some (join_case env other_case case2)
        else only_case2 ()
    end
    | Some case1, Some case2 -> Some (join_case env case1 case2)
  in
  let known_tags =
    Tag.Map.merge
      (fun _tag case1 case2 -> join_knowns_tags case1 case2)
      known1 known2
  in
  let other_tags : case Or_bottom.t =
    match other1, other2 with
    | Bottom, Bottom -> Bottom
    | Ok other1, Bottom ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env env)
          ~left_env:(Join_env.left_join_env env)
          ~right_env:(Join_env.left_join_env env)
      in
      let other1 = join_case env other1 other1 in
      Ok other1
    | Bottom, Ok other2 ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env env)
          ~left_env:(Join_env.right_join_env env)
          ~right_env:(Join_env.right_join_env env)
      in
      let other2 = join_case env other2 other2 in
      Ok other2
    | Ok other1, Ok other2 -> Ok (join_case env other1 other2)
  in
  { known_tags; other_tags }

and join_row_like_for_blocks env by_tag1 by_tag2 = assert false

and join_row_like_for_closures env (by_closure_id1 : TG.Row_like_for_closures.t)
    (by_closure_id2 : TG.Row_like_for_closures.t) : TG.Row_like_for_closures.t =
  assert false

and join_env_extension env (ext1 : TEE.t) (ext2 : TEE.t) : TEE.t =
  let equations =
    Name.Map.merge
      (fun name ty1_opt ty2_opt ->
        match ty1_opt, ty2_opt with
        | None, _ | _, None -> None
        | Some ty1, Some ty2 -> begin
          match join env ty1 ty2 with
          | Known ty ->
            if MTC.is_alias_of_name ty name
            then
              (* This is rare but not anomalous. It may mean that [ty1] and
                 [ty2] are both alias types which canonicalize to [name], for
                 instance. In any event, if the best type available for [name]
                 is [= name], we effectively know nothing, so we drop [name].
                 ([name = name] would be rejected by [Typing_env.add_equation]
                 anyway.) *)
              None
            else begin
              (* This should always pass due to the [is_alias_of_name] check. *)
              MTC.check_equation name ty;
              Some ty
            end
          | Unknown -> None
        end)
      (TEE.to_map ext1) (TEE.to_map ext2)
  in
  TEE.from_map equations

and join_generic_product :
      'key 'key_map.
      Join_env.t ->
      components_by_index1:'key_map ->
      components_by_index2:'key_map ->
      merge:
        (('key -> TG.t option -> TG.t option -> TG.t option) ->
        'key_map ->
        'key_map ->
        'key_map) ->
      'key_map =
 fun env ~components_by_index1 ~components_by_index2 ~merge ->
  merge
    (fun _index ty1_opt ty2_opt ->
      match ty1_opt, ty2_opt with
      | None, _ | _, None -> None
      | Some ty1, Some ty2 -> begin
        match join env ty1 ty2 with Known ty -> Some ty | Unknown -> None
      end)
    components_by_index1 components_by_index2

and join_closure_id_indexed_product env
    ({ closure_id_components_by_index = components_by_index1 } :
      TG.Product.Closure_id_indexed.t)
    ({ closure_id_components_by_index = components_by_index2 } :
      TG.Product.Closure_id_indexed.t) : TG.Product.Closure_id_indexed.t =
  let closure_id_components_by_index =
    join_generic_product env ~components_by_index1 ~components_by_index2
      ~merge:Closure_id.Map.merge
  in
  TG.Product.Closure_id_indexed.create closure_id_components_by_index

and join_var_within_closure_indexed_product env
    ({ var_within_closure_components_by_index = components_by_index1 } :
      TG.Product.Var_within_closure_indexed.t)
    ({ var_within_closure_components_by_index = components_by_index2 } :
      TG.Product.Var_within_closure_indexed.t) :
    TG.Product.Var_within_closure_indexed.t =
  let var_within_closure_components_by_index =
    join_generic_product env ~components_by_index1 ~components_by_index2
      ~merge:Var_within_closure.Map.merge
  in
  TG.Product.Var_within_closure_indexed.create
    var_within_closure_components_by_index

and join_int_indexed_product env
    ({ fields = fields1; kind = kind1 } : TG.Product.Int_indexed.t)
    ({ fields = fields2; kind = kind2 } : TG.Product.Int_indexed.t) :
    TG.Product.Int_indexed.t =
  if not (K.equal kind1 kind2)
  then
    Misc.fatal_errorf
      "join_int_indexed_product between mismatching kinds %a and %a@." K.print
      kind1 K.print kind2;
  let length1 = Array.length fields1 in
  let length2 = Array.length fields2 in
  let length = min length1 length2 in
  let exception Exit in
  let all_phys_equal =
    try
      for index = 0 to length - 1 do
        if fields1.(index) != fields2.(index) then raise Exit
      done;
      true
    with Exit -> false
  in
  let fields =
    if all_phys_equal
    then
      if length1 = length
      then fields1
      else begin
        assert (length2 = length);
        fields2
      end
    else
      Array.init length (fun index ->
          if fields1.(index) == fields2.(index)
          then fields1.(index)
          else
            match join env fields1.(index) fields2.(index) with
            | Unknown -> MTC.unknown kind1
            | Known ty -> ty)
  in
  TG.Product.Int_indexed.create_from_array kind1 fields

and join_head_of_kind_value env (head1 : TG.head_of_kind_value)
    (head2 : TG.head_of_kind_value) : TG.head_of_kind_value Or_unknown.t =
  match head1, head2 with
  | ( Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1 },
      Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2 } )
    ->
    Or_unknown.map (join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates) ->
        (* Uniqueness tracks whether duplication/lifting is allowed. It must
           always be propagated, both for meet and join. *)
        let is_unique = is_unique1 || is_unique2 in
        TG.Head_of_kind_value.create_variant ~is_unique ~blocks ~immediates)
  | Boxed_float n1, Boxed_float n2 ->
    Or_unknown.map (join env n1 n2) ~f:(fun n ->
        TG.Head_of_kind_value.create_boxed_float n)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_unknown.map (join env n1 n2) ~f:(fun n ->
        TG.Head_of_kind_value.create_boxed_int32 n)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_unknown.map (join env n1 n2) ~f:(fun n ->
        TG.Head_of_kind_value.create_boxed_int64 n)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_unknown.map (join env n1 n2) ~f:(fun n ->
        TG.Head_of_kind_value.create_boxed_nativeint n)
  | ( Closures { by_closure_id = by_closure_id1 },
      Closures { by_closure_id = by_closure_id2 } ) ->
    let by_closure_id =
      join_row_like_for_closures env by_closure_id1 by_closure_id2
    in
    Known (TG.Head_of_kind_value.create_closures by_closure_id)
  | String strs1, String strs2 ->
    let strs = String_info.Set.union strs1 strs2 in
    Known (TG.Head_of_kind_value.create_string strs)
  | Array { length = length1 }, Array { length = length2 } ->
    Or_unknown.map (join env length1 length2) ~f:(fun length ->
        TG.Head_of_kind_value.create_array ~length)
  | ( ( Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ),
      _ ) ->
    Unknown

and join_head_of_kind_naked_immediate env
    (head1 : TG.Head_of_kind_naked_immediate.t)
    (head2 : TG.Head_of_kind_naked_immediate.t) :
    TG.Head_of_kind_naked_immediate.t Or_unknown.t =
  let module I = Targetint_31_63 in
  match head1, head2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    let is = I.Set.union is1 is2 in
    Known (TG.Head_of_kind_naked_immediate.create_naked_immediates is)
  | Is_int ty1, Is_int ty2 ->
    Or_unknown.map (join env ty1 ty2) ~f:(fun ty ->
        TG.Head_of_kind_naked_immediate.create_is_int ty)
  | Get_tag ty1, Get_tag ty2 ->
    Or_unknown.map (join env ty1 ty2) ~f:(fun ty ->
        TG.Head_of_kind_naked_immediate.create_get_tag ty)
  (* From now on: Irregular cases *)
  (* CR vlaviron: There could be improvements based on reduction (trying to
     reduce the is_int and get_tag cases to naked_immediate sets, then joining
     those) but this looks unlikely to be useful and could end up begin quite
     expensive. *)
  | Is_int ty, Naked_immediates is_int | Naked_immediates is_int, Is_int ty ->
    if I.Set.is_empty is_int
    then Known (TG.Head_of_kind_naked_immediate.create_is_int ty)
    else
      (* Slightly better than Unknown *)
      Known
        (TG.Head_of_kind_naked_immediate.create_naked_immediates
           (I.Set.add I.zero (I.Set.add I.one is_int)))
  | Get_tag ty, Naked_immediates tags | Naked_immediates tags, Get_tag ty ->
    if I.Set.is_empty tags
    then Known (TG.Head_of_kind_naked_immediate.create_get_tag ty)
    else Unknown
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) -> Unknown

and join_head_or_unknown_or_bottom (env : Join_env.t)
    (head1 : _ Or_unknown_or_bottom.t) (head2 : _ Or_unknown_or_bottom.t) :
    _ Or_unknown_or_bottom.t =
  match head1, head2 with
  | Bottom, Bottom -> Bottom
  (* The target environment defines all the names from the left and right
     environments, so we can safely return any input as the result *)
  | Ok head, Bottom | Bottom, Ok head -> Ok head
  | Unknown, _ -> Unknown
  | _, Unknown -> Unknown
  | Ok head1, Ok head2 -> (
    match Head.join env head1 head2 with
    | Known head -> Ok head
    | Unknown -> Unknown)

and join_descr ?bound_name ~to_type join_env _ty1 _ty2 t1 t2 : _ Or_unknown.t =
  (* CR mshinwell: Rewrite this to avoid the [option] allocations from
     [get_canonical_simples_and_expand_heads] *)
  let canonical_simple1, expanded1, canonical_simple2, expanded2 =
    Expand_head.get_canonical_simples_and_expand_heads
      ~left_env:(Join_env.left_join_env join_env)
      ~left_ty:t1
      ~right_env:(Join_env.right_join_env join_env)
      ~right_ty:t2
  in
  (* CR mshinwell: Add shortcut when the canonical simples are equal *)
  let shared_aliases =
    let shared_aliases =
      match canonical_simple1, expanded1, canonical_simple2, expanded2 with
      | None, _, None, _
      | None, (Ok _ | Unknown), _, _
      | _, _, None, (Ok _ | Unknown) ->
        Aliases.Alias_set.empty
      | Some simple1, _, _, Bottom -> Aliases.Alias_set.singleton simple1
      | _, Bottom, Some simple2, _ -> Aliases.Alias_set.singleton simple2
      | Some simple1, _, Some simple2, _ ->
        if Simple.same simple1 simple2
        then Aliases.Alias_set.singleton simple1
        else
          Aliases.Alias_set.inter
            (all_aliases_of
               (Join_env.left_join_env join_env)
               canonical_simple1
               ~in_env:(Join_env.target_join_env join_env))
            (all_aliases_of
               (Join_env.right_join_env join_env)
               canonical_simple2
               ~in_env:(Join_env.target_join_env join_env))
    in
    match bound_name with
    | None -> shared_aliases
    | Some bound_name ->
      (* CR vlaviron: this ensures that we're not creating an alias to a
         different simple that is just bound_name with different coercion. Such
         an alias is forbidden. *)
      Aliases.Alias_set.filter
        ~f:(fun simple -> not (Simple.same simple (Simple.name bound_name)))
        shared_aliases
  in
  match Aliases.Alias_set.find_best shared_aliases with
  | Some alias -> Known (to_type (TG.create_equals alias))
  | None -> (
    match canonical_simple1, canonical_simple2 with
    | Some simple1, Some simple2
      when Join_env.already_joining join_env simple1 simple2 ->
      (* CR vlaviron: Fix this to Unknown when Product can handle it *)
      Known (to_type (unknown ()))
    | Some _, Some _ | Some _, None | None, Some _ | None, None -> (
      let join_env =
        match canonical_simple1, canonical_simple2 with
        | Some simple1, Some simple2 ->
          Join_env.now_joining join_env simple1 simple2
        | Some _, None | None, Some _ | None, None -> join_env
      in
      match join_head_or_unknown_or_bottom join_env head1 head2 with
      | Bottom -> Known (to_type (bottom ()))
      | Unknown -> Known (to_type (unknown ()))
      | Ok head -> Known (to_type (create head))))

(* The entry point for joining types. *)
and join ?bound_name env (t1 : TG.t) (t2 : TG.t) : TG.t Or_unknown.t =
  match t1, t2 with
  | Value ty1, Value ty2 ->
    join_type_descr ?bound_name env K.value t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_value ~to_type:(fun ty -> Value ty)
  | Naked_immediate ty1, Naked_immediate ty2 ->
    join_type_descr ?bound_name env K.naked_immediate t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_immediate ~to_type:(fun ty ->
        Naked_immediate ty)
  | Naked_float ty1, Naked_float ty2 ->
    join_type_descr ?bound_name env K.naked_float t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_float ~to_type:(fun ty ->
        Naked_float ty)
  | Naked_int32 ty1, Naked_int32 ty2 ->
    join_type_descr ?bound_name env K.naked_int32 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int32 ~to_type:(fun ty ->
        Naked_int32 ty)
  | Naked_int64 ty1, Naked_int64 ty2 ->
    join_type_descr ?bound_name env K.naked_int64 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int64 ~to_type:(fun ty ->
        Naked_int64 ty)
  | Naked_nativeint ty1, Naked_nativeint ty2 ->
    join_type_descr ?bound_name env K.naked_nativeint t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_nativeint ~to_type:(fun ty ->
        Naked_nativeint ty)
  | Rec_info ty1, Rec_info ty2 ->
    join_type_descr ?bound_name env K.rec_info t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_rec_info ~to_type:(fun ty -> Rec_info ty)
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ),
      _ ) ->
    Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus@ %a" print t1 print
      t2

let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
  let result = Bound_name.var result_var in
  let env = Typing_env.add_definition env result result_kind in
  match meet (Meet_env.create env) t shape with
  | Bottom -> Bottom
  | Ok (_meet_ty, env_extension) -> Ok env_extension
