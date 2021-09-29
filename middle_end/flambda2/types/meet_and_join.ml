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

module TG = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level

let add_equation _env (simple : Simple.t) ty_of_simple env_extension ~to_type
    ~apply_coercion =
  match Simple.must_be_name simple with
  (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
  | Some (name, coercion_from_name_to_simple) ->
    let coercion_from_simple_to_name =
      Coercion.inverse coercion_from_name_to_simple
    in
    let ty_of_name =
      match apply_coercion ty_of_simple coercion_from_simple_to_name with
      | Ok ty -> ty
      | Bottom -> bottom ()
    in
    TEE.add_or_replace_equation env_extension name (to_type ty_of_name)
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

let[@inline always] get_canonical_simples_and_expand_heads ~force_to_kind
    ~to_type kind ~left_env ~left_ty ~right_env ~right_ty =
  let canonical_simple1 =
    match
      TE.get_alias_then_canonical_simple_exn left_env (to_type left_ty)
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let head1 = expand_head ~force_to_kind left_ty left_env kind in
  let canonical_simple2 =
    match
      TE.get_alias_then_canonical_simple_exn right_env (to_type right_ty)
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let head2 = expand_head ~force_to_kind right_ty right_env kind in
  canonical_simple1, head1, canonical_simple2, head2

type 'head meet_or_join_head_or_unknown_or_bottom_result =
  | Left_head_unchanged
  | Right_head_unchanged
  | New_head of 'head Or_unknown_or_bottom.t * TEE.t

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
  (* CR lmaurer: This could be doing things like discovering two depth variables
     are equal *)
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

let rec meet_variant env ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_bottom.t =
  let blocks =
    meet_unknown Blocks.meet ~contents_is_bottom:Blocks.is_bottom env blocks1
      blocks2
  in
  let blocks : _ Or_bottom.t =
    (* XXX Clean this up *)
    match blocks with
    | Bottom | Ok (Or_unknown.Unknown, _) -> blocks
    | Ok (Or_unknown.Known blocks', _) ->
      if Blocks.is_bottom blocks' then Bottom else blocks
  in
  let imms =
    meet_unknown T.meet ~contents_is_bottom:T.is_obviously_bottom env imms1
      imms2
  in
  let imms : _ Or_bottom.t =
    match imms with
    | Bottom | Ok (Or_unknown.Unknown, _) -> imms
    | Ok (Or_unknown.Known imms', _) ->
      if T.is_obviously_bottom imms' then Bottom else imms
  in
  match blocks, imms with
  | Bottom, Bottom -> Bottom
  | Ok (blocks, env_extension), Bottom ->
    let immediates : _ Or_unknown.t = Known (T.bottom K.naked_immediate) in
    Ok (blocks, immediates, env_extension)
  | Bottom, Ok (immediates, env_extension) ->
    let blocks : _ Or_unknown.t = Known (Blocks.create_bottom ()) in
    Ok (blocks, immediates, env_extension)
  | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
    begin
      match (blocks : _ Or_unknown.t) with
      | Unknown -> ()
      | Known blocks -> assert (not (Blocks.is_bottom blocks))
    end;
    begin
      match (immediates : _ Or_unknown.t) with
      | Unknown -> ()
      | Known imms -> assert (not (T.is_obviously_bottom imms))
    end;
    let env_extension =
      let env = Meet_env.env env in
      let join_env = Join_env.create env ~left_env:env ~right_env:env in
      Typing_env_extension_meet_and_join.join join_env env_extension1
        env_extension2
    in
    Ok (blocks, immediates, env_extension)

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
        Variant (Variant.create ~is_unique ~blocks ~immediates), env_extension)
  | Boxed_float n1, Boxed_float n2 ->
    Or_bottom.map (T.meet env n1 n2) ~f:(fun (n, env_extension) ->
        Boxed_float n, env_extension)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_bottom.map (T.meet env n1 n2) ~f:(fun (n, env_extension) ->
        Boxed_int32 n, env_extension)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_bottom.map (T.meet env n1 n2) ~f:(fun (n, env_extension) ->
        Boxed_int64 n, env_extension)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_bottom.map (T.meet env n1 n2) ~f:(fun (n, env_extension) ->
        Boxed_nativeint n, env_extension)
  | ( Closures { by_closure_id = by_closure_id1 },
      Closures { by_closure_id = by_closure_id2 } ) ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    Or_bottom.map (C.meet env by_closure_id1 by_closure_id2)
      ~f:(fun (by_closure_id, env_extension) ->
        Closures { by_closure_id }, env_extension)
  | String strs1, String strs2 ->
    let strs = String_info.Set.inter strs1 strs2 in
    if String_info.Set.is_empty strs
    then Bottom
    else Or_bottom.Ok (String strs, TEE.empty)
  | Array { length = length1 }, Array { length = length2 } ->
    Or_bottom.map (T.meet env length1 length2)
      ~f:(fun (length, env_extension) -> Array { length }, env_extension)
  | ( ( Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ),
      _ ) ->
    (* CR vlaviron: This assumes that all the different constructors are
       incompatible. This could break very hard for users of Obj. *)
    Bottom

and meet_head_of_kind_naked_immediate env t1 t2 : _ Or_bottom.t =
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    let is = I.Set.inter is1 is2 in
    if I.Set.is_empty is then Bottom else Ok (Naked_immediates is, TEE.empty)
  | Is_int ty1, Is_int ty2 ->
    Or_bottom.map (T.meet env ty1 ty2) ~f:(fun (ty, env_extension) ->
        Is_int ty, env_extension)
  | Get_tag ty1, Get_tag ty2 ->
    Or_bottom.map (T.meet env ty1 ty2) ~f:(fun (ty, env_extension) ->
        Get_tag ty, env_extension)
  | Is_int ty, Naked_immediates is_int | Naked_immediates is_int, Is_int ty ->
    begin
    match I.Set.elements is_int with
    | [] -> Bottom
    | [is_int] -> (
      let shape =
        if I.equal is_int I.zero
        then Some (T.any_block ())
        else if I.equal is_int I.one
        then Some (T.any_tagged_immediate ())
        else None
      in
      match shape with
      | Some shape ->
        Or_bottom.map (T.meet env ty shape) ~f:(fun (ty, env_extension) ->
            Is_int ty, env_extension)
      | None -> Bottom)
    | _ :: _ :: _ ->
      (* Note: we're potentially losing precision because the set could end up
         not containing either 0 or 1 or both, but this should be uncommon. *)
      Ok (Is_int ty, TEE.empty)
  end
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
    match T.blocks_with_these_tags tags with
    | Known shape ->
      Or_bottom.map (T.meet env ty shape) ~f:(fun (ty, env_extension) ->
          Get_tag ty, env_extension)
    | Unknown -> Ok (Get_tag ty, TEE.empty))
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
    (* We can't return Bottom, as it would be unsound, so we need to either do
       the actual meet with Naked_immediates, or just give up and return one of
       the arguments. *)
    Ok (t1, TEE.empty)

and meet_head_or_unknown_or_bottom (env : Meet_env.t)
    (head1 : _ Or_unknown_or_bottom.t) (head2 : _ Or_unknown_or_bottom.t) :
    meet_or_join_head_or_unknown_or_bottom_result =
  match head1, head2 with
  | _, Unknown -> Left_head_unchanged
  | Unknown, _ -> Right_head_unchanged
  | Bottom, _ -> Left_head_unchanged
  | _, Bottom -> Right_head_unchanged
  | Ok head1, Ok head2 -> (
    match Head.meet env head1 head2 with
    | Ok (head, env_extension) -> New_head (Ok head, env_extension)
    | Bottom -> New_head (Bottom, TEE.empty))

(* CR mshinwell: I've seen one case (on tests12.ml) where it appears that an env
   extension for a join point contains an equation for a symbol which is just
   the same as that already in the environment. This shouldn't have been emitted
   from [meet]. *)

and meet_type_descr ~force_to_kind ~to_type env kind ty1 ty2 t1 t2 :
    _ Or_bottom.t =
  let typing_env = Meet_env.env env in
  let head1 = expand_head ~force_to_kind t1 typing_env kind in
  let head2 = expand_head ~force_to_kind t2 typing_env kind in
  match
    TE.get_alias_then_canonical_simple_exn typing_env (to_type t1)
      ~min_name_mode:Name_mode.in_types
  with
  | exception Not_found -> begin
    match
      TE.get_alias_then_canonical_simple_exn typing_env (to_type t2)
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> begin
      match meet_head_or_unknown_or_bottom env head1 head2 with
      | Left_head_unchanged -> Ok (ty1, TEE.empty)
      | Right_head_unchanged -> Ok (ty2, TEE.empty)
      | New_head (head, env_extension) -> (
        match head with
        | Bottom -> Bottom
        | Unknown -> Ok (to_type (unknown ()), env_extension)
        | Ok head -> Ok (to_type (create head), env_extension))
    end
    | simple2 -> begin
      match meet_head_or_unknown_or_bottom env head1 head2 with
      | Left_head_unchanged ->
        let env_extension =
          TEE.empty |> add_equation env simple2 (create_no_alias head1) ~to_type
        in
        Ok (to_type (create_equals simple2), env_extension)
      | Right_head_unchanged -> Ok (to_type (create_equals simple2), TEE.empty)
      | New_head (head, env_extension) -> (
        let env_extension =
          env_extension
          |> add_equation env simple2 (create_no_alias head) ~to_type
        in
        match head with
        | Bottom -> Bottom
        | Unknown | Ok _ -> Ok (to_type (create_equals simple2), env_extension))
    end
  end
  | simple1 -> (
    match
      TE.get_alias_then_canonical_simple_exn typing_env (to_type t2)
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> begin
      match meet_head_or_unknown_or_bottom env head1 head2 with
      | Left_head_unchanged -> Ok (to_type (create_equals simple1), TEE.empty)
      | Right_head_unchanged ->
        let env_extension =
          TEE.empty |> add_equation env simple1 ~to_type (create_no_alias head2)
        in
        Ok (to_type (create_equals simple1), env_extension)
      | New_head (head, env_extension) -> (
        let env_extension =
          env_extension
          |> add_equation env simple1 ~to_type (create_no_alias head)
        in
        match head with
        | Bottom -> Bottom
        | Unknown | Ok _ -> Ok (to_type (create_equals simple1), env_extension))
    end
    | simple2 ->
      if Simple.equal simple1 simple2
         || Meet_env.already_meeting env simple1 simple2
      then
        (* This produces "=simple" for the output rather than a type that might
           need transformation back from an expanded head (as would happen if we
           used the next case). *)
        Ok (to_type (create_equals simple1), TEE.empty)
      else begin
        assert (not (Simple.equal simple1 simple2));
        let env = Meet_env.now_meeting env simple1 simple2 in
        (* In the following cases we may generate equations "pointing the wrong
           way", for example "y : =x" when [y] is the canonical element. This
           doesn't matter, however, because [Typing_env] sorts this out when
           adding equations into an environment. *)
        (* CR mshinwell: May be able to improve efficiency by not doing [meet]
           again (via [TE.add_env_extension]) if we tried here to emit the
           equations the correct way around *)
        match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged ->
          let env_extension =
            TEE.empty
            |> add_equation env simple2 ~to_type (create_equals simple1)
          in
          Ok (to_type (create_equals simple1), env_extension)
        | Right_head_unchanged ->
          let env_extension =
            TEE.empty
            |> add_equation env simple1 ~to_type (create_equals simple2)
          in
          Ok (to_type (create_equals simple2), env_extension)
        | New_head (head, env_extension) -> (
          let env_extension =
            env_extension
            |> add_equation env simple1 ~to_type (create_no_alias head)
            |> add_equation env simple2 ~to_type (create_equals simple1)
          in
          (* It makes things easier (to check if the result of [meet] was
             bottom) to not return "=simple" in the bottom case. This is ok
             because no constraint is being dropped; the type cannot be refined
             any further. *)
          match head with
          | Bottom -> Bottom
          | Unknown | Ok _ -> Ok (to_type (create_equals simple1), env_extension)
          )
      end)

and meet env t1 t2 =
  match t1, t2 with
  | Value ty1, Value ty2 ->
    meet_type_descr env K.value t1 t2 ty1 ty2 ~force_to_kind:force_to_kind_value
      ~to_type:(fun ty -> Value ty)
  | Naked_immediate ty1, Naked_immediate ty2 ->
    meet_type_descr env K.naked_immediate t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_immediate ~to_type:(fun ty ->
        Naked_immediate ty)
  | Naked_float ty1, Naked_float ty2 ->
    T_Nf.meet env K.naked_float t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_float ~to_type:(fun ty ->
        Naked_float ty)
  | Naked_int32 ty1, Naked_int32 ty2 ->
    T_N32.meet env K.naked_int32 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int32 ~to_type:(fun ty ->
        Naked_int32 ty)
  | Naked_int64 ty1, Naked_int64 ty2 ->
    T_N64.meet env K.naked_int64 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int64 ~to_type:(fun ty ->
        Naked_int64 ty)
  | Naked_nativeint ty1, Naked_nativeint ty2 ->
    meet_type_descr env K.naked_nativeint t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_nativeint ~to_type:(fun ty ->
        Naked_nativeint ty)
  | Rec_info ty1, Rec_info ty2 ->
    meet_type_descr env K.rec_info t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_rec_info ~to_type:(fun ty -> Rec_info ty)
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ),
      _ ) ->
    Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a" print t1 print
      t2

let join_unknown join_contents (env : Join_env.t) (or_unknown1 : _ Or_unknown.t)
    (or_unknown2 : _ Or_unknown.t) : _ Or_unknown.t =
  match or_unknown1, or_unknown2 with
  | _, Unknown | Unknown, _ -> Unknown
  | Known contents1, Known contents2 -> join_contents env contents1 contents2

let join_head_of_kind_naked_float _env t1 t2 : _ Or_unknown.t =
  Known (Float.Set.union t1 t2)

let join_head_of_kind_naked_int32 _env t1 t2 : _ Or_unknown.t =
  Known (Int32.Set.union t1 t2)

let join_head_of_kind_naked_int64 _env t1 t2 : _ Or_unknown.t =
  Known (Int64.Set.union t1 t2)

let join_head_of_kind_naked_nativeint _env t1 t2 : _ Or_unknown.t =
  Known (Targetint_32_64.Set.union t1 t2)

let join_head_of_kind_rec_info _env t1 t2 : _ Or_unknown.t =
  if Rec_info_expr.equal t1 t2 then Known t1 else Unknown

let rec join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_unknown.t =
  let blocks_join env b1 b2 : _ Or_unknown.t = Known (Blocks.join env b1 b2) in
  let blocks = join_unknown blocks_join env blocks1 blocks2 in
  let imms = join_unknown (T.join ?bound_name:None) env imms1 imms2 in
  match blocks, imms with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown | Unknown, Known _ | Known _, Known _ ->
    Known (blocks, imms)

and join_head_of_kind_value env (head1 : TG.head_of_kind_value)
    (head2 : TG.head_of_kind_value) : _ Or_unknown.t =
  match head1, head2 with
  | ( Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1 },
      Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2 } )
    ->
    Or_unknown.map (join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates) ->
        (* Uniqueness tracks whether duplication/lifting is allowed. It must
           always be propagated, both for meet and join. *)
        let is_unique = is_unique1 || is_unique2 in
        Variant (Variant.create ~is_unique ~blocks ~immediates))
  | Boxed_float n1, Boxed_float n2 ->
    Or_unknown.map (T.join env n1 n2) ~f:(fun n -> Boxed_float n)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_unknown.map (T.join env n1 n2) ~f:(fun n -> Boxed_int32 n)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_unknown.map (T.join env n1 n2) ~f:(fun n -> Boxed_int64 n)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_unknown.map (T.join env n1 n2) ~f:(fun n -> Boxed_nativeint n)
  | ( Closures { by_closure_id = by_closure_id1 },
      Closures { by_closure_id = by_closure_id2 } ) ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    let by_closure_id = C.join env by_closure_id1 by_closure_id2 in
    Known (Closures { by_closure_id })
  | String strs1, String strs2 ->
    let strs = String_info.Set.union strs1 strs2 in
    Known (String strs)
  | Array { length = length1 }, Array { length = length2 } ->
    Or_unknown.map (T.join env length1 length2) ~f:(fun length ->
        Array { length })
  | ( ( Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ),
      _ ) ->
    Unknown

and join_head_of_kind_naked_immediate env t1 t2 : _ Or_unknown.t =
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    let is = I.Set.union is1 is2 in
    Known (Naked_immediates is)
  | Is_int ty1, Is_int ty2 ->
    Or_unknown.map (T.join env ty1 ty2) ~f:(fun ty -> Is_int ty)
  | Get_tag ty1, Get_tag ty2 ->
    Or_unknown.map (T.join env ty1 ty2) ~f:(fun ty -> Get_tag ty)
  (* From now on: Irregular cases *)
  (* CR vlaviron: There could be improvements based on reduction (trying to
     reduce the is_int and get_tag cases to naked_immediate sets, then joining
     those) but this looks unlikely to be useful and could end up begin quite
     expensive. *)
  | Is_int ty, Naked_immediates is_int | Naked_immediates is_int, Is_int ty ->
    if I.Set.is_empty is_int
    then Known (Is_int ty)
    else
      (* Slightly better than Unknown *)
      Known (Naked_immediates (I.Set.add I.zero (I.Set.add I.one is_int)))
  | Get_tag ty, Naked_immediates tags | Naked_immediates tags, Get_tag ty ->
    if I.Set.is_empty tags then Known (Get_tag ty) else Unknown
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

and join ?bound_name ~force_to_kind ~to_type join_env kind _ty1 _ty2 t1 t2 :
    _ Or_unknown.t =
  (* Format.eprintf "DESCR: Joining %a and %a\n%!" print t1 print t2;
     Format.eprintf "Left:@ %a@ Right:@ %a\n%!" Code_age_relation.print
     (Join_env.left_join_env join_env |> TE.code_age_relation)
     Code_age_relation.print (Join_env.right_join_env join_env |>
     TE.code_age_relation); *)
  (* Typing_env.print (Join_env.left_join_env join_env) Typing_env.print
     (Join_env.right_join_env join_env); *)
  (* CR mshinwell: Rewrite this to avoid the [option] allocations from
     [get_canonical_simples_and_expand_heads] *)
  let canonical_simple1, head1, canonical_simple2, head2 =
    get_canonical_simples_and_expand_heads ~force_to_kind ~to_type kind
      ~left_env:(Join_env.left_join_env join_env)
      ~left_ty:t1
      ~right_env:(Join_env.right_join_env join_env)
      ~right_ty:t2
  in

  (* CR mshinwell: Add shortcut when the canonical simples are equal *)
  let shared_aliases =
    let shared_aliases =
      match canonical_simple1, head1, canonical_simple2, head2 with
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
  (* Format.eprintf "Shared aliases:@ %a\n%!" Simple.Set.print
     shared_aliases; *)
  match Aliases.Alias_set.find_best shared_aliases with
  | Some alias -> Known (to_type (create_equals alias))
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

let join ?bound_name env t1 t2 =
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
