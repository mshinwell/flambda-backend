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

[@@@ocaml.warning "+a-30-40-41-42"]

module K = Flambda_kind
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module TD = Type_descr

module Block_size = struct
  include Targetint_31_63.Imm

  (** [subset t1 t2] is true if [t1] is a subset of [t2] *)
  let subset t1 t2 = Stdlib.( <= ) (compare t1 t2) 0

  (* An integer [i] represents all the values smaller than i, hence a smaller
     number is included in a bigger *)
  let union t1 t2 = Targetint_31_63.Imm.max t1 t2

  let inter t1 t2 = Targetint_31_63.Imm.min t1 t2

  let apply_renaming t _ = t
end

(* The grammar of Flambda types. *)
type t =
  | Value of head_of_kind_value TD.t
  | Naked_immediate of head_of_kind_naked_immediate TD.t
  | Naked_float of head_of_kind_naked_float TD.t
  | Naked_int32 of head_of_kind_naked_int32 TD.t
  | Naked_int64 of head_of_kind_naked_int64 TD.t
  | Naked_nativeint of head_of_kind_naked_nativeint TD.t
  | Rec_info of head_of_kind_rec_info TD.t

and head_of_kind_value =
  | Variant of
      { immediates : t Or_unknown.t;
        blocks : row_like_for_blocks;
        is_unique : bool
      }
  | Boxed_float of t
  | Boxed_int32 of t
  | Boxed_int64 of t
  | Boxed_nativeint of t
  | Closures of { by_closure_id : row_like_for_closures }
  | String of String_info.Set.t
  | Array of { length : t }

and head_of_kind_naked_immediate =
  | Naked_immediates of Targetint_31_63.Set.t
  | Is_int of t
  | Get_tag of t

and head_of_kind_naked_float = Float.Set.t

and head_of_kind_naked_int32 = Int32.Set.t

and head_of_kind_naked_int64 = Int64.Set.t

and head_of_kind_naked_nativeint = Targetint_32_64.Set.t

and head_of_kind_rec_info = Rec_info_expr.t

(* Note: it wouldn't require many changes to change this to an interval:
 * type 'index row_like_index = { at_least : 'index; at_most : 'index }
 * representing { x | at_least \subset x /\ x \subset at_most }
 *)
and 'index row_like_index =
  | Known of 'index  (** [Known x] represents the singleton set: { x } *)
  | At_least of 'index
      (** [At_least x] represents the set { y | x \subset y } *)

and ('index, 'maps_to) row_like_case =
  { maps_to : 'maps_to;
        (** Kinds different from [Value] are only allowed in cases with known
            tags. Currently cases with tag 254 must have fields of kind
            [Naked_float] and all other must have fields of kind [Value]. *)
    index : 'index;
    env_extension : env_extension
  }

and row_like_for_blocks =
  { known_tags : (Block_size.t, int_indexed_product) row_like_case Tag.Map.t;
    other_tags : (Block_size.t, int_indexed_product) row_like_case Or_bottom.t
  }

and row_like_for_closures =
  { known_tags :
      (Set_of_closures_contents.t, closures_entry) row_like_case Tag.Map.t;
    other_tags :
      (Set_of_closures_contents.t, closures_entry) row_like_case Or_bottom.t
  }

(* CR pchambart: This is exactly a product with a different kind of fields
   Product should maybe be functorized once more to represent this. Maybe not
   because product also contains a kind which we don't want in that case. Also
   the closure_types and closure_var_types products shouldn't have to store the
   kind because it is always the same. *)
and closures_entry =
  { function_decls : function_type Or_unknown_or_bottom.t Closure_id.Map.t;
    closure_types : closure_id_indexed_product;
    closure_var_types : var_within_closure_indexed_product
  }

(* Products are a set of constraints: each new field reduces the concrete set.
   The empty product is top. There is no bottom. All components must be of the
   same kind.

   { 1 => Unknown; 2 => V } is equal to { 2 => V } *)
and closure_id_indexed_product =
  { closure_id_components_by_index : t Closure_id.Map.t }

and var_within_closure_indexed_product =
  { var_within_closure_components_by_index : t Var_within_closure.Map.t }

and int_indexed_product =
  { fields : t array;
    kind : Flambda_kind.t
  }

and function_type =
  { code_id : Code_id.t;
    rec_info : t
  }

and env_extension = { equations : t Name.Map.t } [@@unboxed]

let rec apply_renaming t renaming =
  match t with
  | Value ty ->
    let ty' = TD.apply_renaming ty renaming in
    if ty == ty' then t else Value ty'
  | Naked_immediate ty ->
    let ty' = TD.apply_renaming ty renaming in
    if ty == ty' then t else Naked_immediate ty'
  | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ -> t
  | Rec_info ty ->
    let ty' = TD.apply_renaming ty renaming in
    if ty == ty' then t else Rec_info ty'

and apply_renaming_head_of_kind_value head renaming =
  match head with
  | Variant { blocks; immediates; is_unique } -> (
    match apply_renaming_variant blocks immediates perm with
    | None -> t
    | Some (blocks, immediates) ->
      Variant (Variant.create ~is_unique ~blocks ~immediates))
  | Boxed_float ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t else Boxed_nativeint ty'
  | Closures { by_closure_id } ->
    let by_closure_id' =
      Row_like.For_closures_entry_by_set_of_closures_contents.apply_renaming
        by_closure_id perm
    in
    if by_closure_id == by_closure_id'
    then t
    else Closures { by_closure_id = by_closure_id' }
  | String _ -> t
  | Array { length } ->
    let length' = T.apply_renaming length perm in
    if length == length' then t else Array { length = length' }

and apply_renaming_variant blocks immediates perm =
  let immediates' =
    Or_unknown.map immediates ~f:(fun immediates ->
        T.apply_renaming immediates perm)
  in
  let blocks' =
    Or_unknown.map blocks ~f:(fun blocks -> Blocks.apply_renaming blocks perm)
  in
  if immediates == immediates' && blocks == blocks'
  then None
  else Some (blocks', immediates')

and apply_renaming_head_of_kind_naked_immediate head renaming =
  match head with
  | Naked_immediates _ -> head
  | Is_int ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Is_int ty'
  | Get_tag ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Get_tag ty'

and apply_renaming_row_like ({ known_tags; other_tags } as t) renaming =
  let rename_index = function
    | Known index -> Known (Index.apply_renaming index renaming)
    | At_least index -> At_least (Index.apply_renaming index renaming)
  in
  let known_tags' =
    Tag.Map.map_sharing
      (fun { index; maps_to; env_extension } ->
        { index = rename_index index;
          env_extension = TEE.apply_renaming env_extension renaming;
          maps_to = Maps_to.apply_renaming maps_to renaming
        })
      known_tags
  in
  let other_tags' : _ Or_bottom.t =
    match other_tags with
    | Bottom -> Bottom
    | Ok { index; maps_to; env_extension } ->
      Ok
        { index = rename_index index;
          env_extension = TEE.apply_renaming env_extension renaming;
          maps_to = Maps_to.apply_renaming maps_to renaming
        }
  in
  if known_tags == known_tags' && other_tags == other_tags'
  then t
  else { known_tags = known_tags'; other_tags = other_tags' }

let rec free_names t =
  match t with
  | Value ty ->
    TD.free_names ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value ty
  | Naked_immediate ty ->
    TD.free_names
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
      ~free_names_head:free_names_head_of_kind_naked_immediate ty
  | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ ->
    Name_occurrences.empty
  | Rec_info ty ->
    TD.free_names ~apply_renaming_head:Rec_info_expr.apply_renaming
      ~free_names_head:Rec_info_expr.free_names ty

and free_names_head_of_kind_value head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Name_occurrences.union
      (Or_unknown.free_names Blocks.free_names blocks)
      (Or_unknown.free_names free_names immediates)
  | Boxed_float ty -> free_names ty
  | Boxed_int32 ty -> free_names ty
  | Boxed_int64 ty -> free_names ty
  | Boxed_nativeint ty -> free_names ty
  | Closures { by_closure_id } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.free_names
      by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length } -> free_names length

and free_names_head_of_kind_naked_immediate head =
  match head with
  | Naked_immediates _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> free_names ty

and free_names_row_like { known_tags; other_tags } =
  let from_known_tags =
    Tag.Map.fold
      (fun _tag { maps_to; env_extension; index = _ } free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.union
             (TEE.free_names env_extension)
             (Maps_to.free_names maps_to)))
      known_tags Name_occurrences.empty
  in
  match other_tags with
  | Bottom -> from_known_tags
  | Ok { maps_to; env_extension; index = _ } ->
    Name_occurrences.union
      (Maps_to.free_names maps_to)
      (Name_occurrences.union from_known_tags (TEE.free_names env_extension))

let rec print ppf t =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]" TD.print
      ~print_head:print_head_of_kind_value
      ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value ppf ty
  | Naked_immediate ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_immediate@ %a)@]" TD.print ty
  | Naked_float ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float@ %a)@]" T_Nf.print ty
  | Naked_int32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int32@ %a)@]" T_N32.print ty
  | Naked_int64 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int64@ %a)@]" T_N64.print ty
  | Naked_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_nativeint@ %a)@]" TD.print ty
  | Rec_info ty -> Format.fprintf ppf "@[<hov 1>(Rec_info@ %a)@]" TD.print ty

and print_head_of_kind_value ppf head =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Variant%s@ @[<hov 1>(blocks@ %a)@]@ @[<hov 1>(tagged_imms@ \
       %a)@])@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print Blocks.print)
      blocks (Or_unknown.print T.print) immediates
  | Boxed_float naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]" T.print naked_ty
  | Boxed_int32 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]" T.print naked_ty
  | Boxed_int64 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]" T.print naked_ty
  | Boxed_nativeint naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]" T.print naked_ty
  | Closures { by_closure_id } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.print ppf
      by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]" String_info.Set.print
      str_infos
  | Array { length } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]" T.print length

and print_head_of_kind_naked_immediate ppf head =
  match head with
  | Naked_immediates is ->
    Format.fprintf ppf "@[<hov 1>(%a)@]" Targetint_31_63.Set.print is
  | Is_int ty -> Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]" print ty
  | Get_tag ty -> Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]" print ty

and print_head_of_kind_naked_float ppf head =
  Format.fprintf ppf "@[(Naked_float@ (%a))@]" Float.Set.print head

and print_head_of_kind_naked_int32 ppf head =
  Format.fprintf ppf "@[(Naked_int32@ (%a))@]" Int32.Set.print head

and print_head_of_kind_naked_int64 ppf head =
  Format.fprintf ppf "@[(Naked_int64@ (%a))@]" Int64.Set.print head

and print_head_of_kind_naked_nativeint ppf head =
  Format.fprintf ppf "@[(Naked_nativeint@ (%a))@]" Targetint_32_64.Set.print
    head

and print_head_of_kind_rec_info ppf head = Rec_info_expr.print ppf head

and [@ocamlformat "disable"] print_row_like ppf
    (({ known_tags; other_tags } as t) : t) =
  let print_index ppf = function
    | Known index -> Format.fprintf ppf "(Known @[<2>%a@])" Index.print index
    | At_least min_index ->
      Format.fprintf ppf "(At_least @[<2>%a@])" Index.print min_index
  in
  if is_bottom t then
    (* CR mshinwell: factor out (and elsewhere) *)
    let colour = Flambda_colours.top_or_bottom_type () in
    if Flambda_features.unicode () then
      Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s"
        colour (Flambda_colours.normal ())
    else
      Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
  else
    let pp_env_extension ppf env_extension =
      if not (TEE.is_empty env_extension) then
        Format.fprintf ppf "@ %a" TEE.print env_extension
    in
    let [@ocamlformat "disable"] print ppf { maps_to; index; env_extension } =
      Format.fprintf ppf "=> %a,@ %a%a"
        print_index index
        Maps_to.print maps_to
        pp_env_extension env_extension
    in
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(known_tags@ %a)@]@ \
         @[<hov 1>(other_tags@ %a)@]\
         )@]"
      (Tag.Map.print print) known_tags
      (Or_bottom.print print) other_tags

let rec all_ids_for_export t =
  match t with
  | Value ty -> TD.all_ids_for_export ty
  | Naked_immediate ty -> TD.all_ids_for_export ty
  | Naked_float ty -> T_Nf.all_ids_for_export ty
  | Naked_int32 ty -> T_N32.all_ids_for_export ty
  | Naked_int64 ty -> T_N64.all_ids_for_export ty
  | Naked_nativeint ty -> TD.all_ids_for_export ty

and all_ids_for_export_head_of_kind_value head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Ids_for_export.union
      (Or_unknown.all_ids_for_export Blocks.all_ids_for_export blocks)
      (Or_unknown.all_ids_for_export T.all_ids_for_export immediates)
  | Boxed_float ty -> T.all_ids_for_export ty
  | Boxed_int32 ty -> T.all_ids_for_export ty
  | Boxed_int64 ty -> T.all_ids_for_export ty
  | Boxed_nativeint ty -> T.all_ids_for_export ty
  | Closures { by_closure_id } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.all_ids_for_export
      by_closure_id
  | String _ -> Ids_for_export.empty
  | Array { length } -> T.all_ids_for_export length
  | Rec_info ty -> TD.all_ids_for_export ty

and all_ids_for_export_head_of_kind_naked_immediate head =
  match t with
  | Naked_immediates _ -> Ids_for_export.empty
  | Is_int ty | Get_tag ty -> T.all_ids_for_export ty

and all_ids_for_export_row_like { known_tags; other_tags } =
  let from_known_tags =
    Tag.Map.fold
      (fun _tag { maps_to; env_extension; index = _ } ids ->
        Ids_for_export.union ids
          (Ids_for_export.union
             (Maps_to.all_ids_for_export maps_to)
             (TEE.all_ids_for_export env_extension)))
      known_tags Ids_for_export.empty
  in
  match other_tags with
  | Bottom -> from_known_tags
  | Ok { maps_to; env_extension; index = _ } ->
    Ids_for_export.union
      (Maps_to.all_ids_for_export maps_to)
      (Ids_for_export.union from_known_tags
         (TEE.all_ids_for_export env_extension))

let rec apply_coercion t coercion : t Or_bottom.t =
  match t with
  | Value ty ->
    TD.apply_coercion ~apply_coercion_head:apply_coercion_head_of_kind_value
      ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value coercion ty
  | Naked_immediate ty ->
    TD.apply_coercion
      ~apply_coercion_head:apply_coercion_head_of_kind_naked_immediate
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
      ~free_names_head:free_names_head_of_kind_naked_immediate coercion ty
  | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ ->
    if Coercion.is_id coercion then Ok t else Bottom
  | Rec_info ty ->
    TD.apply_coercion ~apply_coercion_head:Rec_info_expr.apply_coercion
      ~apply_renaming_head:Rec_info_expr.apply_renaming
      ~free_names_head:Rec_info_expr.free_names coercion ty

and apply_coercion_head_of_kind_value head coercion : _ Or_bottom.t =
  match head with
  | Closures { by_closure_id } -> begin
    match
      Row_like.For_closures_entry_by_set_of_closures_contents
      .map_function_decl_types by_closure_id
        ~f:(fun (decl : Function_declaration_type.t) : _ Or_bottom.t ->
          Function_declaration_type.apply_coercion decl coercion)
    with
    | Bottom -> Bottom
    | Ok by_closure_id -> (
      match
        Row_like.For_closures_entry_by_set_of_closures_contents
        .map_closure_types by_closure_id ~f:(fun ty ->
            Type_grammar.apply_coercion ty coercion)
      with
      | Bottom -> Bottom
      | Ok by_closure_id -> Ok (Closures { by_closure_id }))
  end
  | Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_nativeint _ | String _ | Array _ ->
    if Coercion.is_id coercion then Ok t else Bottom

let kind _ = ()

let create_variant ~is_unique ~immediates ~blocks =
  begin
    match immediates with
    | Or_unknown.Unknown -> ()
    | Or_unknown.Known immediates ->
      if not (K.equal (kind immediates) K.naked_immediate)
      then
        Misc.fatal_errorf
          "Cannot create [immediates] with type that is not of kind \
           [Naked_immediate]:@ %a"
          print immediates
  end;
  Variant { immediates; blocks; is_unique }

module Row_like = struct
  let create_bottom () = { known_tags = Tag.Map.empty; other_tags = Bottom }

  let create_exactly tag index maps_to =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to; index = Known index; env_extension = TEE.empty () };
      other_tags = Bottom
    }

  let create_at_least tag index maps_to =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to; index = At_least index; env_extension = TEE.empty () };
      other_tags = Bottom
    }

  let create_at_least_unknown_tag index maps_to =
    { known_tags = Tag.Map.empty;
      other_tags =
        Ok { maps_to; index = At_least index; env_extension = TEE.empty () }
    }

  let is_bottom { known_tags; other_tags } =
    Tag.Map.is_empty known_tags && other_tags = Or_bottom.Bottom

  let get_singleton { known_tags; other_tags } =
    match other_tags with
    | Ok _ -> None
    | Bottom -> (
      match Tag.Map.get_singleton known_tags with
      | None -> None
      | Some (tag, { maps_to; index; env_extension = _ }) -> (
        (* If this is a singleton all the information from the env_extension is
           already part of the environment *)
        match index with
        | At_least _ -> None
        | Known index -> Some ((tag, index), maps_to)))

  let all_tags { known_tags; other_tags } : Tag.Set.t Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.keys known_tags)

  let all_tags_and_indexes { known_tags; other_tags } : _ Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.map (fun case -> case.index) known_tags)

  let map_maps_to { known_tags; other_tags }
      ~(f : Maps_to.t -> Maps_to.t Or_bottom.t) : _ Or_bottom.t =
    let known_tags =
      Tag.Map.filter_map
        (fun _ case ->
          match f case.maps_to with
          | Bottom -> None
          | Ok maps_to -> Some { case with maps_to })
        known_tags
    in
    let other_tags : case Or_bottom.t =
      match other_tags with
      | Bottom -> Bottom
      | Ok case ->
        Or_bottom.map (f case.maps_to) ~f:(fun maps_to -> { case with maps_to })
    in
    let result = { known_tags; other_tags } in
    if is_bottom result then Bottom else Ok result
end

let force_to_kind_value t =
  match t with
  | Value ty -> ty
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a" print t

let force_to_kind_naked_immediate t =
  match t with
  | Naked_immediate ty -> ty
  | Value _ | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_immediate]):@ %a"
      print t

let force_to_kind_naked_float t =
  match t with
  | Naked_float ty -> ty
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_float]):@ %a" print
      t

let force_to_kind_naked_int32 t =
  match t with
  | Naked_int32 ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_int32]):@ %a" print
      t

let force_to_kind_naked_int64 t =
  match t with
  | Naked_int64 ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_number Int64]):@ %a"
      print t

let force_to_kind_naked_nativeint t =
  match t with
  | Naked_nativeint ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Rec_info _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Nativeint]):@ %a" print t

let force_to_kind_rec_info t =
  match t with
  | Rec_info ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Rec_info]):@ %a" print t

let force_to_head ~force_to_kind t =
  match descr (force_to_kind t) with
  | No_alias head -> head
  | Equals _ -> Misc.fatal_errorf "Expected [No_alias]:@ %a" T.print t

let kind t =
  match t with
  | Value _ -> K.value
  | Naked_immediate _ -> K.naked_immediate
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Rec_info _ -> K.rec_info

let get_alias_exn t =
  match t with
  | Value ty -> TD.get_alias_exn ty
  | Naked_immediate ty -> TD.get_alias_exn ty
  | Naked_float ty -> T_Nf.get_alias_exn ty
  | Naked_int32 ty -> T_N32.get_alias_exn ty
  | Naked_int64 ty -> T_N64.get_alias_exn ty
  | Naked_nativeint ty -> TD.get_alias_exn ty
  | Rec_info ty -> TD.get_alias_exn ty

(* CR mshinwell: We should have transformations and invariant checks to enforce
   that, when a type can be expressed just using [Equals] (e.g. to a tagged
   immediate [Simple]), then it should be. In the tagged immediate case this
   would mean forbidding Variant with only a single immediate. Although this
   state needs to exist during [meet] or whenever heads are expanded. *)

let is_obviously_bottom t =
  match t with
  | Value ty -> TD.is_obviously_bottom ty
  | Naked_immediate ty -> TD.is_obviously_bottom ty
  | Naked_float ty -> TD.is_obviously_bottom ty
  | Naked_int32 ty -> TD.is_obviously_bottom ty
  | Naked_int64 ty -> TD.is_obviously_bottom ty
  | Naked_nativeint ty -> TD.is_obviously_bottom ty
  | Rec_info ty -> TD.is_obviously_bottom ty

let is_obviously_unknown t =
  match t with
  | Value ty -> TD.is_obviously_unknown ty
  | Naked_immediate ty -> TD.is_obviously_unknown ty
  | Naked_float ty -> TD.is_obviously_unknown ty
  | Naked_int32 ty -> TD.is_obviously_unknown ty
  | Naked_int64 ty -> TD.is_obviously_unknown ty
  | Naked_nativeint ty -> TD.is_obviously_unknown ty
  | Rec_info ty -> TD.is_obviously_unknown ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value -> Value (TD.create_equals name)
  | Naked_number Naked_immediate -> Naked_immediate (TD.create_equals name)
  | Naked_number Naked_float -> Naked_float (TD.create_equals name)
  | Naked_number Naked_int32 -> Naked_int32 (TD.create_equals name)
  | Naked_number Naked_int64 -> Naked_int64 (TD.create_equals name)
  | Naked_number Naked_nativeint -> Naked_nativeint (TD.create_equals name)
  | Rec_info -> Rec_info (TD.create_equals name)
  | Fabricated -> Misc.fatal_error "Unused kind, to be removed"

let bottom_value = Value TD.bottom

let bottom_naked_immediate = Naked_immediate TD.bottom

let bottom_naked_float = Naked_float TD.bottom

let bottom_naked_int32 = Naked_int32 TD.bottom

let bottom_naked_int64 = Naked_int64 TD.bottom

let bottom_naked_nativeint = Naked_nativeint TD.bottom

let bottom_rec_info = Rec_info TD.bottom

let bottom (kind : K.t) =
  match kind with
  | Value -> bottom_value
  | Naked_number Naked_immediate -> bottom_naked_immediate
  | Naked_number Naked_float -> bottom_naked_float
  | Naked_number Naked_int32 -> bottom_naked_int32
  | Naked_number Naked_int64 -> bottom_naked_int64
  | Naked_number Naked_nativeint -> bottom_naked_nativeint
  | Rec_info -> bottom_rec_info
  | Fabricated -> Misc.fatal_error "Unused kind to be removed"

let bottom_like t = bottom (kind t)

let any_value = Value TD.unknown

let any_naked_immediate = Naked_immediate TD.unknown

let any_naked_float = Naked_float TD.unknown

let any_naked_int32 = Naked_int32 TD.unknown

let any_naked_int64 = Naked_int64 TD.unknown

let any_naked_nativeint = Naked_nativeint TD.unknown

let any_rec_info = Rec_info TD.unknown

let unknown (kind : K.t) =
  match kind with
  | Value -> any_value
  | Naked_number Naked_immediate -> any_naked_immediate
  | Naked_number Naked_float -> any_naked_float
  | Naked_number Naked_int32 -> any_naked_int32
  | Naked_number Naked_int64 -> any_naked_int64
  | Naked_number Naked_nativeint -> any_naked_nativeint
  | Rec_info -> any_rec_info
  | Fabricated -> Misc.fatal_error "Unused kind to be removed"

let unknown_like t = unknown (kind t)

let this_naked_immediate i : t =
  Naked_immediate
    (TD.create_equals (Simple.const (Reg_width_const.naked_immediate i)))

let this_naked_float f : t =
  Naked_float
    (T_Nf.create_equals (Simple.const (Reg_width_const.naked_float f)))

let this_naked_int32 i : t =
  Naked_int32
    (T_N32.create_equals (Simple.const (Reg_width_const.naked_int32 i)))

let this_naked_int64 i : t =
  Naked_int64
    (T_N64.create_equals (Simple.const (Reg_width_const.naked_int64 i)))

let this_naked_nativeint i : t =
  Naked_nativeint
    (TD.create_equals (Simple.const (Reg_width_const.naked_nativeint i)))

let these_naked_immediates0 ~no_alias is =
  match Targetint_31_63.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_immediate i
  | _ ->
    if Targetint_31_63.Set.is_empty is
    then bottom K.naked_immediate
    else Naked_immediate (TD.create (Ok (Naked_immediates is)))

let these_naked_floats0 ~no_alias fs =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs
    then bottom K.naked_float
    else Naked_float (T_Nf.create (Ok fs))

let these_naked_int32s0 ~no_alias is =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is
    then bottom K.naked_int32
    else Naked_int32 (T_N32.create (Ok is))

let these_naked_int64s0 ~no_alias is =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is
    then bottom K.naked_int64
    else Naked_int64 (T_N64.create (Ok is))

let these_naked_nativeints0 ~no_alias is =
  match Targetint_32_64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint_32_64.Set.is_empty is
    then bottom K.naked_nativeint
    else Naked_nativeint (TD.create (Ok is))

let this_naked_immediate_without_alias i =
  these_naked_immediates0 ~no_alias:true (Targetint_31_63.Set.singleton i)

let this_naked_float_without_alias f =
  these_naked_floats0 ~no_alias:true (Float.Set.singleton f)

let this_naked_int32_without_alias i =
  these_naked_int32s0 ~no_alias:true (Int32.Set.singleton i)

let this_naked_int64_without_alias i =
  these_naked_int64s0 ~no_alias:true (Int64.Set.singleton i)

let this_naked_nativeint_without_alias i =
  these_naked_nativeints0 ~no_alias:true (Targetint_32_64.Set.singleton i)

let these_naked_immediates is = these_naked_immediates0 ~no_alias:false is

let these_naked_floats fs = these_naked_floats0 ~no_alias:false fs

let these_naked_int32s is = these_naked_int32s0 ~no_alias:false is

let these_naked_int64s is = these_naked_int64s0 ~no_alias:false is

let these_naked_nativeints is = these_naked_nativeints0 ~no_alias:false is

let box_float (t : t) : t =
  match t with
  | Naked_float _ -> Value (TD.create (Boxed_float t))
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a" print t

let box_int32 (t : t) : t =
  match t with
  | Naked_int32 _ -> Value (TD.create (Boxed_int32 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a" print t

let box_int64 (t : t) : t =
  match t with
  | Naked_int64 _ -> Value (TD.create (Boxed_int64 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a" print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_nativeint _ -> Value (TD.create (Boxed_nativeint t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a" print t

let any_tagged_immediate () : t =
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:Unknown
                ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))

let this_tagged_immediate imm : t =
  Value (TD.create_equals (Simple.const (Reg_width_const.tagged_immediate imm)))

let these_tagged_immediates0 ~no_alias imms : t =
  match Targetint_31_63.Set.get_singleton imms with
  | Some imm when not no_alias -> this_tagged_immediate imm
  | _ ->
    if Targetint_31_63.Set.is_empty imms
    then bottom K.value
    else
      Value
        (TD.create
           (Ok
              (Variant
                 (Variant.create ~is_unique:false
                    ~immediates:(Known (these_naked_immediates imms))
                    ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))

let these_tagged_immediates imms = these_tagged_immediates0 ~no_alias:false imms

let this_tagged_immediate_without_alias imm =
  these_tagged_immediates0 ~no_alias:true (Targetint_31_63.Set.singleton imm)

let tag_immediate t : t =
  match t with
  | Naked_immediate _ ->
    Value
      (TD.create
         (Ok
            (Variant
               (Variant.create ~is_unique:false ~immediates:(Known t)
                  ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))
  | Value _ | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a" print t

let tagged_immediate_alias_to ~naked_immediate : t =
  tag_immediate
    (Naked_immediate (TD.create_equals (Simple.var naked_immediate)))

let any_block () : t =
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false
                ~immediates:(Known (bottom K.naked_immediate))
                ~blocks:Unknown))))

let is_int_for_scrutinee ~scrutinee : t =
  Naked_immediate (TD.create (Is_int (alias_type_of K.value scrutinee)))

let get_tag_for_block ~block : t =
  Naked_immediate (TD.create (Get_tag (alias_type_of K.value block)))

let any_tagged_bool () = these_tagged_immediates Targetint_31_63.all_bools

let any_naked_bool () = these_naked_immediates Targetint_31_63.all_bools

let this_boxed_float f = box_float (this_naked_float f)

let this_boxed_int32 i = box_int32 (this_naked_int32 i)

let this_boxed_int64 i = box_int64 (this_naked_int64 i)

let this_boxed_nativeint i = box_nativeint (this_naked_nativeint i)

let these_boxed_floats fs = box_float (these_naked_floats fs)

let these_boxed_int32s is = box_int32 (these_naked_int32s is)

let these_boxed_int64s is = box_int64 (these_naked_int64s is)

let these_boxed_nativeints is = box_nativeint (these_naked_nativeints is)

let boxed_float_alias_to ~naked_float =
  box_float (Naked_float (T_Nf.create_equals (Simple.var naked_float)))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_int32 (T_N32.create_equals (Simple.var naked_int32)))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_int64 (T_N64.create_equals (Simple.var naked_int64)))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint
    (Naked_nativeint (TD.create_equals (Simple.var naked_nativeint)))

let blocks_with_these_tags tags : _ Or_unknown.t =
  if Tag.Set.for_all Tag.is_structured_block tags
  then
    let blocks =
      Row_like.For_blocks.create_blocks_with_these_tags
        ~field_kind:Flambda_kind.value tags
    in
    (* CR vlaviron: There is a potential soundness issue as this forbids Array
       values, which could have tag 0. *)
    Known
      (Value
         (TD.create
            (Ok
               (Variant
                  (Variant.create ~is_unique:false
                     ~immediates:(Known (bottom K.naked_immediate))
                     ~blocks:(Known blocks))))))
  else Unknown

let immutable_block ~is_unique tag ~field_kind ~fields =
  match Targetint_31_63.Imm.of_int_option (List.length fields) with
  | None ->
    (* CR mshinwell: This should be a special kind of error. *)
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    Value
      (TD.create
         (Ok
            (Variant
               (Variant.create ~is_unique
                  ~immediates:(Known (bottom K.naked_immediate))
                  ~blocks:
                    (Known
                       (Row_like.For_blocks.create ~field_kind ~field_tys:fields
                          (Closed tag)))))))

let immutable_block_with_size_at_least ~tag ~n ~field_kind ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then unknown field_kind
        else alias_type_of field_kind (Simple.var field_n_minus_one))
  in
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false
                ~immediates:(Known (bottom K.naked_immediate))
                ~blocks:
                  (Known
                     (Row_like.For_blocks.create ~field_kind ~field_tys
                        (Open tag)))))))

let variant ~const_ctors ~non_const_ctors =
  let blocks =
    let field_tys_by_tag =
      Tag.Scannable.Map.fold
        (fun tag ty non_const_ctors ->
          Tag.Map.add (Tag.Scannable.to_tag tag) ty non_const_ctors)
        non_const_ctors Tag.Map.empty
    in
    Row_like.For_blocks.create_exactly_multiple ~field_tys_by_tag
  in
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:(Known const_ctors)
                ~blocks:(Known blocks)))))

let open_variant_from_const_ctors_type ~const_ctors =
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:(Known const_ctors)
                ~blocks:Unknown))))

let open_variant_from_non_const_ctor_with_size_at_least ~n ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then any_value ()
        else alias_type_of K.value (Simple.var field_n_minus_one))
  in
  Value
    (TD.create
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:Unknown
                ~blocks:
                  (Known
                     (Row_like.For_blocks.create ~field_kind:K.value ~field_tys
                        (Open Unknown)))))))

let this_immutable_string str =
  (* CR mshinwell: Use "length" not "size" for strings *)
  let size = Targetint_31_63.Imm.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (TD.create (String string_info))

let mutable_string ~size =
  let size = Targetint_31_63.Imm.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (TD.create (String string_info))

let any_boxed_float () = box_float (any_naked_float ())

let any_boxed_int32 () = box_int32 (any_naked_int32 ())

let any_boxed_int64 () = box_int64 (any_naked_int64 ())

let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

let create_function_declaration code_id ~rec_info =
  Function_declaration_type.create code_id ~rec_info

let exactly_this_closure closure_id ~all_function_decls_in_set:function_decls
    ~all_closures_in_set:closure_types
    ~all_closure_vars_in_set:closure_var_types =
  let closure_types =
    Product.Closure_id_indexed.create Flambda_kind.value closure_types
  in
  let closures_entry =
    let closure_var_types =
      Product.Var_within_closure_indexed.create Flambda_kind.value
        closure_var_types
    in
    Closures_entry.create ~function_decls ~closure_types ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys function_decls)
        (Var_within_closure.Map.keys closure_var_types)
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_exactly
      closure_id set_of_closures_contents closures_entry
  in
  Value (TD.create (Closures { by_closure_id }))

let at_least_the_closures_with_ids ~this_closure closure_ids_and_bindings =
  let closure_ids_and_types =
    Closure_id.Map.map
      (fun bound_to -> alias_type_of K.value bound_to)
      closure_ids_and_bindings
  in
  let function_decls =
    Closure_id.Map.map
      (fun _ -> Or_unknown_or_bottom.Unknown)
      closure_ids_and_bindings
  in
  let closure_types =
    Product.Closure_id_indexed.create Flambda_kind.value closure_ids_and_types
  in
  let closures_entry =
    Closures_entry.create ~function_decls ~closure_types
      ~closure_var_types:
        (Product.Var_within_closure_indexed.create_top Flambda_kind.value)
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys closure_ids_and_types)
        Var_within_closure.Set.empty
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  Value (TD.create (Closures { by_closure_id }))

let closure_with_at_least_these_closure_vars ~this_closure closure_vars : t =
  let closure_var_types =
    let type_of_var v = alias_type_of K.value (Simple.var v) in
    let map = Var_within_closure.Map.map type_of_var closure_vars in
    Product.Var_within_closure_indexed.create Flambda_kind.value map
  in
  let closures_entry =
    Closures_entry.create ~function_decls:Closure_id.Map.empty
      ~closure_types:(Product.Closure_id_indexed.create_top Flambda_kind.value)
      ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create Closure_id.Set.empty
        (Var_within_closure.Map.keys closure_vars)
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  Value (TD.create (Closures { by_closure_id }))

let closure_with_at_least_this_closure_var ~this_closure closure_var
    ~closure_element_var : t =
  closure_with_at_least_these_closure_vars ~this_closure
    (Var_within_closure.Map.singleton closure_var closure_element_var)

let array_of_length ~length = Value (TD.create (Array { length }))

let type_for_const const =
  match Reg_width_const.descr const with
  | Naked_immediate i -> this_naked_immediate i
  | Tagged_immediate i -> this_tagged_immediate i
  | Naked_float f -> this_naked_float f
  | Naked_int32 n -> this_naked_int32 n
  | Naked_int64 n -> this_naked_int64 n
  | Naked_nativeint n -> this_naked_nativeint n

let kind_for_const const = kind (type_for_const const)

let this_rec_info (rec_info_expr : Rec_info_expr.t) =
  match rec_info_expr with
  | Var dv -> Rec_info (TD.create_equals (Simple.var dv))
  | Const _ | Succ _ | Unroll_to _ -> Rec_info (TD.create rec_info_expr)

let is_alias_of_name ty name =
  match get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' ~coercion:_ -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation name ty =
  if Flambda_features.check_invariants ()
  then
    if is_alias_of_name ty name
    then
      Misc.fatal_errorf "Directly recursive equation@ %a = %a@ disallowed"
        Name.print name print ty

(* Closures_entry *)

let function_decl_types t = t.function_decls

let closure_types t = PC.to_map t.closure_types

let closure_var_types t = PV.to_map t.closure_var_types

let find_function_declaration t closure_id : _ Or_bottom.t =
  match Closure_id.Map.find closure_id t.function_decls with
  | exception Not_found -> Bottom
  | func_decl -> Ok func_decl

let map_function_decl_types { function_decls; closure_types; closure_var_types }
    ~(f : FDT.t -> FDT.t Or_bottom.t) : _ Or_bottom.t =
  (* CR mshinwell: This needs to deal with [closure_types] too. Deferring until
     new approach for [Rec_info] is sorted out. *)
  let bottom = ref false in
  let function_decls =
    Closure_id.Map.map
      (fun function_decl ->
        match f function_decl with
        | Ok function_decl -> function_decl
        | Bottom ->
          bottom := true;
          function_decl)
      function_decls
  in
  if !bottom
  then Bottom
  else
    let t = { function_decls; closure_types; closure_var_types } in
    Ok t

let map_closure_types { function_decls; closure_types; closure_var_types }
    ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t) : _ Or_bottom.t =
  let closure_types = Product.Closure_id_indexed.map_types closure_types ~f in
  Or_bottom.map closure_types ~f:(fun closure_types ->
      { function_decls; closure_types; closure_var_types })

let fields_kind _ = Flambda_kind.value

(* Row_like *)
module For_blocks = struct
  module Tag_or_unknown = Tag_or_unknown_and_size.Tag_or_unknown
  include Make (Tag) (Targetint_ocaml_index) (Product.Int_indexed)

  type open_or_closed =
    | Open of Tag.t Or_unknown.t
    | Closed of Tag.t

  let create ~(field_kind : Flambda_kind.t) ~field_tys
      (open_or_closed : open_or_closed) =
    let field_kind' =
      List.map Type_grammar.kind field_tys
      |> Flambda_kind.Set.of_list |> Flambda_kind.Set.get_singleton
    in
    (* CR pchambart: move to invariant check *)
    begin
      match field_kind' with
      | None ->
        if List.length field_tys <> 0
        then Misc.fatal_error "[field_tys] must all be of the same kind"
      | Some field_kind' ->
        if not (Flambda_kind.equal field_kind field_kind')
        then
          Misc.fatal_errorf "Declared field kind %a doesn't match [field_tys]"
            Flambda_kind.print field_kind
    end;

    let tag : _ Or_unknown.t =
      let tag : _ Or_unknown.t =
        match open_or_closed with
        | Open (Known tag) -> Known tag
        | Open Unknown -> Unknown
        | Closed tag -> Known tag
      in
      match tag with
      | Unknown -> begin
        match field_kind with
        | Value -> Unknown
        | Naked_number Naked_float -> Known Tag.double_array_tag
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Fabricated | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind
      end
      | Known tag -> begin
        match field_kind with
        | Value -> begin
          match Tag.Scannable.of_tag tag with
          | Some _ -> Known tag
          | None ->
            Misc.fatal_error
              "Blocks full of [Value]s must have a tag less than [No_scan_tag]"
        end
        | Naked_number Naked_float ->
          if not (Tag.equal tag Tag.double_array_tag)
          then
            Misc.fatal_error
              "Blocks full of naked floats must have tag [Tag.double_array_tag]";
          Known tag
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Fabricated | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind
      end
    in
    let product = Product.Int_indexed.create_from_list field_kind field_tys in
    let size = Targetint_31_63.Imm.of_int (List.length field_tys) in
    match open_or_closed with
    | Open _ -> begin
      match tag with
      | Known tag -> create_at_least tag size product
      | Unknown -> create_at_least_unknown_tag size product
    end
    | Closed _ -> (
      match tag with
      | Known tag -> create_exactly tag size product
      | Unknown -> assert false)
  (* see above *)

  let create_blocks_with_these_tags ~field_kind tags =
    let maps_to = Product.Int_indexed.create_top field_kind in
    let case =
      { maps_to;
        index = At_least Targetint_31_63.Imm.zero;
        env_extension = TEE.empty ()
      }
    in
    { known_tags = Tag.Map.of_set (fun _ -> case) tags; other_tags = Bottom }

  let create_exactly_multiple ~field_tys_by_tag =
    let known_tags =
      Tag.Map.map
        (fun field_tys ->
          (* CR mshinwell: Validate [field_tys] like [create] does, above *)
          let field_kind =
            match field_tys with
            | [] -> Flambda_kind.value
            | field_ty :: _ -> Type_grammar.kind field_ty
          in
          let maps_to =
            Product.Int_indexed.create_from_list field_kind field_tys
          in
          let size = Targetint_31_63.Imm.of_int (List.length field_tys) in
          { maps_to; index = Known size; env_extension = TEE.empty () })
        field_tys_by_tag
    in
    { known_tags; other_tags = Bottom }

  let all_tags_and_sizes t : Targetint_31_63.Imm.t Tag.Map.t Or_unknown.t =
    match all_tags_and_indexes t with
    | Unknown -> Unknown
    | Known tags_and_indexes ->
      let any_unknown = ref false in
      let by_tag =
        Tag.Map.map
          (fun index ->
            match index with
            | Known index -> index
            | At_least index ->
              any_unknown := true;
              index)
          tags_and_indexes
      in
      if !any_unknown then Unknown else Known by_tag

  let get_field t field_index : _ Or_unknown_or_bottom.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, size), maps_to) -> (
      let index = Targetint_31_63.to_targetint field_index in
      if Targetint_31_63.Imm.( <= ) size index
      then Bottom
      else
        match
          Product.Int_indexed.project maps_to (Targetint_31_63.Imm.to_int index)
        with
        | Unknown -> Unknown
        | Known res -> Ok res)

  let get_variant_field t variant_tag field_index : _ Or_unknown_or_bottom.t =
    let index = Targetint_31_63.to_targetint field_index in
    let aux { index = size; maps_to; env_extension = _ } :
        _ Or_unknown_or_bottom.t =
      match size with
      | Known i when i <= index -> Bottom
      | _ -> (
        match
          Product.Int_indexed.project maps_to (Targetint_31_63.Imm.to_int index)
        with
        | Unknown -> Unknown
        | Known res -> Ok res)
    in
    match Tag.Map.find variant_tag t.known_tags with
    | case -> aux case
    | exception Not_found -> begin
      match t.other_tags with Bottom -> Bottom | Ok case -> aux case
    end
end

module For_closures_entry_by_set_of_closures_contents = struct
  include Make (Closure_id) (Set_of_closures_contents) (Closures_entry)

  let map_function_decl_types t ~f =
    map_maps_to t ~f:(fun closures_entry ->
        Closures_entry.map_function_decl_types closures_entry ~f)

  let map_closure_types t ~f =
    map_maps_to t ~f:(fun closures_entry ->
        Closures_entry.map_closure_types closures_entry ~f)

  let create_exactly (closure_id : Closure_id.t)
      (contents : Set_of_closures_contents.t)
      (closures_entry : Closures_entry.t) : t =
    let known_tags =
      Closure_id.Map.singleton closure_id
        { index = Known contents;
          maps_to = closures_entry;
          env_extension = TEE.empty ()
        }
    in
    { known_tags; other_tags = Bottom }

  let create_at_least (closure_id : Closure_id.t)
      (contents : Set_of_closures_contents.t)
      (closures_entry : Closures_entry.t) : t =
    let known_tags =
      Closure_id.Map.singleton closure_id
        { index = At_least contents;
          maps_to = closures_entry;
          env_extension = TEE.empty ()
        }
    in
    { known_tags; other_tags = Bottom }

  let get_env_var t env_var : _ Or_unknown.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, index), maps_to) ->
      if not
           (Var_within_closure.Set.mem env_var
              (Set_of_closures_contents.closure_vars index))
      then Unknown
      else
        let env_var_ty =
          try
            Var_within_closure.Map.find env_var
              (Closures_entry.closure_var_types maps_to)
          with Not_found ->
            Misc.fatal_errorf
              "Environment variable %a is bound in index but not in \
               maps_to@.Index:@ %a@.Maps_to:@ %a"
              Var_within_closure.print env_var Set_of_closures_contents.print
              index Closures_entry.print maps_to
        in
        Known env_var_ty
end
