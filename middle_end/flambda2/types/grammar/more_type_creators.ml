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

module K = Flambda_kind
module RWC = Reg_width_const
module TG = Type_grammar

let unknown (kind : K.t) =
  match kind with
  | Value -> TG.any_value
  | Naked_number Naked_immediate -> TG.any_naked_immediate
  | Naked_number Naked_float -> TG.any_naked_float
  | Naked_number Naked_int32 -> TG.any_naked_int32
  | Naked_number Naked_int64 -> TG.any_naked_int64
  | Naked_number Naked_nativeint -> TG.any_naked_nativeint
  | Rec_info -> TG.any_rec_info
  | Region -> TG.any_region

let unknown_like t = unknown (TG.kind t)

let bottom (kind : K.t) =
  match kind with
  | Value -> TG.bottom_value
  | Naked_number Naked_immediate -> TG.bottom_naked_immediate
  | Naked_number Naked_float -> TG.bottom_naked_float
  | Naked_number Naked_int32 -> TG.bottom_naked_int32
  | Naked_number Naked_int64 -> TG.bottom_naked_int64
  | Naked_number Naked_nativeint -> TG.bottom_naked_nativeint
  | Rec_info -> TG.bottom_rec_info
  | Region -> TG.bottom_region

let bottom_like t = bottom (TG.kind t)

(* CR mshinwell: can remove [no_alias] now *)

let these_naked_immediates is = TG.these_naked_immediates ~no_alias:false is

let these_naked_floats fs = TG.these_naked_floats ~no_alias:false fs

let these_naked_int32s is = TG.these_naked_int32s ~no_alias:false is

let these_naked_int64s is = TG.these_naked_int64s ~no_alias:false is

let these_naked_nativeints is = TG.these_naked_nativeints ~no_alias:false is

let any_tagged_immediate =
  TG.create_variant ~is_unique:false ~immediates:Unknown
    ~blocks:(Known TG.Row_like_for_blocks.bottom) (Known Heap)

let these_tagged_immediates0 ~no_alias imms =
  match Targetint_31_63.Set.get_singleton imms with
  | Some imm when not no_alias -> TG.this_tagged_immediate imm
  | _ ->
    if Targetint_31_63.Set.is_empty imms
    then TG.bottom_value
    else
      TG.create_variant ~is_unique:false
        ~immediates:(Known (these_naked_immediates imms))
        ~blocks:(Known TG.Row_like_for_blocks.bottom) (Known Heap)

let these_tagged_immediates imms = these_tagged_immediates0 ~no_alias:false imms

let any_tagged_bool = these_tagged_immediates Targetint_31_63.all_bools

let any_naked_bool =
  TG.these_naked_immediates ~no_alias:false Targetint_31_63.all_bools

let this_boxed_float f alloc_mode =
  TG.box_float (TG.this_naked_float f) alloc_mode

let this_boxed_int32 i alloc_mode =
  TG.box_int32 (TG.this_naked_int32 i) alloc_mode

let this_boxed_int64 i alloc_mode =
  TG.box_int64 (TG.this_naked_int64 i) alloc_mode

let this_boxed_nativeint i alloc_mode =
  TG.box_nativeint (TG.this_naked_nativeint i) alloc_mode

let these_boxed_floats fs alloc_mode =
  TG.box_float (these_naked_floats fs) alloc_mode

let these_boxed_int32s is alloc_mode =
  TG.box_int32 (these_naked_int32s is) alloc_mode

let these_boxed_int64s is alloc_mode =
  TG.box_int64 (these_naked_int64s is) alloc_mode

let these_boxed_nativeints is alloc_mode =
  TG.box_nativeint (these_naked_nativeints is) alloc_mode

let any_boxed_float = TG.box_float TG.any_naked_float Unknown

let any_boxed_int32 = TG.box_int32 TG.any_naked_int32 Unknown

let any_boxed_int64 = TG.box_int64 TG.any_naked_int64 Unknown

let any_boxed_nativeint = TG.box_nativeint TG.any_naked_nativeint Unknown

let any_block =
  TG.create_variant ~is_unique:false
    ~immediates:(Known TG.bottom_naked_immediate) ~blocks:Unknown Unknown

let blocks_with_these_tags tags : _ Or_unknown.t =
  if not (Tag.Set.for_all Tag.is_structured_block tags)
  then Unknown
  else
    let blocks =
      TG.Row_like_for_blocks.create_blocks_with_these_tags ~field_kind:K.value
        tags
    in
    (* CR vlaviron: There is a potential soundness issue as this forbids Array
       values, which could have tag 0. *)
    Known
      (TG.create_variant ~is_unique:false
         ~immediates:(Known TG.bottom_naked_immediate) ~blocks:(Known blocks)
         Unknown)

let immutable_block ~is_unique tag ~field_kind alloc_mode ~fields =
  match Targetint_31_63.Imm.of_int_option (List.length fields) with
  | None ->
    (* CR mshinwell: This should be a special kind of error. *)
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    TG.create_variant ~is_unique ~immediates:(Known TG.bottom_naked_immediate)
      ~blocks:
        (Known
           (TG.Row_like_for_blocks.create ~field_kind ~field_tys:fields
              (Closed tag)))
      alloc_mode

let immutable_block_with_size_at_least ~tag ~n ~field_kind ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then unknown field_kind
        else TG.alias_type_of field_kind (Simple.var field_n_minus_one))
  in
  TG.create_variant ~is_unique:false
    ~immediates:(Known (bottom K.naked_immediate))
    ~blocks:
      (Known (TG.Row_like_for_blocks.create ~field_kind ~field_tys (Open tag)))
    Unknown

let variant ~const_ctors ~non_const_ctors alloc_mode =
  let blocks =
    let field_tys_by_tag =
      Tag.Scannable.Map.fold
        (fun tag ty non_const_ctors ->
          Tag.Map.add (Tag.Scannable.to_tag tag) ty non_const_ctors)
        non_const_ctors Tag.Map.empty
    in
    TG.Row_like_for_blocks.create_exactly_multiple ~field_tys_by_tag
  in
  TG.create_variant ~is_unique:false ~immediates:(Known const_ctors)
    ~blocks:(Known blocks) alloc_mode

let open_variant_from_const_ctors_type ~const_ctors =
  TG.create_variant ~is_unique:false ~immediates:(Known const_ctors)
    ~blocks:Unknown Unknown

let open_variant_from_non_const_ctor_with_size_at_least ~n ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then TG.any_value
        else TG.alias_type_of K.value (Simple.var field_n_minus_one))
  in
  TG.create_variant ~is_unique:false ~immediates:Unknown
    ~blocks:
      (Known
         (TG.Row_like_for_blocks.create ~field_kind:K.value ~field_tys
            (Open Unknown)))
    Unknown

let exactly_this_closure function_slot ~all_function_slots_in_set:function_types
    ~all_closure_types_in_set:closure_types
    ~all_value_slots_in_set:value_slot_types alloc_mode =
  let closure_types = TG.Product.Function_slot_indexed.create closure_types in
  let closures_entry =
    let value_slot_types =
      TG.Product.Value_slot_indexed.create value_slot_types
    in
    TG.Closures_entry.create ~function_types ~closure_types ~value_slot_types
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Function_slot.Map.keys function_types)
        (Value_slot.Map.keys value_slot_types)
    in
    TG.Row_like_for_closures.create_exactly function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures alloc_mode by_function_slot

let closure_with_at_least_these_function_slots ~this_function_slot
    function_slots_and_bindings =
  let function_slot_components_by_index =
    Function_slot.Map.map
      (fun bound_to -> TG.alias_type_of K.value bound_to)
      function_slots_and_bindings
  in
  let function_types =
    Function_slot.Map.map
      (fun _ -> Or_unknown_or_bottom.Unknown)
      function_slots_and_bindings
  in
  let closure_types =
    TG.Product.Function_slot_indexed.create function_slot_components_by_index
  in
  let closures_entry =
    TG.Closures_entry.create ~function_types ~closure_types
      ~value_slot_types:TG.Product.Value_slot_indexed.top
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Function_slot.Map.keys function_slot_components_by_index)
        Value_slot.Set.empty
    in
    TG.Row_like_for_closures.create_at_least this_function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures Unknown by_function_slot

let closure_with_at_least_these_value_slots ~this_function_slot value_slots =
  let value_slot_types =
    let type_of_var v = TG.alias_type_of K.value (Simple.var v) in
    let value_slot_components_by_index =
      Value_slot.Map.map type_of_var value_slots
    in
    TG.Product.Value_slot_indexed.create value_slot_components_by_index
  in
  let closures_entry =
    TG.Closures_entry.create ~function_types:Function_slot.Map.empty
      ~closure_types:TG.Product.Function_slot_indexed.top ~value_slot_types
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create Function_slot.Set.empty
        (Value_slot.Map.keys value_slots)
    in
    TG.Row_like_for_closures.create_at_least this_function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures Unknown by_function_slot

let closure_with_at_least_this_value_slot ~this_function_slot value_slot
    ~value_slot_var =
  closure_with_at_least_these_value_slots ~this_function_slot
    (Value_slot.Map.singleton value_slot value_slot_var)

let type_for_const const =
  match RWC.descr const with
  | Naked_immediate i -> TG.this_naked_immediate i
  | Tagged_immediate i -> TG.this_tagged_immediate i
  | Naked_float f -> TG.this_naked_float f
  | Naked_int32 n -> TG.this_naked_int32 n
  | Naked_int64 n -> TG.this_naked_int64 n
  | Naked_nativeint n -> TG.this_naked_nativeint n

let kind_for_const const = TG.kind (type_for_const const)

let is_alias_of_name ty name =
  match TG.get_alias_exn ty with
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
        Name.print name TG.print ty

let arity_of_list ts = Flambda_arity.create (List.map TG.kind ts)

let unknown_types_from_arity arity = List.map (fun kind -> unknown kind) arity

let rec unknown_with_descr (descr : Flambda_kind.With_subkind.descr) =
  match descr with
  | Any_value -> TG.any_value
  | Naked_number Naked_immediate -> TG.any_naked_immediate
  | Naked_number Naked_float -> TG.any_naked_float
  | Naked_number Naked_int32 -> TG.any_naked_int32
  | Naked_number Naked_int64 -> TG.any_naked_int64
  | Naked_number Naked_nativeint -> TG.any_naked_nativeint
  | Boxed_float -> any_boxed_float
  | Boxed_int32 -> any_boxed_int32
  | Boxed_int64 -> any_boxed_int64
  | Boxed_nativeint -> any_boxed_nativeint
  | Tagged_immediate -> any_tagged_immediate
  | Rec_info -> TG.any_rec_info
  | Block { tag; fields } ->
    assert (not (Tag.equal tag Tag.double_array_tag));
    immutable_block ~is_unique:false tag ~field_kind:Flambda_kind.value
      ~fields:(List.map unknown_with_descr fields)
      Unknown
  | Float_block { num_fields } ->
    immutable_block ~is_unique:false Tag.double_array_tag
      ~field_kind:Flambda_kind.naked_float
      ~fields:(List.init num_fields (fun _ -> TG.any_naked_float))
      Unknown
  | Float_array ->
    TG.array_of_length
      ~element_kind:(Known Flambda_kind.With_subkind.naked_float)
      ~length:any_tagged_immediate
  | Immediate_array ->
    TG.array_of_length
      ~element_kind:(Known Flambda_kind.With_subkind.tagged_immediate)
      ~length:any_tagged_immediate
  | Value_array ->
    TG.array_of_length ~element_kind:(Known Flambda_kind.With_subkind.any_value)
      ~length:any_tagged_immediate
  | Generic_array ->
    TG.array_of_length ~element_kind:Unknown ~length:any_tagged_immediate

let unknown_with_subkind kind =
  unknown_with_descr (Flambda_kind.With_subkind.descr kind)

let unknown_types_from_arity_with_subkinds arity =
  List.map (fun kind -> unknown_with_subkind kind) arity

let bottom_types_from_arity arity = List.map (fun kind -> bottom kind) arity
