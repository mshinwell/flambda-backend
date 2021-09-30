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
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module Row_like = Type_grammar.Row_like
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
  | Fabricated -> Misc.fatal_error "Unused kind to be removed"

let unknown_like t = unknown (kind t)

let bottom (kind : K.t) =
  match kind with
  | Value -> TG.bottom_value
  | Naked_number Naked_immediate -> TG.bottom_naked_immediate
  | Naked_number Naked_float -> TG.bottom_naked_float
  | Naked_number Naked_int32 -> TG.bottom_naked_int32
  | Naked_number Naked_int64 -> TG.bottom_naked_int64
  | Naked_number Naked_nativeint -> TG.bottom_naked_nativeint
  | Rec_info -> TG.bottom_rec_info
  | Fabricated -> Misc.fatal_error "Unused kind to be removed"

let bottom_like t = bottom (kind t)

let any_tagged_bool = TG.these_tagged_immediates Targetint_31_63.all_bools

let any_naked_bool = TG.these_naked_immediates Targetint_31_63.all_bools

let this_boxed_float f = TG.box_float (TG.this_naked_float f)

let this_boxed_int32 i = TG.box_int32 (TG.this_naked_int32 i)

let this_boxed_int64 i = TG.box_int64 (TG.this_naked_int64 i)

let this_boxed_nativeint i = TG.box_nativeint (TG.this_naked_nativeint i)

let these_boxed_floats fs = TG.box_float (TG.these_naked_floats fs)

let these_boxed_int32s is = TG.box_int32 (TG.these_naked_int32s is)

let these_boxed_int64s is = TG.box_int64 (TG.these_naked_int64s is)

let these_boxed_nativeints is = TG.box_nativeint (TG.these_naked_nativeints is)

let any_boxed_float = TG.box_float TG.any_naked_float

let any_boxed_int32 = TG.box_int32 TG.any_naked_int32

let any_boxed_int64 = TG.box_int64 TG.any_naked_int64

let any_boxed_nativeint = TG.box_nativeint TG.any_naked_nativeint

let immutable_block_with_size_at_least ~tag ~n ~field_kind ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then TG.unknown field_kind
        else TG.alias_type_of field_kind (Simple.var field_n_minus_one))
  in
  TG.create_variant ~is_unique:false
    ~immediates:(Known (TG.bottom K.naked_immediate))
    ~blocks:
      (Known (Row_like.For_blocks.create ~field_kind ~field_tys (Open tag)))

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
  TG.create_variant ~is_unique:false ~immediates:(Known const_ctors)
    ~blocks:(Known blocks)

let open_variant_from_const_ctors_type ~const_ctors =
  TG.create_variant ~is_unique:false ~immediates:(Known const_ctors)
    ~blocks:Unknown

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
         (Row_like.For_blocks.create ~field_kind:K.value ~field_tys
            (Open Unknown)))

let exactly_this_closure closure_id ~all_function_decls_in_set:function_decls
    ~all_closures_in_set:closure_types
    ~all_closure_vars_in_set:closure_var_types =
  let closure_types = { closure_id_components_by_index = closure_types } in
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
  let closure_id_components_by_index =
    Closure_id.Map.map
      (fun bound_to -> TG.alias_type_of K.value bound_to)
      closure_ids_and_bindings
  in
  let function_decls =
    Closure_id.Map.map
      (fun _ -> Or_unknown_or_bottom.Unknown)
      closure_ids_and_bindings
  in
  let closure_types = { closure_id_components_by_index } in
  let closures_entry =
    { function_decls;
      closure_types;
      closure_var_types = Product.Var_within_closure_indexed.create_top K.value
    }
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys closure_id_components_by_index)
        Var_within_closure.Set.empty
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  TG.create_closures by_closure_id

let closure_with_at_least_these_closure_vars ~this_closure closure_vars : t =
  let closure_var_types =
    let type_of_var v = TG.alias_type_of K.value (Simple.var v) in
    let var_within_closure_components_by_index =
      Var_within_closure.Map.map type_of_var closure_vars
    in
    { var_within_closure_components_by_index }
  in
  let closures_entry =
    { function_decls = Closure_id.Map.empty;
      closure_types = Product.Closure_id_indexed.create_top K.value;
      closure_var_types
    }
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create Closure_id.Set.empty
        (Var_within_closure.Map.keys closure_vars)
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  TG.create_closures by_closure_id

let closure_with_at_least_this_closure_var ~this_closure closure_var
    ~closure_element_var : t =
  closure_with_at_least_these_closure_vars ~this_closure
    (Var_within_closure.Map.singleton closure_var closure_element_var)
