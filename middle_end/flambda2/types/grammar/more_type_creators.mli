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

(** Type creation functions that augment the ones in [Type_grammar] but do not
    require direct access to the representation. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module K = Flambda_kind
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module RWC = Reg_width_const
module TG = Type_grammar

val unknown : K.t -> TG.t

val unknown_like : TG.t -> TG.t

val bottom : K.t -> TG.t

val bottom_like : TG.t -> TG.t

val this_naked_immediate_without_alias : Targetint_31_63.t -> TG.t

val this_naked_float_without_alias : Float.t -> TG.t

val this_naked_int32_without_alias : int32 -> TG.t

val this_naked_int64_without_alias : int64 -> TG.t

val this_naked_nativeint_without_alias : Targetint_32_64.t -> TG.t

val these_naked_immediates : Targetint_31_63.Set.t -> TG.t

val these_naked_floats : TG.head_of_kind_naked_float -> TG.t

val these_naked_int32s : TG.head_of_kind_naked_int32 -> TG.t

val these_naked_int64s : TG.head_of_kind_naked_int64 -> TG.t

val these_naked_nativeints : TG.head_of_kind_naked_nativeint -> TG.t

val any_tagged_immediate : TG.t

val these_tagged_immediates0 : no_alias:bool -> Targetint_31_63.Set.t -> TG.t

val these_tagged_immediates : Targetint_31_63.Set.t -> TG.t

val this_tagged_immediate_without_alias : Targetint_31_63.t -> TG.t

val any_tagged_bool : TG.t

val any_naked_bool : no_alias:bool -> TG.t

val this_boxed_float : Float.t -> TG.t

val this_boxed_int32 : int32 -> TG.t

val this_boxed_int64 : int64 -> TG.t

val this_boxed_nativeint : Targetint_32_64.t -> TG.t

val these_boxed_floats : TG.head_of_kind_naked_float -> TG.t

val these_boxed_int32s : TG.head_of_kind_naked_int32 -> TG.t

val these_boxed_int64s : TG.head_of_kind_naked_int64 -> TG.t

val these_boxed_nativeints : TG.head_of_kind_naked_nativeint -> TG.t

val any_boxed_float : TG.t

val any_boxed_int32 : TG.t

val any_boxed_int64 : TG.t

val any_boxed_nativeint : TG.t

val any_block : TG.t

val blocks_with_these_tags : Tag.Set.t -> TG.t Or_unknown.t

val immutable_block :
  is_unique:bool -> Tag.t -> field_kind:K.t -> fields:TG.t list -> TG.t

val immutable_block_with_size_at_least :
  tag:Tag.t Or_unknown.t ->
  n:Targetint_31_63.Imm.t ->
  field_kind:K.t ->
  field_n_minus_one:Reg_width_things.Variable.t ->
  TG.t

val variant :
  const_ctors:TG.t -> non_const_ctors:TG.t list Tag.Scannable.Map.t -> TG.t

val open_variant_from_const_ctors_type : const_ctors:TG.t -> TG.t

val open_variant_from_non_const_ctor_with_size_at_least :
  n:Targetint_31_63.Imm.t ->
  field_n_minus_one:Reg_width_things.Variable.t ->
  TG.t

val exactly_this_closure :
  Closure_id.t ->
  all_function_decls_in_set:
    TG.function_type Or_unknown_or_bottom.t Closure_id.Map.t ->
  all_closures_in_set:TG.t Closure_id.Map.t ->
  all_closure_vars_in_set:TG.t Var_within_closure.Map.t ->
  TG.t

val at_least_the_closures_with_ids :
  this_closure:Closure_id.t -> Simple.t Closure_id.Map.t -> TG.t

val closure_with_at_least_these_closure_vars :
  this_closure:Closure_id.t ->
  Reg_width_things.Variable.t Var_within_closure.Map.t ->
  TG.t

val closure_with_at_least_this_closure_var :
  this_closure:Closure_id.t ->
  Var_within_closure.t ->
  closure_element_var:Reg_width_things.Variable.t ->
  TG.t

val type_for_const : RWC.t -> TG.t

val kind_for_const : RWC.t -> K.t

val is_alias_of_name : TG.t -> Name.t -> bool

val check_equation : Name.t -> TG.t -> unit
