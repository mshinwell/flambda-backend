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
module TG = Flambda2_types.Type_grammar

val unknown : K.t -> 'a

val unknown_like : 'a -> 'b

val bottom : K.t -> 'a

val bottom_like : 'a -> 'b

val any_tagged_bool : 'a

val any_naked_bool : 'a

val this_naked_immediate_without_alias : Targetint_31_63.t -> 'a

val this_naked_float_without_alias : Float.t -> 'a

val this_naked_int32_without_alias : int32 -> 'a

val this_naked_int64_without_alias : int64 -> 'a

val this_naked_nativeint_without_alias : Targetint_32_64.t -> 'a

val these_naked_immediates : 'a -> 'b

val these_naked_floats : 'a -> 'b

val these_naked_int32s : 'a -> 'b

val these_naked_int64s : 'a -> 'b

val these_naked_nativeints : 'a -> 'b

val this_boxed_float : 'a -> 'b

val this_boxed_int32 : 'a -> 'b

val this_boxed_int64 : 'a -> 'b

val this_boxed_nativeint : 'a -> 'b

val these_boxed_floats : 'a -> 'b

val these_boxed_int32s : 'a -> 'b

val these_boxed_int64s : 'a -> 'b

val these_boxed_nativeints : 'a -> 'b

val any_boxed_float : 'a

val any_boxed_int32 : 'a

val any_boxed_int64 : 'a

val any_boxed_nativeint : 'a

val any_tagged_immediate : 'a

val these_tagged_immediates0 : no_alias:'a -> 'b -> 'c

val these_tagged_immediates : 'a -> 'b

val this_tagged_immediate_without_alias : Targetint_31_63.t -> 'a

val any_block : 'a

(** Create a shape of blocks, for use as a meet constraint.

    Special tags are approximated to Unknown, as there is no good way to
    represent their shapes exactly.

    In particular, creating a Variant shape for a special tag is unsound, as it
    forbids the other special shapes that could apply. *)
val blocks_with_these_tags : Tag.Set.t -> 'a Or_unknown.t

val immutable_block :
  is_unique:'a -> 'b -> field_kind:'c -> fields:'d list -> 'e

val immutable_block_with_size_at_least :
  tag:'a ->
  n:Targetint_31_63.Imm.t ->
  field_kind:'b ->
  field_n_minus_one:Reg_width_things.Variable.t ->
  'c

val variant : const_ctors:'a -> non_const_ctors:'b Tag.Scannable.Map.t -> 'c

val open_variant_from_const_ctors_type : const_ctors:'a -> 'b

val open_variant_from_non_const_ctor_with_size_at_least :
  n:Targetint_31_63.Imm.t -> field_n_minus_one:Reg_width_things.Variable.t -> 'a

val exactly_this_closure :
  'a ->
  all_function_decls_in_set:'b Closure_id.Map.t ->
  all_closures_in_set:'c ->
  all_closure_vars_in_set:'d Var_within_closure.Map.t ->
  'e

val at_least_the_closures_with_ids :
  this_closure:'a -> 'b Closure_id.Map.t -> 'c

val closure_with_at_least_these_closure_vars :
  this_closure:'a -> Reg_width_things.Variable.t Var_within_closure.Map.t -> 'b

val closure_with_at_least_this_closure_var :
  this_closure:'a ->
  Var_within_closure.t ->
  closure_element_var:Reg_width_things.Variable.t ->
  'b

val type_for_const : RWC.t -> 'a

val kind_for_const : RWC.t -> 'a

val is_alias_of_name : 'a -> Name.t -> bool

val check_equation : Name.t -> 'a -> unit
