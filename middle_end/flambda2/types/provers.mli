(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
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
module TE = Typing_env
module TG = Type_grammar

type 'a proof = private
  | Proved of 'a
  | Unknown
  | Invalid

type 'a proof_allowing_kind_mismatch = private
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

type var_or_symbol_or_tagged_immediate = private
  | Var of Variable.t
  | Symbol of Symbol.t
  | Tagged_immediate of Targetint_31_63.t

val prove_equals_to_var_or_symbol_or_tagged_immediate :
  TE.t -> TG.t -> (var_or_symbol_or_tagged_immediate * Coercion.t) proof

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates : TE.t -> TG.t -> Targetint_31_63.Set.t proof

val prove_naked_immediates : TE.t -> TG.t -> Targetint_31_63.Set.t proof

val prove_equals_single_tagged_immediate :
  TE.t -> TG.t -> Targetint_31_63.t proof

val prove_naked_floats :
  TE.t -> TG.t -> Numeric_types.Float_by_bit_pattern.Set.t proof

val prove_naked_int32s : TE.t -> TG.t -> Numeric_types.Int32.Set.t proof

val prove_naked_int64s : TE.t -> TG.t -> Numeric_types.Int64.Set.t proof

val prove_naked_nativeints : TE.t -> TG.t -> Targetint_32_64.Set.t proof

type variant_like_proof = private
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.Imm.t Tag.Scannable.Map.t
  }

val prove_variant_like :
  TE.t -> TG.t -> variant_like_proof proof_allowing_kind_mismatch

(** If [ty] is known to represent a boxed number or a tagged integer,
    [prove_is_a_boxed_number env ty] is [Proved kind]. [kind] is the kind of the
    unboxed number.

    If [ty] is known to represent something of kind value that is not a number
    [prove_is_a_boxed_number env ty] is [Invalid].

    Otherwise it is [Unknown] or [Wrong_kind] when [ty] is not of kind value. *)
val prove_is_a_boxed_number :
  TE.t -> TG.t -> Flambda_kind.Boxable_number.t proof_allowing_kind_mismatch

val prove_is_a_tagged_immediate :
  TE.t -> TG.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_float : TE.t -> TG.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int32 : TE.t -> TG.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int64 : TE.t -> TG.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_nativeint :
  TE.t -> TG.t -> unit proof_allowing_kind_mismatch

val prove_boxed_floats : TE.t -> TG.t -> Float.Set.t proof

val prove_boxed_int32s : TE.t -> TG.t -> Numeric_types.Int32.Set.t proof

val prove_boxed_int64s : TE.t -> TG.t -> Numeric_types.Int64.Set.t proof

val prove_boxed_nativeints : TE.t -> TG.t -> Targetint_32_64.Set.t proof

val prove_tags_and_sizes : TE.t -> TG.t -> Targetint_31_63.Imm.t Tag.Map.t proof

val prove_tags_must_be_a_block : TE.t -> TG.t -> Tag.Set.t proof

val prove_unique_tag_and_size :
  TE.t -> TG.t -> (Tag.t * Targetint_31_63.Imm.t) proof_allowing_kind_mismatch

val prove_is_int : TE.t -> TG.t -> bool proof

(* CR mshinwell: Fix comment and/or function name *)

(** Prove that the given type, of kind [Value], is a closures type describing
    exactly one set of closures. The function declaration type corresponding to
    such closure is returned together with its closure ID, if it is known. *)
val prove_single_closures_entry :
  TE.t ->
  TG.t ->
  (Closure_id.t * TG.Closures_entry.t * TG.Function_type.t) proof

val prove_single_closures_entry' :
  TE.t ->
  TG.t ->
  (Closure_id.t * TG.Closures_entry.t * TG.Function_type.t)
  proof_allowing_kind_mismatch

val prove_strings : TE.t -> TG.t -> String_info.Set.t proof

(** Attempt to show that the provided type describes the tagged version of a
    unique naked immediate [Simple].

    This function will return [Unknown] if values of the provided type might
    sometimes, but not always, be a tagged immediate (for example if it is a
    variant type involving blocks). *)
val prove_is_always_tagging_of_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

(** Attempt to show that the provided type _can_ describe, but might not always
    describe, the tagged version of a unique naked immediate [Simple]. It is
    guaranteed that if a [Simple] is returned, the type does not describe any
    other tagged immediate. *)
val prove_could_be_tagging_of_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

val prove_boxed_float_containing_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

val prove_boxed_int32_containing_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

val prove_boxed_int64_containing_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

val prove_boxed_nativeint_containing_simple :
  TE.t -> min_name_mode:Name_mode.t -> TG.t -> Simple.t proof

val prove_block_field_simple :
  TE.t ->
  min_name_mode:Name_mode.t ->
  TG.t ->
  Targetint_31_63.t ->
  Simple.t proof

val prove_variant_field_simple :
  TE.t ->
  min_name_mode:Name_mode.t ->
  TG.t ->
  Tag.t ->
  Targetint_31_63.t ->
  Simple.t proof

val prove_project_var_simple :
  TE.t ->
  min_name_mode:Name_mode.t ->
  TG.t ->
  Var_within_closure.t ->
  Simple.t proof

val prove_rec_info : TE.t -> TG.t -> Rec_info_expr.t proof
