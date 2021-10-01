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

(** The grammar of Flambda types plus the basic creation functions upon them
    that do not require an environment. *)

module TD = Type_descr
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64

module Block_size : sig
  type t

  val subset : t -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t
end

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
        blocks : row_like_for_blocks Or_unknown.t;
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

and 'index row_like_index =
  | Known of 'index
  | At_least of 'index

and ('index, 'maps_to) row_like_case =
  { maps_to : 'maps_to;
    index : 'index row_like_index;
    env_extension : env_extension
  }

and row_like_for_blocks =
  { known_tags : (Block_size.t, int_indexed_product) row_like_case Tag.Map.t;
    other_tags : (Block_size.t, int_indexed_product) row_like_case Or_bottom.t
  }

and row_like_for_closures =
  { known_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case
      Closure_id.Map.t;
    other_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case Or_bottom.t
  }

and closures_entry =
  { function_decls : function_type Or_unknown_or_bottom.t Closure_id.Map.t;
    closure_types : closure_id_indexed_product;
    closure_var_types : var_within_closure_indexed_product
  }

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

type flambda_type = t

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val kind : t -> Flambda_kind.t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val apply_coercion : t -> Coercion.t -> t Or_bottom.t

val get_alias_exn : t -> Simple.t

val is_obviously_bottom : t -> bool

val is_obviously_unknown : t -> bool

val bottom_value : t

val bottom_naked_immediate : t

val bottom_naked_float : t

val bottom_naked_int32 : t

val bottom_naked_int64 : t

val bottom_naked_nativeint : t

val bottom_rec_info : t

val any_value : t

val any_naked_immediate : t

val any_naked_float : t

val any_naked_int32 : t

val any_naked_int64 : t

val any_naked_nativeint : t

val any_rec_info : t

val this_tagged_immediate : Targetint_31_63.t -> t

val this_rec_info : Rec_info_expr.t -> t

val this_naked_immediate : Targetint_31_63.t -> t

val this_naked_float : Numeric_types.Float_by_bit_pattern.t -> t

val this_naked_int32 : Int32.t -> t

val this_naked_int64 : Int64.t -> t

val this_naked_nativeint : Targetint_32_64.t -> t

val these_naked_immediates : no_alias:bool -> Targetint_31_63.Set.t -> t

val these_naked_floats : no_alias:bool -> Float.Set.t -> t

val these_naked_int32s : no_alias:bool -> Int32.Set.t -> t

val these_naked_int64s : no_alias:bool -> Int64.Set.t -> t

val these_naked_nativeints : no_alias:bool -> Targetint_32_64.Set.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> t

val boxed_int32_alias_to : naked_int32:Variable.t -> t

val boxed_int64_alias_to : naked_int64:Variable.t -> t

val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> t

val box_float : t -> t

val box_int32 : t -> t

val box_int64 : t -> t

val box_nativeint : t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t

val tag_immediate : t -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t

val get_tag_for_block : block:Simple.t -> t

val create_variant :
  is_unique:bool ->
  immediates:t Or_unknown.t ->
  blocks:row_like_for_blocks Or_unknown.t ->
  t

val create_closures : row_like_for_closures -> t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

val array_of_length : length:t -> t

module Product : sig
  module Closure_id_indexed : sig
    type t = closure_id_indexed_product

    val top : t

    val create : flambda_type Closure_id.Map.t -> t

    val width : t -> Targetint_31_63.Imm.t

    (* CR mshinwell: check if this is used *)
    val components : t -> flambda_type list
  end

  module Var_within_closure_indexed : sig
    type t = var_within_closure_indexed_product

    val top : t

    val create : flambda_type Var_within_closure.Map.t -> t

    val width : t -> Targetint_31_63.Imm.t

    val components : t -> flambda_type list
  end

  module Int_indexed : sig
    type t = int_indexed_product

    val create_top : Flambda_kind.t -> t

    val create_from_list : Flambda_kind.t -> flambda_type list -> t

    val field_kind : t -> Flambda_kind.t

    val width : t -> Targetint_31_63.Imm.t

    val components : t -> flambda_type list
  end
end

module Function_type : sig
  type t = function_type
end

module Closures_entry : sig
  type t = closures_entry

  val find_function_type :
    t -> Closure_id.t -> Function_type.t Or_unknown_or_bottom.t
end

module Row_like_for_blocks : sig
  type t = row_like_for_blocks

  val bottom : t

  type open_or_closed =
    | Open of Tag.t Or_unknown.t
    | Closed of Tag.t

  val create :
    field_kind:Flambda_kind.t ->
    field_tys:flambda_type list ->
    open_or_closed ->
    t

  val create_blocks_with_these_tags :
    field_kind:Flambda_kind.t -> Tag.Set.t -> t

  val create_exactly_multiple :
    field_tys_by_tag:flambda_type list Tag.Map.t -> t

  val all_tags : t -> Tag.Set.t Or_unknown.t

  val all_tags_and_sizes : t -> Targetint_31_63.Imm.t Tag.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_size.t * Product.Int_indexed.t) option

  (** Get the nth field of the block if it is unambiguous.

      This can be done precisely using Type_grammar.meet_shape, but this meet
      can be expensive. This function allows to give a precise answer quickly in
      the common case where the block type is known exactly (for example, it is
      the result of a previous record or module allocation).

      This will return Unknown if:

      - There is no nth field (the read is invalid, and will produce bottom)

      - The block type represents a disjunction (several possible tags)

      - The tag or size is not exactly known

      - The nth field exists, is unique, but has Unknown type

      The handling of those cases could be improved:

      - When there is no valid field, Bottom could be returned instead

      - In the case of disjunctions, if all possible nth fields point to the
      same type, this type could be returned directly.

      - When the tag or size is not known but there is a unique possible value,
      it could be returned anyway

      - There could be a distinction between the first three cases (where we
      expect that doing the actual meet could give us a better result) and the
      last case where we already know what the result of the meet will be. *)
  val get_field : t -> Targetint_31_63.t -> flambda_type Or_unknown_or_bottom.t

  val get_variant_field :
    t -> Tag.t -> Targetint_31_63.t -> flambda_type Or_unknown_or_bottom.t

  val is_bottom : t -> bool

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
end

module Row_like_for_closures : sig
  type t = row_like_for_closures

  val create_exactly :
    Closure_id.t -> Set_of_closures_contents.t -> Closures_entry.t -> t

  val create_at_least :
    Closure_id.t -> Set_of_closures_contents.t -> Closures_entry.t -> t

  val get_singleton :
    t -> ((Closure_id.t * Set_of_closures_contents.t) * Closures_entry.t) option

  (** Same as For_blocks.get_field: attempt to find the type associated to the
      given environment variable without an expensive meet. *)
  val get_env_var : t -> Var_within_closure.t -> flambda_type Or_unknown.t
end
