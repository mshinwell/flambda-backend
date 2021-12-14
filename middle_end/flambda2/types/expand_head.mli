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

(** Functions involving the expansion of any [Alias] type at the outermost level
    of a type. *)

module Expanded_type : sig
  type t

  val create_value : Type_grammar.head_of_kind_value -> t

  val create_naked_immediate : Type_grammar.head_of_kind_naked_immediate -> t

  val create_naked_float : Type_grammar.head_of_kind_naked_float -> t

  val create_naked_int32 : Type_grammar.head_of_kind_naked_int32 -> t

  val create_naked_int64 : Type_grammar.head_of_kind_naked_int64 -> t

  val create_naked_nativeint : Type_grammar.head_of_kind_naked_nativeint -> t

  val create_rec_info : Type_grammar.head_of_kind_rec_info -> t

  val create_bottom : Flambda_kind.t -> t

  val create_unknown : Flambda_kind.t -> t

  val bottom_like : t -> t

  val unknown_like : t -> t

  val is_bottom : t -> bool

  val is_unknown : t -> bool

  val to_type : t -> Type_grammar.t

  type descr = private
    | Value of Type_grammar.head_of_kind_value
    | Naked_immediate of Type_grammar.head_of_kind_naked_immediate
    | Naked_float of Type_grammar.head_of_kind_naked_float
    | Naked_int32 of Type_grammar.head_of_kind_naked_int32
    | Naked_int64 of Type_grammar.head_of_kind_naked_int64
    | Naked_nativeint of Type_grammar.head_of_kind_naked_nativeint
    | Rec_info of Type_grammar.head_of_kind_rec_info

  val descr : t -> descr Or_unknown_or_bottom.t

  type descr_oub = private
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t

  val descr_oub : t -> descr_oub
end

val expand_head : Typing_env.t -> Type_grammar.t -> Expanded_type.t

(** The following can be used when the canonical simple must be checked prior to
    expanding the head, to avoid finding the canonical simple twice. *)
module Prep : sig
  type t

  val canonical_simple : t -> Simple.t option

  val really_expand_head : t -> Expanded_type.t
end

val prepare_to_expand_head :
  Typing_env.t -> Type_grammar.t -> min_name_mode:Name_mode.t -> Prep.t

val get_canonical_simples_and_expand_heads :
  left_env:Typing_env.t ->
  left_ty:Type_grammar.t ->
  right_env:Typing_env.t ->
  right_ty:Type_grammar.t ->
  Simple.t option * Expanded_type.t * Simple.t option * Expanded_type.t

val is_bottom : Typing_env.t -> Type_grammar.t -> bool

val is_unknown : Typing_env.t -> Type_grammar.t -> bool

val make_suitable_for_environment :
  Typing_env.t ->
  Type_grammar.t ->
  suitable_for:Typing_env.t ->
  bind_to:Name.t ->
  Typing_env_extension.With_extra_variables.t
