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
  type descr = private
    | Value of Type_grammar.head_of_kind_value
    | Naked_immediate of Type_grammar.head_of_kind_naked_immediate
    | Naked_float of Type_grammar.head_of_kind_naked_float
    | Naked_int32 of Type_grammar.head_of_kind_naked_int32
    | Naked_int64 of Type_grammar.head_of_kind_naked_int64
    | Naked_nativeint of Type_grammar.head_of_kind_naked_nativeint
    | Rec_info of Type_grammar.head_of_kind_rec_info

  type t = descr Or_unknown_or_bottom.t

  val to_type : t -> Type_grammar.t

  val create_value : Type_grammar.head_of_kind_value -> t

  val create_naked_immediate : Type_grammar.head_of_kind_naked_immediate -> t

  val create_naked_float : Type_grammar.head_of_kind_naked_float -> t

  val create_naked_int32 : Type_grammar.head_of_kind_naked_int32 -> t

  val create_naked_int64 : Type_grammar.head_of_kind_naked_int64 -> t

  val create_naked_nativeint : Type_grammar.head_of_kind_naked_nativeint -> t

  val create_rec_info : Type_grammar.head_of_kind_rec_info -> t

  val is_bottom : t -> bool
end

type expanded_type_or_const = private
  | Const of Reg_width_const.Descr.t
  | Expanded of Expanded_type.t

val expand_head : Typing_env.t -> Type_grammar.t -> Expanded_type.t

val make_suitable_for_environment :
  Typing_env.t ->
  Type_grammar.t ->
  suitable_for:Typing_env.t ->
  bind_to:Name.t ->
  Typing_env_extension.With_extra_variables.t

val is_bottom : Typing_env.t -> Type_grammar.t -> bool
