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
  type t = private
    | Const of Reg_width_const.Descr.t
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
end

val expand_head : Typing_env.t -> Type_grammar.t -> Expanded_type.t

val make_suitable_for_environment :
  Typing_env.t ->
  Type_grammar.t ->
  suitable_for:Typing_env.t ->
  bind_to:Name.t ->
  Typing_env_extension.With_extra_variables.t
