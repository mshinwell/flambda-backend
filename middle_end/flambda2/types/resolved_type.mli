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

type t =
  | Const of Reg_width_const.Descr.t
  | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
  | Naked_immediate of
      Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
  | Naked_float of Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
  | Naked_int32 of Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
  | Naked_int64 of Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
  | Naked_nativeint of
      Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
  | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t
