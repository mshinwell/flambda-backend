(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Decide on doc or non-doc comments in here. There are some
   modules which aren't exposed in the interface but probably require
   documentation. *)

(* CR mshinwell: Remove when warning 60 fixed *)
[@@@ocaml.warning "-60"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
include Type_grammar

type flambda_type = t

let meet env t1 t2 : _ Or_bottom.t =
  let meet_env = Typing_env.Meet_env.create env in
  meet meet_env t1 t2

let join ?bound_name central_env ~left_env ~left_ty ~right_env ~right_ty =
  let join_env = Typing_env.Join_env.create central_env ~left_env ~right_env in
  match join ?bound_name join_env left_ty right_ty with
  | Unknown -> unknown_like left_ty
  | Known ty -> ty

let arity_of_list ts = Flambda_arity.create (List.map kind ts)

type typing_env = Typing_env.t

type typing_env_extension = Typing_env.Typing_env_extension.t

module Typing_env_extension = Typing_env.Typing_env_extension

let cut_and_n_way_join = Typing_env_level_join.cut_and_n_way_join

type 'a type_accessor = Typing_env.t -> 'a

let unknown_types_from_arity arity = List.map (fun kind -> unknown kind) arity

let rec unknown_with_descr (descr : Flambda_kind.With_subkind.descr) =
  match descr with
  | Any_value -> any_value ()
  | Naked_number Naked_immediate -> any_naked_immediate ()
  | Naked_number Naked_float -> any_naked_float ()
  | Naked_number Naked_int32 -> any_naked_int32 ()
  | Naked_number Naked_int64 -> any_naked_int64 ()
  | Naked_number Naked_nativeint -> any_naked_nativeint ()
  | Boxed_float -> any_boxed_float ()
  | Boxed_int32 -> any_boxed_int32 ()
  | Boxed_int64 -> any_boxed_int64 ()
  | Boxed_nativeint -> any_boxed_nativeint ()
  | Tagged_immediate -> any_tagged_immediate ()
  | Rec_info -> any_rec_info ()
  | Block { tag; fields } ->
    assert (not (Tag.equal tag Tag.double_array_tag));
    immutable_block ~is_unique:false tag ~field_kind:Flambda_kind.value
      ~fields:(List.map unknown_with_descr fields)
  | Float_block { num_fields } ->
    immutable_block ~is_unique:false Tag.double_array_tag
      ~field_kind:Flambda_kind.naked_float
      ~fields:(List.init num_fields (fun _ -> any_naked_float ()))

let unknown_with_subkind kind =
  unknown_with_descr (Flambda_kind.With_subkind.descr kind)

let unknown_types_from_arity_with_subkinds arity =
  List.map (fun kind -> unknown_with_subkind kind) arity

let bottom_types_from_arity arity = List.map (fun kind -> bottom kind) arity
