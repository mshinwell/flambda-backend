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

module MTC = More_type_creators
module OUB = Or_unknown_or_bottom
module TD = Type_descr
module TE = Typing_env
module TG = Type_grammar
module TEEV = Typing_env_extension.With_extra_variables

module Expanded_type : sig
  type t = private
    | Const of Reg_width_const.Descr.t
    | Value of TG.head_of_kind_value OUB.t
    | Naked_immediate of TG.head_of_kind_naked_immediate OUB.t
    | Naked_float of TG.head_of_kind_naked_float OUB.t
    | Naked_int32 of TG.head_of_kind_naked_int32 OUB.t
    | Naked_int64 of TG.head_of_kind_naked_int64 OUB.t
    | Naked_nativeint of TG.head_of_kind_naked_nativeint OUB.t
    | Rec_info of TG.head_of_kind_rec_info OUB.t

  val of_const : Reg_width_const.t -> t

  val of_non_alias_type : ?coercion:Coercion.t -> TG.t -> t

  val to_type : t -> TG.t
end = struct
  type t =
    | Const of Reg_width_const.Descr.t
    | Value of TG.head_of_kind_value OUB.t
    | Naked_immediate of TG.head_of_kind_naked_immediate OUB.t
    | Naked_float of TG.head_of_kind_naked_float OUB.t
    | Naked_int32 of TG.head_of_kind_naked_int32 OUB.t
    | Naked_int64 of TG.head_of_kind_naked_int64 OUB.t
    | Naked_nativeint of TG.head_of_kind_naked_nativeint OUB.t
    | Rec_info of TG.head_of_kind_rec_info OUB.t

  let of_const cst = Const (Reg_width_const.descr cst)

  let of_non_alias_type ?coercion ty : t =
    match TG.descr ty with
    | Value Unknown -> Value Unknown
    | Value Bottom -> Value Bottom
    | Value (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion -> TG.apply_coercion_head_of_kind_value head coercion
      in
      Value (Ok head)
    | Naked_immediate Unknown -> Naked_immediate Unknown
    | Naked_immediate Bottom -> Naked_immediate Bottom
    | Naked_immediate (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion ->
          TG.apply_coercion_head_of_kind_naked_immediate head coercion
      in

      Naked_immediate (Ok head)
    | Naked_float Unknown -> Naked_float Unknown
    | Naked_float Bottom -> Naked_float Bottom
    | Naked_float (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion ->
          TG.apply_coercion_head_of_kind_naked_float head coercion
      in
      Naked_float (Ok head)
    | Naked_int32 Unknown -> Naked_int32 Unknown
    | Naked_int32 Bottom -> Naked_int32 Bottom
    | Naked_int32 (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion ->
          TG.apply_coercion_head_of_kind_naked_int32 head coercion
      in
      Naked_int32 (Ok head)
    | Naked_int64 Unknown -> Naked_int64 Unknown
    | Naked_int64 Bottom -> Naked_int64 Bottom
    | Naked_int64 (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion ->
          TG.apply_coercion_head_of_kind_naked_int64 head coercion
      in
      Naked_int64 (Ok head)
    | Naked_nativeint Unknown -> Naked_nativeint Unknown
    | Naked_nativeint Bottom -> Naked_nativeint Bottom
    | Naked_nativeint (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion ->
          TG.apply_coercion_head_of_kind_naked_nativeint head coercion
      in

      Naked_nativeint (Ok head)
    | Rec_info Unknown -> Rec_info Unknown
    | Rec_info Bottom -> Rec_info Bottom
    | Rec_info (Ok (No_alias head)) ->
      let head =
        match coercion with
        | None -> head
        | Some coercion -> TG.apply_coercion_head_of_kind_rec_info head coercion
      in
      Rec_info (Ok head)
    | Value (Ok (Equals _))
    | Naked_immediate (Ok (Equals _))
    | Naked_float (Ok (Equals _))
    | Naked_int32 (Ok (Equals _))
    | Naked_int64 (Ok (Equals _))
    | Naked_nativeint (Ok (Equals _))
    | Rec_info (Ok (Equals _)) ->
      Misc.fatal_errorf "Type cannot be an alias type:@ %a" TG.print ty

  let to_type (t : t) =
    match t with
    | Const descr -> (
      match descr with
      | Naked_immediate i -> MTC.this_naked_immediate_without_alias i
      | Tagged_immediate i -> MTC.this_tagged_immediate_without_alias i
      | Naked_float f -> MTC.this_naked_float_without_alias f
      | Naked_int32 i -> MTC.this_naked_int32_without_alias i
      | Naked_int64 i -> MTC.this_naked_int64_without_alias i
      | Naked_nativeint i -> MTC.this_naked_nativeint_without_alias i)
    | Value Unknown -> TG.any_value
    | Value Bottom -> TG.bottom_value
    | Value (Ok head) -> TG.create_from_head_value head
    | Naked_immediate Unknown -> TG.any_naked_immediate
    | Naked_immediate Bottom -> TG.bottom_naked_immediate
    | Naked_immediate (Ok head) -> TG.create_from_head_naked_immediate head
    | Naked_float Unknown -> TG.any_naked_float
    | Naked_float Bottom -> TG.bottom_naked_float
    | Naked_float (Ok head) -> TG.create_from_head_naked_float head
    | Naked_int32 Unknown -> TG.any_naked_int32
    | Naked_int32 Bottom -> TG.bottom_naked_int32
    | Naked_int32 (Ok head) -> TG.create_from_head_naked_int32 head
    | Naked_int64 Unknown -> TG.any_naked_int64
    | Naked_int64 Bottom -> TG.bottom_naked_int64
    | Naked_int64 (Ok head) -> TG.create_from_head_naked_int64 head
    | Naked_nativeint Unknown -> TG.any_naked_nativeint
    | Naked_nativeint Bottom -> TG.bottom_naked_nativeint
    | Naked_nativeint (Ok head) -> TG.create_from_head_naked_nativeint head
    | Rec_info Unknown -> TG.any_rec_info
    | Rec_info Bottom -> TG.bottom_rec_info
    | Rec_info (Ok head) -> TG.create_from_head_rec_info head
end

let expand_head0 simple env kind : Expanded_type.t =
  let min_name_mode = Name_mode.min_in_types in
  match TE.get_canonical_simple_exn env simple ~min_name_mode with
  | exception Not_found ->
    (* This can happen when [simple] is of [Phantom] name mode. We're not
       interested in propagating types for phantom variables, so [Unknown] is
       fine here. *)
    Expanded_type.of_non_alias_type (MTC.unknown kind)
  | simple ->
    let[@inline always] name name ~coercion =
      let ty = TE.find env name (Some kind) in
      match TG.get_alias_exn ty with
      | exception Not_found ->
        let coercion =
          if Coercion.is_id coercion then None else Some coercion
        in
        Expanded_type.of_non_alias_type ?coercion ty
      | _alias ->
        Misc.fatal_errorf
          "Canonical alias %a should never have [Equals] type %a:@\n\n%a"
          Simple.print simple TG.print ty TE.print env
    in
    Simple.pattern_match simple ~const:Expanded_type.of_const ~name

let expand_head env ty =
  match TG.get_alias_exn ty with
  | exception Not_found -> Expanded_type.of_non_alias_type ty
  | simple -> expand_head0 simple env (TG.kind ty)

let missing_kind env free_names =
  Name_occurrences.fold_variables free_names ~init:false
    ~f:(fun missing_kind var ->
      missing_kind || TE.variable_is_from_missing_cmx_file env (Name.var var))

(* CR mshinwell: There is a subtlety here: the presence of a name in
   [suitable_for] doesn't mean that we should blindly return "=name". The type
   of the name in [suitable_for] might be (much) worse than the one in the
   environment [t]. *)
let rec make_suitable_for_environment0_core env t ~depth ~suitable_for level =
  let free_names = TG.free_names t in
  if Name_occurrences.no_variables free_names
  then level, t
  else if missing_kind env free_names
  then level, MTC.unknown (TG.kind t)
  else
    let to_erase =
      let var free_var = not (TE.mem suitable_for (Name.var free_var)) in
      Name_occurrences.filter_names free_names ~f:(fun free_name ->
          Name.pattern_match free_name ~var ~symbol:(fun _ -> true))
    in
    if Name_occurrences.is_empty to_erase
    then level, t
    else if depth > 1
    then level, MTC.unknown (TG.kind t)
    else
      let level, renaming =
        (* To avoid writing an erasure operation, we define irrelevant fresh
           variables in the returned [TEL], and swap them with the variables
           that we wish to erase throughout the type. *)
        Name_occurrences.fold_names to_erase ~init:(level, Renaming.empty)
          ~f:(fun ((level, renaming) as acc) to_erase_name ->
            Name.pattern_match to_erase_name
              ~symbol:(fun _ -> acc)
              ~var:(fun to_erase ->
                let original_type = TE.find env to_erase_name None in
                let kind = TG.kind original_type in
                let fresh_var = Variable.rename to_erase in
                let level =
                  let level, ty =
                    match
                      TE.get_canonical_simple_exn env
                        ~min_name_mode:Name_mode.in_types (Simple.var to_erase)
                    with
                    | exception Not_found -> level, MTC.unknown kind
                    | canonical_simple ->
                      if TE.mem_simple suitable_for canonical_simple
                      then level, TG.alias_type_of kind canonical_simple
                      else
                        let t = TE.find env (Name.var to_erase) (Some kind) in
                        let t = expand_head env t |> Expanded_type.to_type in
                        make_suitable_for_environment0_core env t
                          ~depth:(depth + 1) ~suitable_for level
                  in
                  TEEV.add_definition level fresh_var kind ty
                in
                let renaming =
                  Renaming.add_variable renaming to_erase fresh_var
                in
                level, renaming))
      in
      level, TG.apply_renaming t renaming

let make_suitable_for_environment0 env t ~suitable_for level =
  make_suitable_for_environment0_core env t ~depth:0 ~suitable_for level

let make_suitable_for_environment env t ~suitable_for ~bind_to =
  if not (TE.mem suitable_for bind_to)
  then
    Misc.fatal_errorf
      "[bind_to] %a is expected to be\n\
      \   bound in the [suitable_for] environment:@ %a" Name.print bind_to
      TE.print suitable_for;
  let level, t =
    make_suitable_for_environment0 env t ~suitable_for TEEV.empty
  in
  let level = TEEV.add_or_replace_equation level bind_to t in
  level
