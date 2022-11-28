(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart & Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let simplify_nullary_primitive dacc original_prim (prim : P.nullary_primitive)
    dbg ~result_var =
  match prim with
  | Invalid _result_kind -> Simplify_primitive_result.create_invalid dacc
  | Optimised_out result_kind ->
    (match Bound_var.name_mode result_var with
    | Phantom -> ()
    | Normal | In_types ->
      Misc.fatal_errorf
        "The [Optimised_out] primitive should only be used in bindings of \
         phantom variables");
    let named = Named.create_prim original_prim dbg in
    let ty = T.unknown result_kind in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Probe_is_enabled { name = _ } ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_naked_bool in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Begin_region { try_region_parent } ->
    let try_region_parent =
      Option.map
        (fun try_region_parent ->
          let min_name_mode = Bound_var.name_mode result_var in
          let try_region_parent_ty =
            S.simplify_simple dacc (Simple.var try_region_parent) ~min_name_mode
          in
          let try_region_parent = T.get_alias_exn try_region_parent_ty in
          match Simple.must_be_var try_region_parent with
          | Some (try_region_parent, coercion) ->
            if not (Coercion.is_id coercion)
            then
              Misc.fatal_errorf
                "Did not expect non-identity coercion on region variable %a:@ \
                 %a"
                Variable.print try_region_parent Coercion.print coercion
            else try_region_parent
          | None ->
            Misc.fatal_errorf "Did not expect non-variable for region: %a"
              Simple.print try_region_parent)
        try_region_parent
    in
    let named =
      Named.create_prim (Nullary (Begin_region { try_region_parent })) dbg
    in
    let ty = T.any_region in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
