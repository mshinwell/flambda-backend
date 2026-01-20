(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type t =
  { dacc : Downwards_acc.t;
    bindings_to_place : Expr_builder.binding_to_place list;
    was_lifted_set_of_closures : bool
  }

let with_dacc ~dacc t = { t with dacc }

let create dacc binding_to_place =
  (* If [original_defining_expr] was simplified to a new term then the benefit
     of doing so is counted in [simplify_named]. *)
  { dacc;
    bindings_to_place = [binding_to_place];
    was_lifted_set_of_closures = false
  }

let create_empty dacc =
  { dacc; bindings_to_place = []; was_lifted_set_of_closures = false }

let create_have_lifted_set_of_closures dacc bound_vars_to_symbols
    ~original_defining_expr =
  (* The benefit of lifting the set of closures is added in [Simplify_named]. *)
  let machine_width = Downwards_env.machine_width (Downwards_acc.denv dacc) in
  { dacc;
    bindings_to_place =
      List.mapi
        (fun i (var, sym) ->
          Expr_builder.Keep_binding
            { let_bound = Bound_pattern.singleton var;
              simplified_defining_expr =
                Simplified_named.create ~machine_width
                  (Named.create_simple (Simple.symbol sym));
              original_defining_expr =
                (if i = 0 then Some original_defining_expr else None)
            })
        bound_vars_to_symbols;
    was_lifted_set_of_closures = true
  }

let dacc t = t.dacc

let no_bindings t =
  match t.bindings_to_place with [] -> true | _ :: _ -> false

let bindings_to_place t = t.bindings_to_place

let was_lifted_set_of_closures t = t.was_lifted_set_of_closures

let defines_symbols t =
  List.concat_map
    (fun (binding : Expr_builder.binding_to_place) ->
      match binding with
      | Delete_binding _ -> []
      | Keep_binding { let_bound; simplified_defining_expr; _ } -> (
        match let_bound with
        | Bound_pattern.Singleton bound_var -> (
          match simplified_defining_expr.named with
          | Simple simple -> (
            match Simple.must_be_symbol simple with
            | Some (sym, _coercion) -> [bound_var, sym]
            | None -> [])
          | Prim _ | Set_of_closures _ | Rec_info _ -> [])
        | Bound_pattern.Set_of_closures _bound_vars ->
          (* TODO: handle set of closures case where closures map to symbols *)
          []
        | Bound_pattern.Static _ ->
          (* Static bindings are already symbol bindings, not var-to-symbol *)
          []))
    t.bindings_to_place
