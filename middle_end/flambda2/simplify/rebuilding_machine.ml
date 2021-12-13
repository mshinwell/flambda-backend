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

[@@@ocaml.warning "+a-30-40-41-42"]

let rec eval (r : Rebuilding_instructions.t) k =
  match r with
  | Let_body
      { let_metadata;
        body;
        uacc
      } ->
    eval body (fun body ->
        Simplify_let_expr.rebuild_let simplify_named_result removed_operations
          ~lifted_constants_from_defining_expr ~at_unit_toplevel ~closure_info
          ~body uacc k)
  | Whole_let ->
