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

type t =
  | Let_body_then_let of
      { simplify_named_result : Simplify_named_result.t;
        removed_operations : Removed_operations.t;
        lifted_constants_from_defining_expr : Lifted_constant_state.t;
        at_unit_toplevel : bool;
        closure_info : Closure_info.t
      }
