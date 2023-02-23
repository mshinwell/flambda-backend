(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Simplification of external calls *)

type t =
  | Unchanged of { return_types : Flambda2_types.t list Or_unknown.t }
  | Specialised of Downwards_acc.t * Flambda.Expr.t * Removed_operations.t
  | Invalid

val simplify_extcall :
  Downwards_acc.t ->
  Flambda.Apply.t ->
  callee_ty:Flambda2_types.t ->
  arg_types:Flambda2_types.t list ->
  t
