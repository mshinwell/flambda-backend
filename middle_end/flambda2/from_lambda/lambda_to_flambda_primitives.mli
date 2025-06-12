(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module Acc = Closure_conversion_aux.Acc
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val check_float_array_optimisation_enabled : string -> unit

val convert_and_bind :
  Acc.t ->
  big_endian:bool ->
  Exn_continuation.t option ->
  register_const0:(Acc.t -> Static_const.t -> string -> Acc.t * Symbol.t) ->
  Lambda.primitive ->
  args:Simple.t list list ->
  Debuginfo.t ->
  current_region:Variable.t option ->
  current_ghost_region:Variable.t option ->
  (Acc.t -> Flambda.Named.t list -> Expr_with_acc.t) ->
  Expr_with_acc.t
