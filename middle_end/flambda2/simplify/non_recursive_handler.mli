(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  { cont : Continuation.t;
    params : Bound_parameters.t;
    lifted_params : Lifted_cont_params.t;
    handler : Flambda.Expr.t;
    is_exn_handler : bool;
    is_cold : bool
  }

val create :
  cont:Continuation.t ->
  params:Bound_parameters.t ->
  lifted_params:Lifted_cont_params.t ->
  handler:Flambda.Expr.t ->
  is_exn_handler:bool ->
  is_cold:bool ->
  t

val print : Format.formatter -> t -> unit

val with_handler : Flambda.Expr.t -> t -> t

val rename_params : t -> t
