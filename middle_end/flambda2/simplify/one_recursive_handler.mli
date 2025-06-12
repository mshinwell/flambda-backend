(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

val create :
  params:Bound_parameters.t -> handler:Flambda.Expr.t -> is_cold:bool -> t

val print : Format.formatter -> t -> unit

val with_handler : Flambda.Expr.t -> t -> t
