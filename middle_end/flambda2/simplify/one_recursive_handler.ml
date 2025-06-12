(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

let create ~params ~handler ~is_cold = { params; handler; is_cold }

let print ppf { params; handler; is_cold } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hv 1>(params@ %a)@]@ @[<hv 1>(is_cold@ %b)@]@ @[<hv \
     1>(handler@ %a)@])@]"
    Bound_parameters.print params is_cold Flambda.Expr.print handler

let with_handler handler t = { t with handler }
