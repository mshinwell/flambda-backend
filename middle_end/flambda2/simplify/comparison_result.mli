(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val create :
  prim:Flambda_primitive.t -> comparison_results:t Variable.Map.t -> t option

val print : Format.formatter -> t -> unit

val convert_result_compared_to_tagged_zero :
  t -> _ Flambda_primitive.comparison -> Flambda_primitive.t
