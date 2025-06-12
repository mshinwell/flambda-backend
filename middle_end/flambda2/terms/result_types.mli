(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val create :
  params:Bound_parameters.t ->
  results:Bound_parameters.t ->
  Flambda2_types.Typing_env_extension.With_extra_variables.t ->
  t

val pattern_match :
  t ->
  f:
    (params:Bound_parameters.t ->
    results:Bound_parameters.t ->
    Flambda2_types.Typing_env_extension.With_extra_variables.t ->
    'a) ->
  'a

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
