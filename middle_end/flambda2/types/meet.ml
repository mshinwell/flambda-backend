(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

let meet env t1 t2 =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet env t1 t2
  else Meet_and_join.meet env t1 t2

let[@inline] meet_type () =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet_type
  else Meet_and_join.meet_type

let meet_shape env t ~shape =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet_shape env t ~shape
  else Meet_and_join.meet_shape env t ~shape
