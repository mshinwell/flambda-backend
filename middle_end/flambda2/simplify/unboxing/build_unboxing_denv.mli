(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Transform an unboxing decision tree into the downwards environment required
    for simplification. *)

open! Simplify_import

val denv_of_decision :
  DE.t -> param_var:Variable.t -> Unboxing_types.decision -> DE.t
