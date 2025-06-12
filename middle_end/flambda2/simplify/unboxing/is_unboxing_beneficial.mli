(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* Filter out non beneficial decisions *)
(* *********************************** *)

val filter_non_beneficial_decisions :
  Unboxing_types.decision -> Unboxing_types.decision
