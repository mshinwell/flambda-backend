(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Int_ids.Symbol

let is_predefined_exception t =
  Compilation_unit.equal (compilation_unit t) Compilation_unit.predef_exn
