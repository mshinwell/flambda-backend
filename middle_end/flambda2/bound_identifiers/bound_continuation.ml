(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Continuation

let free_names t = Name_occurrences.singleton_continuation t

let apply_renaming t renaming = Renaming.apply_continuation renaming t

let ids_for_export t = Ids_for_export.singleton_continuation t

let renaming t ~guaranteed_fresh =
  Renaming.add_fresh_continuation Renaming.empty t ~guaranteed_fresh
