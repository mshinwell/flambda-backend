(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Int_ids.Code_id_or_name

let pattern_match' t ~code_id ~name =
  pattern_match t ~code_id
    ~var:(fun var -> name (Name.var var))
    ~symbol:(fun symbol -> name (Name.symbol symbol))
