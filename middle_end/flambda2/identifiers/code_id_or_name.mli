(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include module type of struct
  include Int_ids.Code_id_or_name
end

val pattern_match' : t -> code_id:(Code_id.t -> 'a) -> name:(Name.t -> 'a) -> 'a
