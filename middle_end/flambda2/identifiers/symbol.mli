(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A symbol identifies a piece of statically-allocated data. The linkage name
    must be unique across the whole program. *)

include module type of struct
  include Int_ids.Symbol
end

val is_predefined_exception : t -> bool
