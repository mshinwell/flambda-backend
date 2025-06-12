(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module type S = sig
  type t

  (** Gather all table identifiers to export them. *)
  val ids_for_export : t -> Ids_for_export.t
end
