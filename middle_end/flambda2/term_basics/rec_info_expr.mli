(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include module type of struct
  include Int_ids.Rec_info_expr
end

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val free_names_in_types : t -> Name_occurrences.t
