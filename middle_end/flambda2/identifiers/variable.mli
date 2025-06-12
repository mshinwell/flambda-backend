(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include module type of struct
  include Int_ids.Variable
end

val create_with_same_name_as_ident : ?user_visible:unit -> Ident.t -> t

(** [rename] always returns a variable with a compilation unit set to that of
    the current unit, not the unit of the variable passed in. *)
val rename : ?append:string -> t -> t

val is_renamed_version_of : t -> t -> bool

val unique_name : t -> string

val raw_name : t -> string
