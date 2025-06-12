(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Code without any function bodies, but with all the associated metadata, e.g.
    free names. *)

type t = unit Code0.t

val code_metadata : t -> Code_metadata.t

include Code_metadata.Code_metadata_accessors_result_type with type 'a t := t

val create_with_metadata :
  free_names_of_params_and_body:Name_occurrences.t ->
  code_metadata:Code_metadata.t ->
  t

val create :
  free_names_of_params_and_body:Name_occurrences.t ->
  t Code_metadata.create_type

include Contains_names.S with type t := t

val print : Format.formatter -> t -> unit
