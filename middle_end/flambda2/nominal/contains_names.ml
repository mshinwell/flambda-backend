(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module type S = sig
  type t

  (** Compute the free names of a term. Such computation covers all kinds of
      bindable names (variables, continuations, ...) *)
  val free_names : t -> Name_occurrences.t

  (** Apply a renaming throughout a term. *)
  val apply_renaming : t -> Renaming.t -> t
end
