(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Construct terms using only information from types. *)

val try_to_reify :
  Downwards_acc.t ->
  Debuginfo.t ->
  Simplified_named.t ->
  bound_to:Bound_var.t ->
  kind_of_bound_to:Flambda_kind.t ->
  allow_lifting:bool ->
  Simplified_named.t Or_invalid.t * Downwards_acc.t
