(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)
(** Annotation on function call that represents a probe. *)

type desc = private
  { name : string;
    enabled_at_init : bool
  }

type t = desc option

val print : Format.formatter -> t -> unit

val from_lambda : Lambda.probe -> t
