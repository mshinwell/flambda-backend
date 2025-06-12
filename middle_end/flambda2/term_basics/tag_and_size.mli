(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = Tag.t * Targetint_31_63.t

include Container_types.S with type t := t

val tag : t -> Tag.t

val size : t -> Targetint_31_63.t
