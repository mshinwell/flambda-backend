(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = Tag.t * Targetint_31_63.t

include Container_types.Make_pair (Tag) (Targetint_31_63)

let tag (tag, _) = tag

let size (_, size) = size
