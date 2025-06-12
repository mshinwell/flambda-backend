(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* CR mshinwell: This module is unpleasant. We should arrange things so that
   [Value_slot.Set.Map] exists. (This should be easier now that things brought
   in using "include" can be shadowed.) *)
type t = Value_slot.Set.t

val empty : t

val subset : t -> t -> bool

include Container_types.S with type t := t
