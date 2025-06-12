(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A label, unique across the whole program, that identifies the value of a
    captured variable in a set of closures. Value slots are assigned integer
    offsets inside [Closure_tag] blocks, where the relevant captured value will
    be stored at runtime, by the [Slot_offsets] module. *)

include Slot.S
