(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A label, unique across the whole program, that identifies a function within
    a set of closures. In essence these function slots describe the semantics of
    a particular piece of code when it is executed in the context of a
    particular closure.

    Function slots are assigned integer offsets inside [Closure_tag] blocks,
    where the relevant information (code pointers, arity, etc.) will be stored
    at runtime, by the [Slot_offsets] module. *)

include Slot.S
