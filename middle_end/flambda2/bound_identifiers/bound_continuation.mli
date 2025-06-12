(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A [Continuation] equipped with operations that mean it can be used in
    binding position within a [Name_abstraction] value. *)

type t = Continuation.t

include Bindable.S with type t := t
