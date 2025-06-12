(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Ok of 'a
  | Invalid

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val map : 'a t -> f:('a -> 'b) -> 'b t
