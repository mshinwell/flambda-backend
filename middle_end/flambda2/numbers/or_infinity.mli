(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a t =
  | Finite of 'a
  | Infinity

val compare : f:('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash : f:('a -> int) -> 'a t -> int

val print :
  f:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
