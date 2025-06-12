(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a t =
  | Unknown
  | Ok of 'a
  | Bottom

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_sharing : 'a t -> f:('a -> 'a) -> 'a t

module Let_syntax : sig
  val ( let<>* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let<>+ ) : 'a t -> ('a -> 'b) -> 'b t
end
