(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a t =
  | Ok of 'a
  | Bottom

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val both : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val map : 'a t -> f:('a -> 'b) -> 'b t

val value_map : 'a t -> bottom:'b -> f:('a -> 'b) -> 'b

val all : 'a t list -> 'a list t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

module Let_syntax : sig
  val ( let<* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let<+ ) : 'a t -> ('a -> 'b) -> 'b t
end
