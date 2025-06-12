(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Things that the world does to a primitive application. *)
type t =
  | No_coeffects
      (** "No coeffects" means that the primitive does not observe the effects
          (in the sense described above) of other expressions. For example, it
          must not read from any mutable storage or call arbitrary external
          functions.

          It is assumed in Flambda that, subject to data dependencies,
          expressions with neither effects nor coeffects may be reordered with
          respect to other expressions. *)
  | Has_coeffects
      (** The primitive may be affected by effects from other expressions. *)

(** Print function. *)
val print : Format.formatter -> t -> unit

(** Comparison function. *)
val compare : t -> t -> int

(** Join two coeffects. *)
val join : t -> t -> t

val from_lambda : Primitive.coeffects -> t
