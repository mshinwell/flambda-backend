(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Mutable
  | Immutable
  | Immutable_unique
      (** Mutable means that contents may vary at any moment.

          Immutable means that not only contents will never vary, but we're also
          allowed to share or duplicate identical values at will.

          Immutable_unique means that the contents will never vary, but physical
          equality is meaningful so the value must not be duplicated, nor
          shared. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val join : t -> t -> t

val to_asttypes : t -> Asttypes.mutable_flag

val from_lambda : Lambda.mutable_flag -> t

val is_mutable : t -> bool
