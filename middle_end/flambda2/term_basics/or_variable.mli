(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Values of type ['a] must not contain names! *)

type 'a t =
  | Const of 'a
  | Var of Variable.t * Debuginfo.t
      (** The [Debuginfo.t] will be used to give correct debugging information
          at the point in the code where the corresponding statically-allocated
          block is patched. It would typically identify the place where the
          original allocation occurred in the source code. *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

val free_names : _ t -> Name_occurrences.t

val apply_renaming : 'a t -> Renaming.t -> 'a t
