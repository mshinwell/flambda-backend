(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Values_or_immediates_or_naked_floats  (** Traditional OCaml arrays. *)
  | Unboxed_products
  | Naked_float32s
  | Naked_int32s
  | Naked_int64s
  | Naked_nativeints
  | Naked_vec128s
      (** Arrays of unboxed numbers, with a slightly different runtime
          representation. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val of_element_kind : Flambda_kind.t -> t

val of_lambda : Lambda.array_kind -> t
