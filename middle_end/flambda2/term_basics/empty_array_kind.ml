(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Values_or_immediates_or_naked_floats
  | Unboxed_products
  | Naked_float32s
  | Naked_int32s
  | Naked_int64s
  | Naked_nativeints
  | Naked_vec128s

let print ppf t =
  match t with
  | Values_or_immediates_or_naked_floats -> Format.pp_print_string ppf "regular"
  | Unboxed_products -> Format.pp_print_string ppf "Unboxed_products"
  | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
  | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
  | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
  | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"
  | Naked_vec128s -> Format.pp_print_string ppf "Naked_vec128s"

let compare = Stdlib.compare

let of_element_kind t =
  (* This is used for reification, and we don't yet handle unboxed product
     arrays there. *)
  match (t : Flambda_kind.t) with
  | Value | Naked_number Naked_float -> Values_or_immediates_or_naked_floats
  | Naked_number Naked_immediate ->
    Misc.fatal_errorf
      "Arrays cannot yet contain elements of kind naked immediate"
  | Naked_number Naked_float32 -> Naked_float32s
  | Naked_number Naked_int32 -> Naked_int32s
  | Naked_number Naked_int64 -> Naked_int64s
  | Naked_number Naked_nativeint -> Naked_nativeints
  | Naked_number Naked_vec128 -> Naked_vec128s
  | Region | Rec_info ->
    Misc.fatal_errorf
      "Arrays cannot contain elements of kind region or rec_info"

let of_lambda array_kind =
  match (array_kind : Lambda.array_kind) with
  | Pgenarray | Paddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray Unboxed_float64 ->
    Values_or_immediates_or_naked_floats
  | Punboxedfloatarray Unboxed_float32 -> Naked_float32s
  | Punboxedintarray Unboxed_int32 -> Naked_int32s
  | Punboxedintarray Unboxed_int64 -> Naked_int64s
  | Punboxedintarray Unboxed_nativeint -> Naked_nativeints
  | Punboxedvectorarray Unboxed_vec128 -> Naked_vec128s
  | Pgcscannableproductarray _ | Pgcignorableproductarray _ -> Unboxed_products
