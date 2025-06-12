(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Value_unknown
  | Value_symbol of Symbol.t
  | Value_const of Reg_width_const.t
  | Closure_approximation of
      { code_id : Code_id.t;
        function_slot : Function_slot.t;
        code : 'code;
        symbol : Symbol.t option
      }
  | Block_approximation of
      Tag.Scannable.t
      * Flambda_kind.Scannable_block_shape.t
      * 'code t array
      * Alloc_mode.For_types.t

val print : Format.formatter -> 'a t -> unit

val is_unknown : 'a t -> bool

val free_names :
  code_free_names:('code -> Name_occurrences.t) -> 'code t -> Name_occurrences.t
