(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)
(** [@zero_alloc ...] annotations on function declaration (not call sites) *)
type t = Lambda.zero_alloc_attribute =
  | Default_zero_alloc
  | Check of
      { strict : bool;
        loc : Location.t;
        custom_error_msg : string option
      }
  | Assume of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val from_lambda : Lambda.zero_alloc_attribute -> t
