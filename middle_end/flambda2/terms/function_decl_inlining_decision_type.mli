(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Not_yet_decided
  | Never_inline_attribute
  | Function_body_too_large of Code_size.t
  | Stub
  | Attribute_inline
  | Small_function of
      { size : Code_size.t;
        small_function_size : Code_size.t
      }
  | Speculatively_inlinable of
      { size : Code_size.t;
        small_function_size : Code_size.t;
        large_function_size : Code_size.t
      }
  | Functor of { size : Code_size.t }
  | Recursive

val print : Format.formatter -> t -> unit

val report : Format.formatter -> t -> unit

val must_be_inlined : t -> bool

val has_attribute_inline : t -> bool

val cannot_be_inlined : t -> bool

val equal : t -> t -> bool
