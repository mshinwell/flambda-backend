(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Numbering of the nesting depth of continuations. *)

type t

include Container_types.S with type t := t

val initial : t

val prev : t -> t

val next : t -> t

val ( <= ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( > ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val max : t -> t -> t

val to_int : t -> int
