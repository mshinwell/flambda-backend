(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type string_contents =
  | Contents of string
  | Unknown_or_mutable

type t

(* CR mshinwell: [size] shouldn't be needed when passing [Contents] *)
val create : contents:string_contents -> size:Targetint_31_63.t -> t

val contents : t -> string_contents

val size : t -> Targetint_31_63.t

include Container_types.S with type t := t
