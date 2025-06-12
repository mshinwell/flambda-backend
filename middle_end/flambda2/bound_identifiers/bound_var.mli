(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Variables with name modes, as occur on the left-hand sides of
    [Let]-expressions (see [Bound_pattern]). *)

type t

val create : Variable.t -> Name_mode.t -> t

val var : t -> Variable.t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

include Bindable.S with type t := t
