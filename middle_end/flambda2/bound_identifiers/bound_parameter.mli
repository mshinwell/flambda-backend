(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A parameter (to a function, continuation, etc.) together with its kind. *)
type t

(** Create a kinded parameter. *)
val create : Variable.t -> Flambda_kind.With_subkind.t -> t

(** The underlying variable. *)
val var : t -> Variable.t

val name : t -> Name.t

(** As for [var], but returns a [Simple.t] describing the variable. *)
val simple : t -> Int_ids.Simple.t

(** The kind of the given parameter. *)
val kind : t -> Flambda_kind.With_subkind.t

(** Replace the kind of the given parameter. *)
val with_kind : t -> Flambda_kind.With_subkind.t -> t

val rename : t -> t

val is_renamed_version_of : t -> t -> bool

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
