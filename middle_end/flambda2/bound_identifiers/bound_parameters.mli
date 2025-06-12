(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val empty : t

val create : Bound_parameter.t list -> t

val cons : Bound_parameter.t -> t -> t

val append : t -> t -> t

val to_list : t -> Bound_parameter.t list

val is_empty : t -> bool

val same_number : t -> t -> bool

val equal : t -> t -> bool

val arity : t -> [> ] Flambda_arity.t

val check_no_duplicates : t -> unit

val cardinal : t -> int

val simples : t -> Int_ids.Simple.t list

val to_set : t -> Bound_parameter.Set.t

val vars : t -> Variable.t list

val var_set : t -> Variable.Set.t

val iter : (Bound_parameter.t -> unit) -> t -> unit

val filter : (Bound_parameter.t -> bool) -> t -> t

val exists : (Bound_parameter.t -> bool) -> t -> bool

include Bindable.S with type t := t
