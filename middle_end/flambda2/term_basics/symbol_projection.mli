(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module Projection : sig
  type t = private
    | Block_load of { index : Targetint_31_63.t }
    | Project_value_slot of
        { project_from : Function_slot.t;
          value_slot : Value_slot.t
        }

  val block_load : index:Targetint_31_63.t -> t

  val project_value_slot : Function_slot.t -> Value_slot.t -> t
end

type t

val print : Format.formatter -> t -> unit

val create : Symbol.t -> Projection.t -> Flambda_kind.With_subkind.t -> t

val kind : t -> Flambda_kind.With_subkind.t

val symbol : t -> Symbol.t

val projection : t -> Projection.t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
