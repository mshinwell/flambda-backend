(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** The names of continuations. *)

type t = private Table_by_int_id.Id.t

type exported

include Container_types.S with type t := t

module Lmap : Lmap.S with type key := t

module Sort : sig
  type t =
    | Normal_or_exn
    | Return
    | Define_root_symbol
    | Toplevel_return

  val equal : t -> t -> bool
end

val create : ?sort:Sort.t -> ?name:string -> unit -> t

val rename : t -> t

val is_renamed_version_of : t -> t -> bool

val name : t -> string

val sort : t -> Sort.t

val export : t -> exported

val import : exported -> t

val initialise : unit -> unit

val reset : unit -> unit
