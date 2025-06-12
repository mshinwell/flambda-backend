(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module DE = Downwards_env
module T = Flambda2_types

type t

val create :
  Continuation_use_kind.t ->
  env_at_use:DE.t ->
  Apply_cont_rewrite_id.t ->
  arg_types:T.t list ->
  t

val print : Format.formatter -> t -> unit

val id : t -> Apply_cont_rewrite_id.t

val use_kind : t -> Continuation_use_kind.t

val arg_types : t -> T.t list

val env_at_use : t -> DE.t

val mark_non_inlinable : t -> t
