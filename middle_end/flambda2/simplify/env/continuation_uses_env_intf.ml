(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

module type S = sig
  type t

  (** We don't have an interface that insists on adding continuations before
      seeing their uses. This would be problematic when inserting wrappers,
      where we have already advanced past the point at which such wrappers would
      need to be defined, before knowing that a wrapper is needed. *)

  val record_continuation_use :
    t ->
    Continuation.t ->
    Continuation_use_kind.t ->
    env_at_use:Downwards_env.t ->
    arg_types:Flambda2_types.t list ->
    t * Apply_cont_rewrite_id.t

  val delete_continuation_uses : t -> Continuation.t -> t

  val get_typing_env_no_more_than_one_use :
    t -> Continuation.t -> Flambda2_types.Typing_env.t option

  val num_continuation_uses : t -> Continuation.t -> int

  val all_continuations_used : t -> Continuation.Set.t
end
