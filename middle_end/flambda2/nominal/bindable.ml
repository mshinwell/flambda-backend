(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** The signature of "name-like things" that may occur in binding position
    inside [Name_abstraction] constructs. *)

module type S = sig
  type t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  (** Freshen the given name. *)
  val rename : t -> t

  (** Equivalence relation on renamed variables.

      [is_renamed_version_of x y] is [true] if there exists a bindable [z]
      such that [x] and [y] are renamed versions of [z].

      Note: this function can return [true] in other cases (if there are some name collisions
      for instance), this is (at least currently) only used for a sanity check, so
      users should not rely too much on its expected semantics.
      *)
  val is_renamed_version_of : t -> t -> bool

  (** [renaming stale ~guaranteed_fresh:fresh] is to create a renaming that
      turns all occurrences of the name [stale] into [fresh] (in a
      capture-avoiding manner, but that is inherent in [Renaming]). *)
  val renaming : t -> guaranteed_fresh:t -> Renaming.t
end
