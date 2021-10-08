(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Consider caching the free names of the whole abstraction on
   each abstraction. *)

module type Term = sig
  include Contains_names.S

  include Contains_ids.S with type t := t
end

type ('bindable, 'term) t

module Make (Bindable : Bindable.S) (Term : Term) : sig
  (** The type [t] is the equivalent of an atom abstraction construction
      "[--]--" in nominal sets. *)
  type nonrec t = (Bindable.t, Term.t) t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val create : Bindable.t -> Term.t -> t

  (** Concretion of an abstraction at a fresh name. *)
  val pattern_match : t -> f:(Bindable.t -> Term.t -> 'a) -> 'a

  (** Concretion of a pair of abstractions at the same fresh name. *)
  val pattern_match_pair :
    t -> t -> f:(Bindable.t -> Term.t -> Term.t -> 'a) -> 'a

  (** Same as [pattern_match]. *)
  val ( let<> ) : t -> (Bindable.t * Term.t -> 'a) -> 'a
end

module Make_let_and_renaming
    (Bindable : Bindable.S) (Term : sig
      type t

      val apply_renaming : t -> Renaming.t -> t
    end) : sig
  (* Like [Make] but only produces the let-operator and [apply_renaming]. *)

  type nonrec t = (Bindable.t, Term.t) t

  val apply_renaming : t -> Renaming.t -> t

  val ( let<> ) : t -> (Bindable.t * Term.t -> 'a) -> 'a
end
