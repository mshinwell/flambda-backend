(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val create : Bound_parameters.t -> t

val to_list : t -> Bound_parameters.t

val rename : t -> t

val renaming : t -> guaranteed_fresh:t -> Renaming.t

val is_empty : t -> bool

val arity : t -> Flambda_arity.t

val arity_with_subkinds : t -> Flambda_arity.With_subkinds.t

val check_no_duplicates : t -> unit

(** As for [vars] but returns a set. *)
val var_set : t -> Variable.Set.t

(* (** As for [Variable.List.vars]. *) val vars : t -> Variable.t list

   (** As for [vars] but returns a list of [Simple.t] values describing the
   variables. *) val simples : t -> Simple.t list

   (** As for [var_set] but returns a set of [Name]s. *) val name_set : t ->
   Name.Set.t

   val equal_vars : t -> Variable.t list -> bool

   val rename : t -> t

   val print : Format.formatter -> t -> unit

   val equal : t -> t -> bool *)
