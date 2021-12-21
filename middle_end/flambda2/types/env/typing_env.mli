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

type typing_env

type t = typing_env

module Meet_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> t

  val env : t -> typing_env

  (** Note that we are now in the process of meeting the given two [Simple]s. *)
  val now_meeting : t -> Simple.t -> Simple.t -> t

  (** Determine whether we are now in the process of meeting the given two
      [Simple]s. The arguments do not have to be provided in the same order as
      when [now_meeting] was called. *)
  val already_meeting : t -> Simple.t -> Simple.t -> bool
end

module Join_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> left_env:typing_env -> right_env:typing_env -> t

  val target_join_env : t -> typing_env

  val left_join_env : t -> typing_env

  val right_join_env : t -> typing_env

  val now_joining : t -> Simple.t -> Simple.t -> t

  val already_joining : t -> Simple.t -> Simple.t -> bool
end

type meet_type =
  Meet_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t * Typing_env_extension.t) Or_bottom.t

val invariant : t -> unit

val print : Format.formatter -> t -> unit

val create :
  resolver:(Compilation_unit.t -> t option) ->
  get_imported_names:(unit -> Name.Set.t) ->
  t

val closure_env : t -> t

val resolver : t -> Compilation_unit.t -> t option

val code_age_relation_resolver :
  t -> Compilation_unit.t -> Code_age_relation.t option

val name_domain : t -> Name.Set.t

val current_scope : t -> Scope.t

val increment_scope : t -> t

val add_definition : t -> Bound_name.t -> Flambda_kind.t -> t

(** The caller is to ensure that the supplied type is the most precise available
    for the given name. *)
val add_equation : t -> Name.t -> Type_grammar.t -> meet_type:meet_type -> t

val add_definitions_of_params : t -> params:Bound_parameter.t list -> t

val add_symbol_definition : t -> Symbol.t -> t

val add_symbol_definitions : t -> Symbol.Set.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val add_equations_on_params :
  t ->
  params:Bound_parameter.t list ->
  param_types:Type_grammar.t list ->
  meet_type:meet_type ->
  t

(** If the kind of the name is known, it should be specified, otherwise it can
    be omitted. Such omission will cause an error if the name satisfies
    [variable_is_from_missing_cmx_file]. *)
val find : t -> Name.t -> Flambda_kind.t option -> Type_grammar.t

val find_or_missing : t -> Name.t -> Type_grammar.t option

val find_params : t -> Bound_parameter.t list -> Type_grammar.t list

val variable_is_from_missing_cmx_file : t -> Name.t -> bool

val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

(* CR mshinwell: clarify that this does not meet *)
(* CR vlaviron: If the underlying level in the extension defines several
   variables, then there is no guarantee that the binding order in the result
   will match the binding order used to create the level. If they don't match,
   then adding equations in the wrong order can make equations disappear. *)
val add_env_extension : t -> Typing_env_extension.t -> meet_type:meet_type -> t

val add_env_extension_with_extra_variables :
  t -> Typing_env_extension.With_extra_variables.t -> meet_type:meet_type -> t

val add_env_extension_from_level :
  t -> Typing_env_level.t -> meet_type:meet_type -> t

val type_simple_in_term_exn :
  t -> ?min_name_mode:Name_mode.t -> Simple.t -> Type_grammar.t

(** [name_mode_of_existing_simple] can be provided to improve performance of
    this function. *)
val get_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Simple.t ->
  Simple.t

val get_alias_then_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Type_grammar.t ->
  Simple.t

val aliases_of_simple :
  t -> min_name_mode:Name_mode.t -> Simple.t -> Aliases.Alias_set.t

val aliases_of_simple_allowable_in_types : t -> Simple.t -> Aliases.Alias_set.t

val add_to_code_age_relation :
  t -> new_code_id:Code_id.t -> old_code_id:Code_id.t option -> t

val code_age_relation : t -> Code_age_relation.t

val with_code_age_relation : t -> Code_age_relation.t -> t

val cut : t -> unknown_if_defined_at_or_later_than:Scope.t -> Typing_env_level.t

val free_names_transitive : t -> Type_grammar.t -> Name_occurrences.t

module Pre_serializable : sig
  type t

  val create : typing_env -> used_closure_vars:Var_within_closure.Set.t -> t

  val find_or_missing : t -> Name.t -> Type_grammar.t option
end

module Serializable : sig
  type t

  val create : Pre_serializable.t -> reachable_names:Name_occurrences.t -> t

  val print : Format.formatter -> t -> unit

  val to_typing_env :
    t ->
    resolver:(Compilation_unit.t -> typing_env option) ->
    get_imported_names:(unit -> Name.Set.t) ->
    typing_env

  val all_ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  val merge : t -> t -> t
end
