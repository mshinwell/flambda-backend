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

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val empty : unit -> t

val is_empty : t -> bool

val create :
  defined_vars:Flambda_kind.t Variable.Map.t ->
  binding_times:Variable.Set.t Binding_time.Map.t ->
  equations:Type_grammar.t Name.Map.t ->
  symbol_projections:Symbol_projection.t Variable.Map.t ->
  t

val defined_variables : t -> Variable.Set.t

val defined_variables_with_kinds : t -> Flambda_kind.t Variable.Map.t

val defined_names : t -> Name.Set.t

val fold_on_defined_vars :
  (Variable.t -> Flambda_kind.t -> 'a -> 'a) -> t -> 'a -> 'a

val equations : t -> Type_grammar.t Name.Map.t

val add_definition : t -> Variable.t -> Flambda_kind.t -> Binding_time.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val concat : t -> t -> t

val find_kind : t -> Variable.t -> Flambda_kind.t

val variables_by_binding_time : t -> Variable.Set.t Binding_time.Map.t

val variable_is_defined : t -> Variable.t -> bool

(* CR vlaviron: this is only needed because Typing_env_extension creates a
   Name_abstraction over it. These functions should not be called, as levels are
   not exported. *)
include Contains_ids.S with type t := t
