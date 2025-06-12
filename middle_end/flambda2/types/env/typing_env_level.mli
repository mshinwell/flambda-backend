(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t

val print_equations : Format.formatter -> Type_grammar.t Name.Map.t -> unit

val print : Format.formatter -> t -> unit

val empty : t

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

val equations : t -> Type_grammar.t Name.Map.t

val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val add_definition : t -> Variable.t -> Flambda_kind.t -> Binding_time.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val concat : earlier:t -> later:t -> t

val ids_for_export : t -> Ids_for_export.t

val find_kind : t -> Variable.t -> Flambda_kind.t

val variables_by_binding_time : t -> Variable.Set.t Binding_time.Map.t

val variable_is_defined : t -> Variable.t -> bool

val fold_on_defined_vars :
  (Variable.t -> Flambda_kind.t -> 'a -> 'a) -> t -> 'a -> 'a

val as_extension_without_bindings : t -> Type_grammar.Env_extension.t
