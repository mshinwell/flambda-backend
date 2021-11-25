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

module K = Flambda_kind
module TG = Type_grammar

type t =
  { defined_vars_by_binding_time : (Variable.t * K.t) list Binding_time.Map.t;
    equations : TG.t Name.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

(* CR mshinwell: print symbol projections along with tidying up this function *)
let print_equations ppf equations =
  let equations = Name.Map.bindings equations in
  match equations with
  | [] -> Format.pp_print_string ppf "()"
  | _ :: _ ->
    Format.pp_print_string ppf "(";
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (name, ty) ->
        Format.fprintf ppf "@[<hov 1>%a@ :@ %a@]" Name.print name TG.print ty)
      ppf equations;
    Format.pp_print_string ppf ")"

let [@ocamlformat "disable"] print ppf
      { defined_vars_by_binding_time ; equations;
        symbol_projections = _; } =
  (* CR mshinwell: Print [defined_vars] when not called from
     [Typing_env.print] *)
  if Binding_time.Map.is_empty defined_vars_by_binding_time then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<v 1>%a@])@])\
        @]"
      print_equations equations
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
        )@]"
      Variable.Set.print
        (Binding_time.Map.data defined_vars_by_binding_time
        |> List.concat
        |> List.map fst
        |> Variable.Set.of_list)
      print_equations equations

let empty =
  { defined_vars_by_binding_time = Binding_time.Map.empty;
    equations = Name.Map.empty;
    symbol_projections = Variable.Map.empty
  }

let is_empty { defined_vars_by_binding_time; equations; symbol_projections } =
  Binding_time.Map.is_empty defined_vars_by_binding_time
  && Name.Map.is_empty equations
  && Variable.Map.is_empty symbol_projections

let check_no_multiply_defined_vars ~defined_vars_by_binding_time =
  if Flambda_features.check_invariants ()
  then
    ignore
      (Binding_time.Map.fold
         (fun _binding_time vars_with_kinds variables ->
           List.fold_left
             (fun variables (var, _kind) ->
               if Variable.Set.mem var variables
               then
                 Misc.fatal_errorf
                   "Multiply-defined variables in [Typing_env_level]:@ %a"
                   Variable.print var
               else Variable.Set.add var variables)
             variables vars_with_kinds)
         defined_vars_by_binding_time Variable.Set.empty
        : Variable.Set.t)

let create ~defined_vars_from_these_levels ~restrict_defined_vars_to ~equations
    ~symbol_projections =
  let defined_vars_by_binding_time =
    List.fold_left
      (fun defined_vars_by_binding_time level ->
        Binding_time.Map.union
          (fun _binding_time existing_vars_with_kinds new_vars_with_kinds ->
            let new_vars_with_kinds =
              List.filter
                (fun (var, _kind) ->
                  Name_occurrences.mem_var restrict_defined_vars_to var)
                new_vars_with_kinds
            in
            Some (new_vars_with_kinds @ existing_vars_with_kinds))
          defined_vars_by_binding_time level.defined_vars_by_binding_time)
      Binding_time.Map.empty defined_vars_from_these_levels
  in
  check_no_multiply_defined_vars ~defined_vars_by_binding_time;
  { defined_vars_by_binding_time; equations; symbol_projections }

let add_symbol_projection t var proj =
  let symbol_projections = Variable.Map.add var proj t.symbol_projections in
  { t with symbol_projections }

let add_definition t var kind binding_time =
  if Flambda_features.check_invariants ()
     && Binding_time.Map.exists
          (fun _ vars_with_kinds ->
            Variable.Set.mem var
              (List.map fst vars_with_kinds |> Variable.Set.of_list))
          t.defined_vars_by_binding_time
  then
    Misc.fatal_errorf "[Typing_env_level] already binds variable %a:@ %a"
      Variable.print var print t;
  let defined_vars_by_binding_time =
    let vars =
      match
        Binding_time.Map.find binding_time t.defined_vars_by_binding_time
      with
      | exception Not_found -> [var, kind]
      | prev_vars -> (var, kind) :: prev_vars
    in
    Binding_time.Map.add binding_time vars t.defined_vars_by_binding_time
  in
  { t with defined_vars_by_binding_time }

let add_or_replace_equation t name ty =
  More_type_creators.check_equation name ty;
  if TG.is_obviously_unknown ty
  then { t with equations = Name.Map.remove name t.equations }
  else { t with equations = Name.Map.add name ty t.equations }

let equations t = t.equations

let symbol_projections t = t.symbol_projections

let fold_on_defined_vars f t init =
  Binding_time.Map.fold
    (fun bt vars acc ->
      List.fold_left (fun acc (var, kind) -> f var bt kind acc) acc vars)
    t.defined_vars_by_binding_time init

let concat (t1 : t) (t2 : t) =
  let defined_vars_by_binding_time =
    Binding_time.Map.union
      (fun _binding_time vars1 vars2 ->
        (* CR vlaviron: Technically this is feasible, as we can allow several
           variables with the same binding time, but it should only come from
           joins; concat arguments should always have disjoint binding time
           domains *)
        Misc.fatal_errorf
          "Cannot concatenate levels that have variables with overlapping \
           binding times (e.g. %a and %a):@ %a@ and@ %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun ppf (var, _kind) -> Variable.print ppf var))
          vars1
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun ppf (var, _kind) -> Variable.print ppf var))
          vars2 print t1 print t2)
      t1.defined_vars_by_binding_time t2.defined_vars_by_binding_time
  in
  check_no_multiply_defined_vars ~defined_vars_by_binding_time;
  let equations =
    Name.Map.union (fun _ _ty1 ty2 -> Some ty2) t1.equations t2.equations
  in
  let symbol_projections =
    Variable.Map.union
      (fun _var _proj1 proj2 -> Some proj2)
      t1.symbol_projections t2.symbol_projections
  in
  { defined_vars_by_binding_time; equations; symbol_projections }

let all_ids_for_export t =
  let variables =
    Binding_time.Map.fold
      (fun _binding_time vars_with_kinds variables ->
        List.fold_left
          (fun variables (var, _kind) -> Variable.Set.add var variables)
          variables vars_with_kinds)
      t.defined_vars_by_binding_time Variable.Set.empty
  in
  let ids = Ids_for_export.create ~variables () in
  let equation name ty ids =
    let ids = Ids_for_export.union ids (TG.all_ids_for_export ty) in
    Ids_for_export.add_name ids name
  in
  let ids = Name.Map.fold equation t.equations ids in
  let symbol_projection var proj ids =
    let ids =
      Ids_for_export.union ids (Symbol_projection.all_ids_for_export proj)
    in
    Ids_for_export.add_variable ids var
  in
  Variable.Map.fold symbol_projection t.symbol_projections ids
