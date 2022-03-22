(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(* [Symbol]s and [Code_id]s are always bound at normal name mode. *)
type t =
  | Variable of Bound_var.t
  | Set_of_closures of Bound_var.t list
  | Symbols of Bound_symbols.t
  | Code of Code_id.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Variable bound_var -> Bound_var.print ppf bound_var
  | Set_of_closures bound_vars ->
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Bound_var.print)
      bound_vars
  | Symbols bound_symbols ->
    Format.fprintf ppf "@[<hov 1>\
        @[(bound_symbols@ %a)@]\
        )@]"
      Bound_symbols.print bound_symbols
  | Code code_id -> Code_id.print ppf code_id

let free_names t =
  match t with
  | Variable bound_var ->
    Name_occurrences.singleton_variable (Bound_var.var bound_var)
      Name_mode.normal
  | Set_of_closures bound_vars ->
    List.fold_left
      (fun free_names bound_var ->
        Name_occurrences.add_variable free_names (Bound_var.var bound_var)
          Name_mode.normal)
      Name_occurrences.empty bound_vars
  | Symbols bound_symbols -> Bound_symbols.free_names bound_symbols
  | Code code_id -> Name_occurrences.singleton_code_id code_id Name_mode.normal

let rec map_sharing f l0 =
  match l0 with
  | a :: l ->
    let a' = f a in
    let l' = map_sharing f l in
    if a' == a && l' == l then l0 else a' :: l'
  | [] -> []

let apply_renaming t renaming =
  match t with
  | Variable bound_var ->
    let bound_var' = Bound_var.apply_renaming bound_var renaming in
    if bound_var == bound_var' then t else Variable bound_var'
  | Set_of_closures bound_vars ->
    let bound_vars' =
      map_sharing
        (fun bound_var -> Bound_var.apply_renaming bound_var renaming)
        bound_vars
    in
    if bound_vars == bound_vars' then t else Set_of_closures bound_vars'
  | Symbols bound_symbols ->
    let bound_symbols' = Bound_symbols.apply_renaming bound_symbols renaming in
    if bound_symbols == bound_symbols' then t else Symbols bound_symbols'
  | Code code_id ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id' then t else Code code_id'

let all_ids_for_export t =
  match t with
  | Variable bound_var -> Bound_var.all_ids_for_export bound_var
  | Set_of_closures bound_vars ->
    List.fold_left
      (fun ids bound_var ->
        Ids_for_export.union ids (Bound_var.all_ids_for_export bound_var))
      Ids_for_export.empty bound_vars
  | Symbols bound_symbols -> Bound_symbols.all_ids_for_export bound_symbols
  | Code code_id -> Ids_for_export.singleton_code_id code_id

let rename t =
  match t with
  | Variable bound_var -> Variable (Bound_var.rename bound_var)
  | Set_of_closures bound_vars ->
    let bound_vars =
      List.map (fun bound_var -> Bound_var.rename bound_var) bound_vars
    in
    Set_of_closures bound_vars
  | Symbols _ | Code _ -> t

let renaming t1 ~guaranteed_fresh:t2 =
  match t1, t2 with
  | Variable bound_var1, Variable bound_var2 ->
    Renaming.add_fresh_variable Renaming.empty (Bound_var.var bound_var1)
      ~guaranteed_fresh:(Bound_var.var bound_var2)
  | Set_of_closures bound_vars1, Set_of_closures bound_vars2 ->
    if List.compare_lengths bound_vars1 bound_vars2 = 0
    then
      List.fold_left2
        (fun renaming var1 var2 ->
          Renaming.add_fresh_variable renaming (Bound_var.var var1)
            ~guaranteed_fresh:(Bound_var.var var2))
        Renaming.empty bound_vars1 bound_vars2
    else
      Misc.fatal_errorf
        "Mismatching bound vars for sets of closures:@ %a@ and@ %a" print t1
        print t2
  | Symbols _, Symbols _ | Code _, Code _ -> Renaming.empty
  | (Variable _ | Set_of_closures _ | Symbols _ | Code _), _ ->
    Misc.fatal_errorf "Pattern mismatch:@ %a@ and@ %a" print t1 print t2

let variable bound_var = Variable bound_var

let set_of_closures bound_vars =
  let name_mode =
    List.fold_left
      (fun name_mode bound_var ->
        let next_name_mode = Bound_var.name_mode bound_var in
        match name_mode with
        | None -> Some next_name_mode
        | Some name_mode ->
          if not (Name_mode.equal name_mode next_name_mode)
          then
            Misc.fatal_errorf "Mismatching name modes:@ %a"
              (Format.pp_print_list ~pp_sep:Format.pp_print_space
                 Bound_var.print)
              bound_vars
          else Some name_mode)
      None bound_vars
  in
  match name_mode with
  | None -> Misc.fatal_error "No bound variables provided for closures"
  | Some _name_mode -> Set_of_closures bound_vars

let symbols bound_symbols = Symbols bound_symbols

let name_mode t =
  match t with
  | Variable bound_var | Set_of_closures (bound_var :: _) ->
    Bound_var.name_mode bound_var
  | Set_of_closures [] -> assert false (* see [set_of_closures] above *)
  | Symbols _ | Code _ -> Name_mode.normal

let with_name_mode t name_mode =
  match t with
  | Variable bound_var ->
    Variable (Bound_var.with_name_mode bound_var name_mode)
  | Set_of_closures bound_vars ->
    Set_of_closures
      (List.map
         (fun bound_var -> Bound_var.with_name_mode bound_var name_mode)
         bound_vars)
  | Symbols _ | Code _ -> t

let must_be_variable t =
  match t with
  | Variable bound_var -> bound_var
  | Set_of_closures _ | Symbols _ | Code _ ->
    Misc.fatal_errorf "Bound pattern is not [Variable]:@ %a" print t

let must_be_variable_opt t =
  match t with
  | Variable bound_var -> Some bound_var
  | Set_of_closures _ | Symbols _ | Code _ -> None

let must_be_set_of_closures t =
  match t with
  | Set_of_closures bound_vars -> bound_vars
  | Variable _ | Symbols _ | Code _ ->
    Misc.fatal_errorf "Bound pattern is not [Set_of_closures]:@ %a" print t

let must_be_symbols t =
  match t with
  | Symbols symbols -> symbols
  | Variable _ | Set_of_closures _ | Code _ ->
    Misc.fatal_errorf "Bound pattern is not [Symbols]:@ %a" print t

let may_be_symbols t =
  match t with
  | Symbols symbols -> Some symbols
  | Variable _ | Set_of_closures _ | Code _ -> None

let fold_all_bound_vars t ~init ~f =
  match t with
  | Variable bound_var -> f init bound_var
  | Set_of_closures bound_vars -> ListLabels.fold_left bound_vars ~init ~f
  | Symbols _ | Code _ -> init

let fold_all_bound_names t ~init ~var ~symbol ~code_id:f_code_id =
  match t with
  | Variable bound_var -> var init bound_var
  | Set_of_closures bound_vars -> ListLabels.fold_left bound_vars ~init ~f:var
  | Symbols bound_symbols ->
    Symbol.Set.fold
      (fun s acc -> symbol acc s)
      (Bound_symbols.being_defined bound_symbols)
      init
  | Code code_id -> f_code_id init code_id
