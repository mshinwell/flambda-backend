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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type symbols = { bound_symbols : Bound_symbols.t }

type t =
  | Singleton of Bound_var.t
  | Set_of_closures of
      { name_mode : Name_mode.t;
        closure_vars : Bound_var.t list
      }
  | Symbols of Bound_symbols.t
  | Code of Code_id.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Singleton var -> Bound_var.print ppf var
  | Set_of_closures { name_mode = _; closure_vars; } ->
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Bound_var.print)
      closure_vars
  | Symbols { bound_symbols } ->
    Format.fprintf ppf "@[<hov 1>\
        @[(bound_symbols@ %a)@]\
        )@]"
      Bound_symbols.print bound_symbols

let free_names t =
  match t with
  | Singleton var ->
    let var = Bound_var.create_var var in
    Name_occurrences.singleton_variable var Name_mode.normal
  | Set_of_closures { name_mode = _; closure_vars } ->
    List.fold_left
      (fun free_names var ->
        let var = Bound_var.create_var var in
        Name_occurrences.add_variable free_names var Name_mode.normal)
      Name_occurrences.empty closure_vars
  | Symbols { bound_symbols } -> Bound_symbols.free_names bound_symbols

let rec map_sharing f l0 =
  match l0 with
  | a :: l ->
    let a' = f a in
    let l' = map_sharing f l in
    if a' == a && l' == l then l0 else a' :: l'
  | [] -> []

let apply_renaming t perm =
  match t with
  | Singleton var ->
    let var' = Bound_var.apply_renaming var perm in
    if var == var' then t else Singleton var'
  | Set_of_closures { name_mode; closure_vars } ->
    let closure_vars' =
      map_sharing (fun var -> Bound_var.apply_renaming var perm) closure_vars
    in
    if closure_vars == closure_vars'
    then t
    else Set_of_closures { name_mode; closure_vars = closure_vars' }
  | Symbols { bound_symbols } ->
    let bound_symbols' = Bound_symbols.apply_renaming bound_symbols perm in
    if bound_symbols == bound_symbols'
    then t
    else Symbols { bound_symbols = bound_symbols' }

let all_ids_for_export t =
  match t with
  | Singleton var ->
    Ids_for_export.add_variable Ids_for_export.empty (Bound_var.create_var var)
  | Set_of_closures { name_mode = _; closure_vars } ->
    List.fold_left
      (fun ids var ->
        Ids_for_export.add_variable ids (Bound_var.create_var var))
      Ids_for_export.empty closure_vars
  | Symbols { bound_symbols } -> Bound_symbols.all_ids_for_export bound_symbols

let rename t =
  match t with
  | Singleton var -> Singleton (Bound_var.rename var)
  | Set_of_closures { name_mode; closure_vars } ->
    let closure_vars =
      List.map (fun var -> Bound_var.rename var) closure_vars
    in
    Set_of_closures { name_mode; closure_vars }
  | Symbols _ -> t

let renaming t1 ~guaranteed_fresh:t2 =
  match t1, t2 with
  | Singleton var1, Singleton var2 ->
    Renaming.add_fresh_variable Renaming.empty
      (Bound_var.create_var var1)
      ~guaranteed_fresh:(Bound_var.create_var var2)
  | ( Set_of_closures { name_mode = _; closure_vars = closure_vars1 },
      Set_of_closures { name_mode = _; closure_vars = closure_vars2 } ) ->
    if List.compare_lengths closure_vars1 closure_vars2 = 0
    then
      List.fold_left2
        (fun renaming var1 var2 ->
          Renaming.add_fresh_variable renaming
            (Bound_var.create_var var1)
            ~guaranteed_fresh:(Bound_var.create_var var2))
        Renaming.empty closure_vars1 closure_vars2
    else
      Misc.fatal_errorf "Mismatching closure vars:@ %a@ and@ %a" print t1 print
        t2
  | Symbols _, Symbols _ -> Renaming.empty
  | (Singleton _ | Set_of_closures _ | Symbols _), _ ->
    Misc.fatal_errorf "Kind mismatch:@ %a@ and@ %a" print t1 print t2

let singleton var = Singleton var

let set_of_closures ~closure_vars =
  let name_modes =
    List.fold_left
      (fun name_modes var ->
        Name_mode.Set.add (Bound_var.name_mode var) name_modes)
      Name_mode.Set.empty closure_vars
  in
  match Name_mode.Set.elements name_modes with
  | [] -> Misc.fatal_error "No closure IDs provided"
  | [name_mode] ->
    (* CR mshinwell: Check there are no duplicates in [closure_vars] *)
    Set_of_closures { name_mode; closure_vars }
  | _ ->
    Misc.fatal_errorf "Inconsistent name occurrence kinds:@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Bound_var.print)
      closure_vars

let symbols bound_symbols = Symbols { bound_symbols }

let name_mode t =
  match t with
  | Singleton var -> Bound_var.name_mode var
  | Set_of_closures { name_mode; _ } -> name_mode
  | Symbols _ -> Name_mode.normal

let with_name_mode t name_mode =
  match t with
  | Singleton var -> Singleton (Bound_var.with_name_mode var name_mode)
  | Set_of_closures { name_mode = _; closure_vars } ->
    Set_of_closures { name_mode; closure_vars }
  | Symbols _ -> t

let must_be_singleton t =
  match t with
  | Singleton var -> var
  | Set_of_closures _ | Symbols _ ->
    Misc.fatal_errorf "Bound name is not a [Singleton]:@ %a" print t

let must_be_singleton_opt t =
  match t with
  | Singleton var -> Some var
  | Set_of_closures _ | Symbols _ -> None

let must_be_set_of_closures t =
  match t with
  | Set_of_closures { closure_vars; _ } -> closure_vars
  | Singleton _ | Symbols _ ->
    Misc.fatal_errorf "Bound name is not a [Set_of_closures]:@ %a" print t

let must_be_symbols t =
  match t with
  | Symbols symbols -> symbols
  | Singleton _ | Set_of_closures _ ->
    Misc.fatal_errorf "Bound name is not a [Symbols]:@ %a" print t

let may_be_symbols t =
  match t with
  | Symbols symbols -> Some symbols
  | Singleton _ | Set_of_closures _ -> None

let fold_all_bound_vars t ~init ~f =
  match t with
  | Singleton var -> f init var
  | Set_of_closures { closure_vars; _ } ->
    ListLabels.fold_left closure_vars ~init ~f
  | Symbols _ -> init

let fold_all_bound_names t ~init ~var ~symbol ~code_id =
  match t with
  | Singleton v -> var init v
  | Set_of_closures { closure_vars; _ } ->
    ListLabels.fold_left closure_vars ~init ~f:var
  | Symbols symbols ->
    Code_id.Set.fold
      (fun cid acc -> code_id acc cid)
      (Bound_symbols.code_being_defined symbols.bound_symbols)
      init
    |> Symbol.Set.fold
         (fun s acc -> symbol acc s)
         (Bound_symbols.being_defined symbols.bound_symbols)
