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

type t =
  { function_decls : Function_declarations.t;
    value_slots : Simple.t Value_slot.Map.t;
    alloc_mode : Alloc_mode.t;
    set_of_closures_symbol : Symbol.t option
  }

let [@ocamlformat "disable"] print ppf
      { function_decls;
        value_slots;
        alloc_mode;
        set_of_closures_symbol
      } =
  Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(value_slots@ %a)@]@ \
      @[<hov 1>(alloc_mode@ %a)@]@ \
      @[<hov 1>(set_of_closures_symbol@ %a)@]\
      )@]"
    (Flambda_colours.prim_constructive ())
    (Flambda_colours.normal ())
    (Function_declarations.print) function_decls
    (Value_slot.Map.print Simple.print) value_slots
    Alloc_mode.print alloc_mode
    (Misc.Stdlib.Option.print Symbol.print) set_of_closures_symbol

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
      { function_decls = function_decls1;
        value_slots = value_slots1;
        alloc_mode = alloc_mode1;
        set_of_closures_symbol = set_of_closures_symbol1
      }
      { function_decls = function_decls2;
        value_slots = value_slots2;
        alloc_mode = alloc_mode2;
        set_of_closures_symbol = set_of_closures_symbol2
      } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0
    then c
    else
      let c = Value_slot.Map.compare Simple.compare value_slots1 value_slots2 in
      if c <> 0
      then c
      else
        let c = Alloc_mode.compare alloc_mode1 alloc_mode2 in
        if c <> 0
        then c
        else
          Option.compare Symbol.compare set_of_closures_symbol1
            set_of_closures_symbol2

  let equal t1 t2 = compare t1 t2 = 0
end)

let is_empty
    { function_decls; value_slots; alloc_mode = _; set_of_closures_symbol = _ }
    =
  Function_declarations.is_empty function_decls
  && Value_slot.Map.is_empty value_slots

let create ~value_slots alloc_mode function_decls ~set_of_closures_symbol =
  (* CR mshinwell: Make sure invariant checks are applied here, e.g. that the
     set of closures is indeed closed. *)
  { function_decls; value_slots; alloc_mode; set_of_closures_symbol }

let function_decls t = t.function_decls

let value_slots t = t.value_slots

let alloc_mode t = t.alloc_mode

let is_closed t = Value_slot.Map.is_empty t.value_slots

let [@ocamlformat "disable"] print ppf
      { function_decls;
        value_slots;
        alloc_mode;
        set_of_closures_symbol = _
      } =
  if Value_slot.Map.is_empty value_slots then
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ %a@ \
        @[<hov 1>%a@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      Alloc_mode.print alloc_mode
      (Function_declarations.print) function_decls
  else
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ %a@ \
        @[<hov 1>%a@]@ \
        @[<hov 1>(env@ %a)@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      Alloc_mode.print alloc_mode
      Function_declarations.print function_decls
      (Value_slot.Map.print Simple.print) value_slots

let free_names
    { function_decls; value_slots; alloc_mode = _; set_of_closures_symbol } =
  let free_names_of_value_slots =
    Value_slot.Map.fold
      (fun value_slot simple free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.add_value_slot_in_declaration
             (Simple.free_names simple) value_slot Name_mode.normal))
      value_slots Name_occurrences.empty
  in
  Name_occurrences.union_list
    [ Function_declarations.free_names function_decls;
      free_names_of_value_slots;
      (match set_of_closures_symbol with
      | None -> Name_occurrences.empty
      | Some set_of_closures_symbol ->
        Name_occurrences.singleton_symbol set_of_closures_symbol
          Name_mode.normal) ]

let apply_renaming
    ({ function_decls; value_slots; alloc_mode; set_of_closures_symbol } as t)
    renaming =
  let function_decls' =
    Function_declarations.apply_renaming function_decls renaming
  in
  let value_slots' =
    Value_slot.Map.filter_map
      (fun var simple ->
        if Renaming.value_slot_is_used renaming var
        then Some (Simple.apply_renaming simple renaming)
        else None)
      value_slots
  in
  let set_of_closures_symbol' =
    match set_of_closures_symbol with
    | None -> None
    | Some symbol ->
      let symbol' = Renaming.apply_symbol renaming symbol in
      if symbol == symbol' then set_of_closures_symbol else Some symbol'
  in
  if function_decls == function_decls'
     && value_slots == value_slots'
     && set_of_closures_symbol == set_of_closures_symbol'
  then t
  else
    { function_decls = function_decls';
      value_slots = value_slots';
      alloc_mode;
      set_of_closures_symbol = set_of_closures_symbol'
    }

let all_ids_for_export
    { function_decls; value_slots; alloc_mode = _; set_of_closures_symbol } =
  let function_decls_ids =
    Function_declarations.all_ids_for_export function_decls
  in
  let all_ids =
    Value_slot.Map.fold
      (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
      value_slots function_decls_ids
  in
  match set_of_closures_symbol with
  | None -> all_ids
  | Some set_of_closures_symbol ->
    Ids_for_export.add_symbol all_ids set_of_closures_symbol

let filter_function_declarations t ~f =
  let function_decls = Function_declarations.filter t.function_decls ~f in
  { t with function_decls }

let set_of_closures_symbol t = t.set_of_closures_symbol
