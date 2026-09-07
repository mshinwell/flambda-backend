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

type t =
  { function_decls : Function_declarations.t;
    value_slots : Simple.t Value_slot.Map.t;
    synthetic_value_slots : Simple.t Value_slot.Map.t
  }

let [@ocamlformat "disable"] print_with_extra_fields extra_fields ppf
      { function_decls;
        value_slots;
        synthetic_value_slots
      } =
  let print_synthetic_value_slots ppf synthetic_value_slots =
    if not (Value_slot.Map.is_empty synthetic_value_slots) then
      Format.fprintf ppf "@ @[<hov 1>(synthetic_value_slots@ %a)@]"
        (Value_slot.Map.print Simple.print) synthetic_value_slots
  in
  Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
      %t\
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(value_slots@ %a)@]\
      %a\
      )@]"
    Flambda_colours.prim_constructive
    Flambda_colours.pop
    extra_fields
    (Function_declarations.print) function_decls
    (Value_slot.Map.print Simple.print) value_slots
    print_synthetic_value_slots synthetic_value_slots

let print ppf t = print_with_extra_fields (fun _ppf -> ()) ppf t

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
      { function_decls = function_decls1;
        value_slots = value_slots1;
        synthetic_value_slots = synthetic_value_slots1
      }
      { function_decls = function_decls2;
        value_slots = value_slots2;
        synthetic_value_slots = synthetic_value_slots2
      } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0
    then c
    else
      let c = Value_slot.Map.compare Simple.compare value_slots1 value_slots2 in
      if c <> 0
      then c
      else
        Value_slot.Map.compare Simple.compare synthetic_value_slots1
          synthetic_value_slots2

  let equal t1 t2 = compare t1 t2 = 0
end)

let is_empty { function_decls; value_slots; synthetic_value_slots = _ } =
  Function_declarations.is_empty function_decls
  && Value_slot.Map.is_empty value_slots

let create ?(synthetic_value_slots = Value_slot.Map.empty) ~value_slots
    function_decls =
  if Flambda_features.check_invariants ()
  then (
    Value_slot.Map.iter
      (fun value_slot _ ->
        if not (Value_slot.is_synthetic value_slot)
        then
          Misc.fatal_errorf
            "Value slot %a used as a synthetic value slot was not created as \
             such"
            Value_slot.print value_slot)
      synthetic_value_slots;
    Value_slot.Map.iter
      (fun value_slot _ ->
        if Value_slot.is_synthetic value_slot
        then
          Misc.fatal_errorf
            "Specialised value slot %a cannot be used as an ordinary value slot"
            Value_slot.print value_slot)
      value_slots);
  { function_decls; value_slots; synthetic_value_slots }

let function_decls t = t.function_decls

let value_slots t = t.value_slots

let synthetic_value_slots t = t.synthetic_value_slots

let is_closed t = Value_slot.Map.is_empty t.value_slots

let [@ocamlformat "disable"] print ppf
      { function_decls;
        value_slots;
        synthetic_value_slots;
      } =
  let print_synthetic_value_slots ppf synthetic_value_slots =
    if not (Value_slot.Map.is_empty synthetic_value_slots) then
      Format.fprintf ppf "@ @[<hov 1>(synthetic_value_slots@ %a)@]"
        (Value_slot.Map.print Simple.print) synthetic_value_slots
  in
  if Value_slot.Map.is_empty value_slots then
    Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
        @[<hov 1>%a@]\
        %a\
        )@]"
      Flambda_colours.prim_constructive
      Flambda_colours.pop
      (Function_declarations.print) function_decls
      print_synthetic_value_slots synthetic_value_slots
  else
    Format.fprintf ppf "@[<hov 1>(%tset_of_closures%t@ \
        @[<hov 1>%a@]@ \
        @[<hov 1>(env@ %a)@]\
        %a\
        )@]"
      Flambda_colours.prim_constructive
      Flambda_colours.pop
      Function_declarations.print function_decls
      (Value_slot.Map.print Simple.print) value_slots
      print_synthetic_value_slots synthetic_value_slots

let free_names { function_decls; value_slots; synthetic_value_slots } =
  let free_names_of_value_slots =
    Value_slot.Map.fold
      (fun value_slot simple free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.add_value_slot_in_declaration
             (Simple.free_names simple) value_slot Name_mode.normal))
      value_slots Name_occurrences.empty
  in
  let free_names_of_synthetic_value_slots =
    Value_slot.Map.fold
      (fun value_slot simple free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.add_value_slot_in_declaration
             (Simple.free_names simple) value_slot Name_mode.normal))
      synthetic_value_slots Name_occurrences.empty
  in
  Name_occurrences.union
    (Name_occurrences.union
       (Function_declarations.free_names function_decls)
       free_names_of_value_slots)
    free_names_of_synthetic_value_slots

let apply_renaming ({ function_decls; value_slots; synthetic_value_slots } as t)
    renaming =
  let function_decls' =
    Function_declarations.apply_renaming function_decls renaming
  in
  let rename_slots slots =
    let changed = ref false in
    let slots' =
      Value_slot.Map.filter_map
        (fun var simple ->
          if Renaming.value_slot_is_used renaming var
          then (
            let simple' = Simple.apply_renaming simple renaming in
            if not (simple == simple') then changed := true;
            Some simple')
          else (
            changed := true;
            None))
        slots
    in
    if !changed then slots' else slots
  in
  let value_slots' = rename_slots value_slots in
  let synthetic_value_slots' = rename_slots synthetic_value_slots in
  if
    function_decls == function_decls'
    && value_slots == value_slots'
    && synthetic_value_slots == synthetic_value_slots'
  then t
  else
    { function_decls = function_decls';
      value_slots = value_slots';
      synthetic_value_slots = synthetic_value_slots'
    }

let ids_for_export { function_decls; value_slots; synthetic_value_slots } =
  let function_decls_ids =
    Function_declarations.ids_for_export function_decls
  in
  let ids =
    Value_slot.Map.fold
      (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
      value_slots function_decls_ids
  in
  Value_slot.Map.fold
    (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
    synthetic_value_slots ids

let filter_function_declarations t ~f =
  let function_decls = Function_declarations.filter t.function_decls ~f in
  { t with function_decls }
