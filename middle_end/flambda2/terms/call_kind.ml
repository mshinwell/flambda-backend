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

let check_arity arity =
  if Flambda_arity.With_subkinds.is_nullary arity
  then Misc.fatal_error "Invalid nullary arity"

let fprintf = Format.fprintf

module Function_call = struct
  type t =
    | Direct of
        { code_id : Code_id.t;
          function_slot : Function_slot.t;
          return_arity : Flambda_arity.With_subkinds.t
        }
    | Indirect_unknown_arity
    | Indirect_known_arity of
        { param_arity : Flambda_arity.With_subkinds.t;
          return_arity : Flambda_arity.With_subkinds.t
        }

  let [@ocamlformat "disable"] print ppf call =
    match call with
    | Direct { code_id; function_slot; return_arity; } ->
      fprintf ppf "@[<hov 1>(Direct@ \
          @[<hov 1>(code_id@ %a)@]@ \
          @[<hov 1>(function_slot@ %a)@]@ \
          @[<hov 1>(return_arity@ %a)@]\
          )@]"
        Code_id.print code_id
        Function_slot.print function_slot
        Flambda_arity.With_subkinds.print return_arity
    | Indirect_unknown_arity ->
      fprintf ppf "Indirect_unknown_arity"
    | Indirect_known_arity { param_arity; return_arity; } ->
      fprintf ppf "@[<hov 1>(Indirect_known_arity %a \u{2192} %a)@]"
        Flambda_arity.With_subkinds.print param_arity
        Flambda_arity.With_subkinds.print return_arity

  let return_arity call =
    match call with
    | Direct { return_arity; _ } | Indirect_known_arity { return_arity; _ } ->
      return_arity
    | Indirect_unknown_arity ->
      Flambda_arity.With_subkinds.create [Flambda_kind.With_subkind.any_value]
end

type method_kind =
  | Self
  | Public
  | Cached

let print_method_kind ppf kind =
  match kind with
  | Self -> fprintf ppf "Self"
  | Public -> fprintf ppf "Public"
  | Cached -> fprintf ppf "Cached"

let method_kind_from_lambda (kind : Lambda.meth_kind) =
  match kind with Self -> Self | Public -> Public | Cached -> Cached

type t =
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.t
      }
  | Method of
      { kind : method_kind;
        obj : Simple.t;
        alloc_mode : Alloc_mode.t
      }
  | C_call of
      { alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
        is_c_builtin : bool
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Function_call.print function_call
      Alloc_mode.print alloc_mode
  | Method { kind; obj; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Simple.print obj
      print_method_kind kind
      Alloc_mode.print alloc_mode
  | C_call { alloc; param_arity; return_arity; is_c_builtin; } ->
    fprintf ppf "@[(C@ @[(alloc %b)@]@ @[(is_c_builtin %b)@]@ \
        @<0>%s@<1>\u{2237}@<0>%s %a @<1>\u{2192} %a)@]"
      alloc
      is_c_builtin
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      Flambda_arity.print param_arity
      Flambda_arity.print return_arity

let direct_function_call code_id function_slot ~return_arity alloc_mode =
  check_arity return_arity;
  Function
    { function_call = Direct { code_id; function_slot; return_arity };
      alloc_mode
    }

let indirect_function_call_unknown_arity alloc_mode =
  Function { function_call = Indirect_unknown_arity; alloc_mode }

let indirect_function_call_known_arity ~param_arity ~return_arity alloc_mode =
  check_arity return_arity;
  Function
    { function_call = Indirect_known_arity { param_arity; return_arity };
      alloc_mode
    }

let method_call kind ~obj alloc_mode = Method { kind; obj; alloc_mode }

let c_call ~alloc ~param_arity ~return_arity ~is_c_builtin =
  begin
    match Flambda_arity.to_list return_arity with
    | [] | [_] -> ()
    | _ :: _ :: _ ->
      Misc.fatal_errorf "Illegal return arity for C call: %a"
        Flambda_arity.print return_arity
  end;
  C_call { alloc; param_arity; return_arity; is_c_builtin }

let return_arity t =
  match t with
  | Function { function_call; _ } -> Function_call.return_arity function_call
  | Method _ ->
    Flambda_arity.With_subkinds.create [Flambda_kind.With_subkind.any_value]
  | C_call { return_arity; _ } ->
    Flambda_arity.With_subkinds.of_arity return_arity

let free_names t =
  match t with
  | Function
      { function_call = Direct { code_id; function_slot; return_arity = _ };
        alloc_mode = _
      } ->
    Name_occurrences.add_function_slot_in_projection
      (Name_occurrences.add_code_id Name_occurrences.empty code_id
         Name_mode.normal)
      function_slot Name_mode.normal
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function
      { function_call =
          Indirect_known_arity { param_arity = _; return_arity = _ };
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Name_occurrences.empty
  | Method { kind = _; obj; alloc_mode = _ } ->
    Simple.pattern_match obj
      ~name:(fun obj ~coercion:_ ->
        Name_occurrences.singleton_name obj Name_mode.normal)
      ~const:(fun _ -> Name_occurrences.empty)

let apply_renaming t perm =
  match t with
  | Function
      { function_call = Direct { code_id; function_slot; return_arity };
        alloc_mode
      } ->
    let code_id' = Renaming.apply_code_id perm code_id in
    if code_id == code_id'
    then t
    else
      Function
        { function_call =
            Direct { code_id = code_id'; function_slot; return_arity };
          alloc_mode
        }
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function
      { function_call =
          Indirect_known_arity { param_arity = _; return_arity = _ };
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    t
  | Method { kind; obj; alloc_mode } ->
    let obj' = Simple.apply_renaming obj perm in
    if obj == obj' then t else Method { kind; obj = obj'; alloc_mode }

let all_ids_for_export t =
  match t with
  | Function
      { function_call = Direct { code_id; function_slot = _; return_arity = _ };
        alloc_mode = _
      } ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function
      { function_call =
          Indirect_known_arity { param_arity = _; return_arity = _ };
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Ids_for_export.empty
  | Method { kind = _; obj; alloc_mode = _ } -> Ids_for_export.from_simple obj
