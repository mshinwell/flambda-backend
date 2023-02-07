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

let check_arity arity =
  if Flambda_arity.With_subkinds.is_nullary arity
  then Misc.fatal_error "Invalid nullary arity"

let fprintf = Format.fprintf

module Function_call = struct
  type t =
    | Direct of Code_id.t
    | Indirect_full_application
    | Indirect

  let [@ocamlformat "disable"] print ppf call =
    match call with
    | Direct code_id ->
      fprintf ppf "@[<hov 1>(Direct@ \
          @[<hov 1>(code_id@ %a)@]@ \
          )@]"
        Code_id.print code_id
    | Indirect ->
      fprintf ppf "Indirect"
    | Indirect_full_application ->
      fprintf ppf "Indirect_full_application"
end

module Method_kind = struct
  type t =
    | Self
    | Public
    | Cached

  let print ppf t =
    match t with
    | Self -> fprintf ppf "Self"
    | Public -> fprintf ppf "Public"
    | Cached -> fprintf ppf "Cached"

  let from_lambda (kind : Lambda.meth_kind) =
    match kind with Self -> Self | Public -> Public | Cached -> Cached

  let to_lambda t : Lambda.meth_kind =
    match t with Self -> Self | Public -> Public | Cached -> Cached
end

type t =
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_types.t;
        return_arity : Flambda_arity.With_subkinds.t
      }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t;
        alloc_mode : Alloc_mode.For_types.t
      }
  | C_call of
      { alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
        is_c_builtin : bool
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; alloc_mode; return_arity } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]@ \
        @[<hov 1>(return_arity@ %a)@]\
        )@]"
      Function_call.print function_call
      Alloc_mode.For_types.print alloc_mode
      Flambda_arity.With_subkinds.print return_arity
  | Method { kind; obj; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Simple.print obj
      Method_kind.print kind
      Alloc_mode.For_types.print alloc_mode
  | C_call { alloc; param_arity; return_arity; is_c_builtin; } ->
    fprintf ppf "@[(C@ @[(alloc %b)@]@ @[(is_c_builtin %b)@]@ \
        %t@<1>\u{2237}%t %a @<1>\u{2192} %a)@]"
      alloc
      is_c_builtin
      Flambda_colours.elide
      Flambda_colours.pop
      Flambda_arity.print param_arity
      Flambda_arity.print return_arity

let direct_function_call code_id ~return_arity alloc_mode =
  check_arity return_arity;
  Function { function_call = Direct code_id; return_arity; alloc_mode }

let indirect_function_call_unknown_arity ~return_arity alloc_mode =
  Function { function_call = Indirect; return_arity; alloc_mode }

let indirect_function_call_known_arity ~return_arity alloc_mode =
  check_arity return_arity;
  Function
    { function_call = Indirect_full_application; return_arity; alloc_mode }

let method_call kind ~obj alloc_mode = Method { kind; obj; alloc_mode }

let c_call ~alloc ~param_arity ~return_arity ~is_c_builtin =
  (match Flambda_arity.to_list return_arity with
  | [] | [_] -> ()
  | _ :: _ :: _ ->
    Misc.fatal_errorf "Illegal return arity for C call: %a" Flambda_arity.print
      return_arity);
  C_call { alloc; param_arity; return_arity; is_c_builtin }

let return_arity t =
  match t with
  | Function { return_arity; _ } -> return_arity
  | Method _ ->
    Flambda_arity.With_subkinds.create [Flambda_kind.With_subkind.any_value]
  | C_call { return_arity; _ } ->
    Flambda_arity.With_subkinds.of_arity return_arity

let free_names t =
  match t with
  | Function
      { function_call = Direct code_id; return_arity = _; alloc_mode = _ } ->
    Name_occurrences.singleton_code_id code_id Name_mode.normal
  | Function { function_call = Indirect; return_arity = _; alloc_mode = _ }
  | Function
      { function_call = Indirect_full_application;
        return_arity = _;
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Name_occurrences.empty
  | Method { kind = _; obj; alloc_mode = _ } -> Simple.free_names obj

let apply_renaming t renaming =
  match t with
  | Function { function_call = Direct code_id; return_arity; alloc_mode } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id'
    then t
    else Function { function_call = Direct code_id'; return_arity; alloc_mode }
  | Function { function_call = Indirect; return_arity = _; alloc_mode = _ }
  | Function
      { function_call = Indirect_full_application;
        return_arity = _;
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    t
  | Method { kind; obj; alloc_mode } ->
    let obj' = Simple.apply_renaming obj renaming in
    if obj == obj' then t else Method { kind; obj = obj'; alloc_mode }

let ids_for_export t =
  match t with
  | Function
      { function_call = Direct code_id; return_arity = _; alloc_mode = _ } ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function { function_call = Indirect; return_arity = _; alloc_mode = _ }
  | Function
      { function_call = Indirect_full_application;
        return_arity = _;
        alloc_mode = _
      }
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Ids_for_export.empty
  | Method { kind = _; obj; alloc_mode = _ } -> Ids_for_export.from_simple obj
