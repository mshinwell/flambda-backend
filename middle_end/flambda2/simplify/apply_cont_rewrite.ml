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

module EA = Continuation_extra_params_and_args.Extra_arg
module BP = Bound_parameter
module Id = Apply_cont_rewrite_id

type used =
  | Used
  | Unused

type t =
  { original_params : Bound_parameters.t;
    used_params : BP.Set.t;
    used_extra_params : Bound_parameters.t;
    extra_args : (EA.t * used) list Id.Map.t
  }

let print_used ppf = function
  | Used -> ()
  | Unused -> Format.fprintf ppf "@ unused"

let print_ea_used ppf t =
  Format.fprintf ppf "(%a)"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (ea, used) ->
         Format.fprintf ppf "%a%a" EA.print ea print_used used))
    t

let [@ocamlformat "disable"] print ppf
  { original_params; used_params; used_extra_params; extra_args; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(original_params@ (%a))@]@ \
      @[<hov 1>(used_params@ %a)@]@ \
      @[<hov 1>(used_extra_params@ (%a))@]@ \
      @[<hov 1>(extra_args@ %a)@]\
      )@]"
  Bound_parameters.print original_params
    BP.Set.print used_params
    Bound_parameters.print used_extra_params
    (Id.Map.print print_ea_used) extra_args

let does_nothing t =
  Bound_parameters.cardinal t.original_params = BP.Set.cardinal t.used_params
  && Bound_parameters.is_empty t.used_extra_params

let create ~original_params ~used_params ~extra_params ~extra_args
    ~used_extra_params =
  Bound_parameters.check_no_duplicates original_params;
  Bound_parameters.check_no_duplicates extra_params;
  if Bound_parameters.cardinal original_params < BP.Set.cardinal used_params
  then
    Misc.fatal_errorf
      "Must have at least as many [original_params] (%a)@ as [used_params] (%a)"
      Bound_parameters.print original_params BP.Set.print used_params;
  if Bound_parameters.cardinal extra_params < BP.Set.cardinal used_extra_params
  then
    Misc.fatal_errorf
      "Must have at least as many [extra_params] (%a)@ as [used_extra_params] \
       (%a)"
      Bound_parameters.print extra_params BP.Set.print used_extra_params;
  let extra_args =
    if Bound_parameters.is_empty extra_params
    then Id.Map.empty
    else
      let num_extra_params = Bound_parameters.cardinal extra_params in
      Id.Map.map
        (fun extra_args ->
          if num_extra_params <> List.length extra_args
          then
            Misc.fatal_errorf
              "Lengths of [extra_params] (%a)@ and all [extra_args] (e.g. %a) \
               should be equal"
              Bound_parameters.print extra_params
              Continuation_extra_params_and_args.Extra_arg.List.print extra_args;
          List.map2
            (fun extra_param extra_arg ->
              ( extra_arg,
                if BP.Set.mem extra_param used_extra_params
                then Used
                else Unused ))
            (Bound_parameters.to_list extra_params)
            extra_args)
        extra_args
  in
  let used_extra_params =
    Bound_parameters.filter
      (fun extra_param -> BP.Set.mem extra_param used_extra_params)
      extra_params
  in
  { original_params; used_params; used_extra_params; extra_args }

let original_params t = t.original_params

let used_params t = t.used_params

let used_extra_params t = t.used_extra_params

let extra_args t id =
  match Id.Map.find id t.extra_args with
  | exception Not_found ->
    if not (Bound_parameters.is_empty t.used_extra_params)
    then
      Misc.fatal_errorf
        "This [Apply_cont_rewrite] does not have any@ extra arguments for use \
         ID %a, but it has@ >= 1 extra parameter:@ %a"
        Id.print id print t;
    []
  | extra_args -> extra_args

let original_params_arity t =
  Bound_parameters.arity_with_subkinds (original_params t)
