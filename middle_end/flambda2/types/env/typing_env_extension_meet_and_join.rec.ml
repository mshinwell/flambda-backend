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

module TEE = Typing_env.Typing_env_extension

exception Bottom_meet

let rec meet0 env t1 t2 extra_extensions =
  (* A symmetrical meet would be hard to implement, as the inner meets can
     produce extra extensions that need to be merged with the result.

     To get around this, we'll suppose that [t2] is smaller than [t1] and add
     equations from [t2] to [t1], along with all extra equations *)
  let equations, extra_extensions =
    Name.Map.fold
      (fun name ty (eqs, extra_extensions) ->
        match Name.Map.find_opt name eqs with
        | None ->
          Type_grammar.check_equation name ty;
          Name.Map.add name ty eqs, extra_extensions
        | Some ty0 -> begin
          match Type_grammar.meet env ty0 ty with
          | Bottom -> raise Bottom_meet
          | Ok (ty, new_ext) ->
            Type_grammar.check_equation name ty;
            Name.Map.add (*replace*) name ty eqs, new_ext :: extra_extensions
        end)
      (TEE.to_map t2)
      (TEE.to_map t1, extra_extensions)
  in
  let ext = TEE.from_map equations in
  match extra_extensions with
  | [] -> ext
  | new_ext :: extra_extensions ->
    (* CR vlaviron: It's a bad idea to drop the extensions in the general case,
       but since we lack the property that the new extensions are stricter than
       the existing ones we can get into an infinite loop here (see
       flambdatest/unit_test/extension_meet.ml, function test_double_recursion
       for an example).

       This is very uncommon though (it needs recursive types involving at least
       three different names), so for now we still do the meet
       systematically. *)
    meet0 env ext new_ext extra_extensions

let meet env t1 t2 : _ Or_bottom.t =
  try Ok (meet0 env t1 t2 []) with Bottom_meet -> Bottom

let join env t1 t2 =
  let equations =
    Name.Map.merge
      (fun name ty1_opt ty2_opt ->
        match ty1_opt, ty2_opt with
        | None, _ | _, None -> None
        | Some ty1, Some ty2 -> begin
          match Type_grammar.join env ty1 ty2 with
          | Known ty ->
            if Type_grammar.is_alias_of_name ty name
            then
              (* This is rare but not anomalous. It may mean that [ty1] and
                 [ty2] are both alias types which canonicalize to [name], for
                 instance. In any event, if the best type available for [name]
                 is [= name], we effectively know nothing, so we drop [name].
                 ([name = name] would be rejected by [Typing_env.add_equation]
                 anyway.) *)
              None
            else begin
              (* This should always pass due to the [is_alias_of_name] check. *)
              Type_grammar.check_equation name ty;
              Some ty
            end
          | Unknown -> None
        end)
      (TEE.to_map t1) (TEE.to_map t2)
  in
  TEE.from_map equations
