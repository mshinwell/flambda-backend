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

type t =
  { var : Variable.t;
    name_mode : Name_mode.t
  }

let [@ocamlformat "disable"] print ppf { var; name_mode; } =
  match name_mode with
  | Normal -> Variable.print ppf var
  | In_types -> assert false (* see [create], below *)
  | Phantom -> Variable.print ppf var

let create var name_mode =
  if not (Name_mode.can_be_in_terms name_mode)
  then
    Misc.fatal_errorf "Name mode %a (for variable %a) not allowed in terms"
      Name_mode.print name_mode Variable.print var;
  { var; name_mode }

let var t = t.var

let name_mode t = t.name_mode

let with_var t var = { t with var }

let with_name_mode t name_mode = { t with name_mode }

let rename t = with_var t (Variable.rename t.var)

let apply_renaming t perm = with_var t (Renaming.apply_variable perm t.var)

let free_names t = Name_occurrences.singleton_variable t.var t.name_mode

let all_ids_for_export { var; name_mode = _ } =
  Ids_for_export.add_variable Ids_for_export.empty var

let renaming { var; name_mode = _ } ~guaranteed_fresh =
  let { var = guaranteed_fresh; name_mode = _ } = guaranteed_fresh in
  Renaming.add_fresh_variable Renaming.empty var ~guaranteed_fresh
