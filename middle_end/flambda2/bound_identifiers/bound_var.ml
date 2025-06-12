(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  { var : Variable.t;
    name_mode : Name_mode.t
  }

let [@ocamlformat "disable"] print ppf { var; name_mode = _; } =
  Variable.print ppf var

let create var name_mode =
  (* Note that [name_mode] might be [In_types], e.g. when dealing with function
     return types and also using [Typing_env.add_definition]. *)
  { var; name_mode }

let var t = t.var

let name_mode t = t.name_mode

let with_var t var = { t with var }

let with_name_mode t name_mode = { t with name_mode }

let rename t = with_var t (Variable.rename t.var)

let is_renamed_version_of t t' =
  Name_mode.equal t.name_mode t'.name_mode
  && Variable.is_renamed_version_of t.var t'.var

let apply_renaming t renaming =
  with_var t (Renaming.apply_variable renaming t.var)

let free_names t = Name_occurrences.singleton_variable t.var t.name_mode

let ids_for_export { var; name_mode = _ } =
  Ids_for_export.add_variable Ids_for_export.empty var

let renaming { var; name_mode = _ } ~guaranteed_fresh =
  let { var = guaranteed_fresh; name_mode = _ } = guaranteed_fresh in
  Renaming.add_fresh_variable Renaming.empty var ~guaranteed_fresh
