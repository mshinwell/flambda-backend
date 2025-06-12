(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = unit

include Container_types.Make (struct
  type nonrec t = t

  let compare () () = 0

  let equal () () = true

  let hash () = 0

  let [@ocamlformat "disable"] print ppf () = Format.pp_print_string ppf "()"
end)

let free_names _ = Name_occurrences.empty

let apply_renaming () _ = ()

let subset () () = true
