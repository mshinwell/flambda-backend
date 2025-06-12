(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = Value_slot.Set.t

let empty = Value_slot.Set.empty

include Container_types.Make (struct
  include Value_slot.Set

  let hash = Hashtbl.hash
end)

let subset t1 t2 = Value_slot.Set.subset t1 t2
