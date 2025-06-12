(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

include Numeric_types.Int

let initial = 0

let next t = t + 1

let prev t =
  if t <= initial
  then
    Misc.fatal_error
      "Cannot decrement continuation level past the initial level";
  t - 1

let ( <= ) (t1 : t) t2 = t1 <= t2

let ( < ) (t1 : t) t2 = t1 < t2

let ( > ) (t1 : t) t2 = t1 > t2

let ( >= ) (t1 : t) t2 = t1 >= t2

let to_int t = t

let max t1 t2 = max t1 t2

module Tree = Patricia_tree.Make (struct
  let print = print
end)

module Set = Tree.Set
module Map = Tree.Map
