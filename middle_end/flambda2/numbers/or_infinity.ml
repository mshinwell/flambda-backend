(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a t =
  | Finite of 'a
  | Infinity

let equal ~f t1 t2 =
  match t1, t2 with
  | Finite a1, Finite a2 -> f a1 a2
  | Infinity, Infinity -> true
  | (Finite _ | Infinity), _ -> false

let compare ~f t1 t2 =
  match t1, t2 with
  | Finite a1, Finite a2 -> f a1 a2
  | Infinity, Infinity -> 0
  | Finite _, Infinity -> -1
  | Infinity, Finite _ -> 1

let hash ~f = function
  | Finite a -> Hashtbl.hash (0, f a)
  | Infinity -> Hashtbl.hash 1

let [@ocamlformat "disable"] print ~f ppf = function
  | Finite a -> f ppf a
  | Infinity -> Format.pp_print_string ppf "\u{221e}"
