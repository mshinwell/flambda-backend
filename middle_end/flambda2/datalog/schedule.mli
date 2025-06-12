(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type stats

val create_stats : unit -> stats

val print_stats : Format.formatter -> stats -> unit

type rule

type deduction =
  [ `Atom of Datalog.atom
  | `And of deduction list ]

val deduce : deduction -> (Heterogenous_list.nil, rule) Datalog.program

type t

val saturate : rule list -> t

val fixpoint : t list -> t

val run : ?stats:stats -> t -> Table.Map.t -> Table.Map.t
