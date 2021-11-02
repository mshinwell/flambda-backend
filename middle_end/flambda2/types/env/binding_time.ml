(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = struct
  include Int

  let [@ocamlformat "disable"] print = Numeric_types.Int.print

  let hash = Hashtbl.hash
end

include T

type binding_time = t

module Set = Patricia_tree.Make_set (T)
module Map = Patricia_tree.Make_map (T) (Set)
module Tbl = Container_types.Make_tbl (T) (Map)

let strictly_earlier (t : t) ~than = t < than

let consts_and_discriminants = 0

let symbols = 1

let imported_variables = 2

let earliest_var = 3

let succ (t : t) =
  if t < earliest_var
  then Misc.fatal_error "Cannot increment binding time for symbols"
  else t + 1

(* CR mshinwell: enforce an upper limit on values of type [t] *)

module With_name_mode = struct
  type t = int

  let[@inline always] create binding_time name_mode =
    let name_mode =
      match Name_mode.descr name_mode with
      | Normal -> 0
      | In_types -> 1
      | Phantom -> 2
    in
    (binding_time lsl 2) lor name_mode

  let binding_time t = t lsr 2

  let[@inline always] name_mode t =
    match t land 3 with
    | 0 -> Name_mode.normal
    | 1 -> Name_mode.in_types
    | 2 -> Name_mode.phantom
    | _ -> assert false

  let scoped_name_mode t ~min_binding_time =
    (* Strictly before [min_binding_time] means out of scope, at
       [min_binding_time] or later is in scope. *)
    if binding_time t < earliest_var || min_binding_time <= binding_time t
    then (* Constant, symbol, or variable in the allowed scope *)
      name_mode t
    else (* Variable out of the allowed scope *)
      Name_mode.in_types

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "(bound at time %d %a)" (binding_time t)
      Name_mode.print (name_mode t)

  let equal t1 t2 = t1 = t2
end

module Make_non_overlapping_interval_tree (Datum : sig
  type t

  val print : Format.formatter -> t -> unit
end) =
struct
  let check_invariants = true

  module Interval = struct
    type nonrec t =
      { min_inclusive : t;
        max_inclusive : t
      }

    let[@ocamlformat "disable"] print ppf { min_inclusive; max_inclusive } =
      Format.fprintf ppf "@<hov 1>@[(\
          @[<hov 1>(min_inclusive@ %a)@]@ \
          @[<hov 1>(max_inclusive@ %a)@]\
          )@]"
        print min_inclusive
        print max_inclusive

    let contains { min_inclusive; max_inclusive } point =
      compare point min_inclusive >= 0 && compare point max_inclusive <= 0

    let point_contained_in_or_after { min_inclusive; max_inclusive = _ } point =
      compare point min_inclusive >= 0

    let overlaps
        ({ min_inclusive = min_inclusive1; max_inclusive = max_inclusive1 } as
        t1)
        ({ min_inclusive = min_inclusive2; max_inclusive = max_inclusive2 } as
        t2) =
      contains t1 min_inclusive2 || contains t1 max_inclusive2
      || contains t2 min_inclusive1 || contains t2 max_inclusive1

    let compare { min_inclusive = min_inclusive1; max_inclusive = _ }
        { min_inclusive = min_inclusive2; max_inclusive = _ } =
      compare min_inclusive1 min_inclusive2

    let equal t1 t2 = compare t1 t2 = 0
  end

  module Set = Container_types.Make_set (Interval)
  module Map = Container_types.Make_map (Interval) (Set)

  type t = Datum.t Map.t

  let print ppf t = Map.print Datum.print ppf t

  let empty = Map.empty

  let add t ~min_inclusive ~max_inclusive datum =
    let interval = { Interval.min_inclusive; max_inclusive } in
    if check_invariants
    then
      Map.iter
        (fun existing _ ->
          if Interval.overlaps interval existing
          then
            Misc.fatal_errorf
              "Cannot add interval@ %a@ that overlaps with existing interval@ \
               %a:@ %a"
              Interval.print interval Interval.print existing print t)
        t;
    Map.add interval datum t

  let find_exn t point =
    let interval, datum =
      Map.find_first
        (fun interval -> Interval.point_contained_in_or_after interval point)
        t
    in
    if Interval.contains interval point then datum else raise Not_found
end
