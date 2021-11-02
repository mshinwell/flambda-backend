(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Make (Point : sig
  type t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end) (Datum : sig
  type t

  val print : Format.formatter -> t -> unit
end) =
struct
  let check_invariants = true

  module Interval = struct
    type nonrec t =
      { min_inclusive : Point.t;
        max_inclusive : Point.t
      }

    let[@ocamlformat "disable"] print ppf { min_inclusive; max_inclusive } =
      Format.fprintf ppf "@<hov 1>@[(\
          @[<hov 1>(min_inclusive@ %a)@]@ \
          @[<hov 1>(max_inclusive@ %a)@]\
          )@]"
        Point.print min_inclusive
        Point.print max_inclusive

    let contains { min_inclusive; max_inclusive } point =
      Point.compare point min_inclusive >= 0
      && Point.compare point max_inclusive <= 0

    let point_contained_in_or_after { min_inclusive; max_inclusive = _ } point =
      Point.compare point min_inclusive >= 0

    let overlaps
        ({ min_inclusive = min_inclusive1; max_inclusive = max_inclusive1 } as
        t1)
        ({ min_inclusive = min_inclusive2; max_inclusive = max_inclusive2 } as
        t2) =
      contains t1 min_inclusive2 || contains t1 max_inclusive2
      || contains t2 min_inclusive1 || contains t2 max_inclusive1

    let compare { min_inclusive = min_inclusive1; max_inclusive = _ }
        { min_inclusive = min_inclusive2; max_inclusive = _ } =
      Point.compare min_inclusive1 min_inclusive2

    let equal t1 t2 = Int.equal (compare t1 t2) 0
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
