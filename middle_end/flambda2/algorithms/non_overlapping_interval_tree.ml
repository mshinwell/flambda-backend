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
  module Interval = struct
    type nonrec t =
      { min_inclusive : Point.t;
        max_exclusive : Point.t
      }

    let[@ocamlformat "disable"] print ppf { min_inclusive; max_exclusive } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(min_inclusive@ %a)@]@ \
          @[<hov 1>(max_exclusive@ %a)@]\
          )@]"
        Point.print min_inclusive
        Point.print max_exclusive

    let contains { min_inclusive; max_exclusive } point =
      Point.compare point min_inclusive >= 0
      && Point.compare point max_exclusive < 0

    let point_contained_in_or_after { min_inclusive; max_exclusive } point =
      Point.compare point min_inclusive < 0
      || Point.compare point max_exclusive <= 0

    let overlaps
        ({ min_inclusive = min_inclusive1; max_exclusive = max_exclusive1 } as
        t1)
        ({ min_inclusive = min_inclusive2; max_exclusive = max_exclusive2 } as
        t2) =
      contains t1 min_inclusive2 || contains t1 max_exclusive2
      || contains t2 min_inclusive1 || contains t2 max_exclusive1

    let compare { min_inclusive = min_inclusive1; max_exclusive = _ }
        { min_inclusive = min_inclusive2; max_exclusive = _ } =
      Point.compare min_inclusive1 min_inclusive2

    let equal t1 t2 = Int.equal (compare t1 t2) 0
  end

  module I = Interval
  module Set = Container_types.Make_set (I)
  module Map = Container_types.Make_map (I) (Set)

  type t = Datum.t Map.t

  let print ppf t = Map.print Datum.print ppf t

  let empty = Map.empty

  let[@inline always] find_exn0 t point =
    let interval, datum =
      Map.find_first
        (fun interval -> I.point_contained_in_or_after interval point)
        t
    in
    if I.contains interval point then interval, datum else raise Not_found

  let find_exn t point =
    let _interval, datum = find_exn0 t point in
    datum

  let add t ~min_inclusive ~max_exclusive datum =
    let interval = { I.min_inclusive; max_exclusive } in
    match find_exn0 t min_inclusive with
    | exception Not_found -> Map.add interval datum t
    | _, _ ->
      (* The new interval may overlap with more than one existing interval. *)
      t
      |> Map.filter (fun existing _ -> not (I.overlaps interval existing))
      |> Map.add interval datum
end
