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

    let contains_excl { min_inclusive; max_exclusive } point =
      Point.compare point min_inclusive >= 0
      && Point.compare point max_exclusive <= 0

    let overlaps
        ({ min_inclusive = min_inclusive1; max_exclusive = max_exclusive1 } as
        t1)
        ({ min_inclusive = min_inclusive2; max_exclusive = max_exclusive2 } as
        t2) =
      contains t1 min_inclusive2
      || contains_excl t1 max_exclusive2
      || contains t2 min_inclusive1
      || contains_excl t2 max_exclusive1
  end

  module I = Interval

  (* CR mshinwell: Produce a more efficient implementation. *)

  type t = (I.t * Datum.t) list

  let print ppf t =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (interval, datum) ->
        Format.fprintf ppf "@[<hov 1>(%a@ %a)@]" Interval.print interval
          Datum.print datum)
      ppf t

  let empty = []

  let rec find_exn0 t point =
    match t with
    | [] -> raise Not_found
    | (interval, datum) :: t ->
      if I.contains interval point then interval, datum else find_exn0 t point

  let find_exn t point =
    let _interval, datum = find_exn0 t point in
    datum

  let add t ~min_inclusive ~max_exclusive datum =
    let interval = { I.min_inclusive; max_exclusive } in
    match find_exn0 t min_inclusive with
    | exception Not_found -> (interval, datum) :: t
    | _, _ ->
      (* The new interval may overlap with more than one existing interval. *)
      t
      |> List.filter (fun (existing, _) -> not (I.overlaps interval existing))
      |> List.cons (interval, datum)
end
