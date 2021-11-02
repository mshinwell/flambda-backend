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
end) : sig
  type t

  val empty : t

  val add : t -> min_inclusive:Point.t -> max_inclusive:Point.t -> Datum.t -> t

  val find_exn : t -> Point.t -> Datum.t
end
