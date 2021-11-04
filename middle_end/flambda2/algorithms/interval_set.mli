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

(** Fast implementation of an interval set using extensible bit vectors. The
    domain is non-negative integers. *)

type t

val create : unit -> t

(** Requirements on the bounds of a new interval:

    min_inclusive >= 0

    max_exclusive > min_inclusive *)
val add : t -> min_inclusive:int -> max_exclusive:int -> t

val mem : t -> int -> bool
