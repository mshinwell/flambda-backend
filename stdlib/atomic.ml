(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type !'a t =
  { mutable contents : 'a [@atomic] }

external make
  : 'a -> 'a t
  = "%makemutable"

external make_contended
  : 'a -> 'a t
  = "caml_atomic_make_contended"

external get
  : 'a t -> 'a
  = "%atomic_load"

external set
  : 'a t -> 'a -> unit
  = "%atomic_set"

external exchange
  : 'a t -> 'a -> 'a
  = "%atomic_exchange"

external compare_and_set
  : 'a t -> 'a -> 'a -> bool
  = "%atomic_cas"

external compare_exchange
  : 'a t -> 'a -> 'a -> 'a
  = "%atomic_compare_exchange"

external fetch_and_add
  :  int t
  -> int
  -> int
  = "%atomic_fetch_add"

external add
  :  int t
  -> int
  -> unit
  = "%atomic_add"

external sub
  :  int t
  -> int
  -> unit
  = "%atomic_sub"

external logand
  :  int t
  -> int
  -> unit
  = "%atomic_land"

external logor
  :  int t
  -> int
  -> unit
  = "%atomic_lor"

external logxor
  :  int t
  -> int
  -> unit
  = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

external get_contended
  : 'a t -> 'a
  = "%atomic_load"

module Loc = struct
  type 'a t = 'a atomic_loc
  external get : 'a t -> 'a = "%atomic_load_loc"
  external set : 'a t -> 'a -> unit = "%atomic_set_loc"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange_loc"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"
  external compare_exchange : 'a t -> 'a -> 'a -> 'a = "%atomic_compare_exchange_loc"

  external fetch_and_add
    : int t -> int -> int
    = "%atomic_fetch_add_loc"

  external add
    : int t -> int -> unit = "%atomic_add_loc"

  external sub
    : int t -> int -> unit = "%atomic_sub_loc"

  external logand
    : int t -> int -> unit = "%atomic_land_loc"

  external logor
    : int t -> int -> unit = "%atomic_lor_loc"

  external logxor
    : int t -> int -> unit = "%atomic_lxor_loc"

  let incr t = add t 1
  let decr t = sub t 1

  external get_contended : 'a t -> 'a = "%atomic_load_loc"
end
