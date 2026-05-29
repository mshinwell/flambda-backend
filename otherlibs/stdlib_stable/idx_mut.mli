(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Ryan Tjoa, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable indices into blocks. *)

(** An alias for the type of mutable indices into blocks. *)
type ('a, 'b) t : bits64 mod everything = ('a, 'b) idx_mut

(** [get a i] uses the index [i] to access [a]. *)
external get
  : 'a -> ('a, 'b) idx_mut -> 'b
  = "%get_idx"

(** [set a i v] uses the index [i] to set [a] to [v].

    It can take [a] locally and [v] globally because mutable indices (e.g. to
    array elements or mutable record fields) can only be created to elements
    with the [global] modality. *)
external set
  : 'a -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"

(** [unsafe_create_into_array i] creates an index into the [i]th element of an
    array.

    This is unsafe because it cannot check array bounds, so calling [get]/[set]
    with the index later could perform an unchecked out-of-bounds access. *)
external unsafe_create_into_array
  : int -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx"

external unsafe_create_into_array_indexed_by_int8
  : int8# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int8#"

external unsafe_create_into_array_indexed_by_int16
  : int16# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int16#"

external unsafe_create_into_array_indexed_by_int32
  : int32# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int32#"

external unsafe_create_into_array_indexed_by_int64
  : int64# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int64#"

external unsafe_create_into_array_indexed_by_nativeint
  : nativeint# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_nativeint#"
