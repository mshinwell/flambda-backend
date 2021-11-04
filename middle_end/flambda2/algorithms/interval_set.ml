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

type t = int array

(* An initial domain of 0 to 188 (on a 64-bit machine) occupying four words
   without resizing seems reasonable. *)
let create () = [| 0; 0; 0 |]

let add (t : t) ~min_inclusive ~max_exclusive =
  if min_inclusive < 0 || max_exclusive <= min_inclusive
  then
    Misc.fatal_errorf
      "Invalid argument(s) to [Interval_set.add]:@ min_inclusive = %d,@ \
       max_exclusive = %d"
      min_inclusive max_exclusive;
  let max_inclusive = max_exclusive - 1 in
  let existing_length = Array.length t in
  let first_index = min_inclusive / Sys.int_size in
  let last_index = max_inclusive / Sys.int_size in
  let t =
    if first_index >= existing_length || last_index >= existing_length
    then
      Array.init
        ((last_index + 1) * 2)
        (fun index ->
          if index < existing_length then Array.unsafe_get t index else 0)
    else Array.copy t
  in
  let current_index = ref (min_inclusive / Sys.int_size) in
  let last_index = max_inclusive / Sys.int_size in
  let first_bit_of_current_index = ref (!current_index * Sys.int_size) in
  while !current_index <= last_index do
    let first_bit_to_set_within_index =
      if min_inclusive < !first_bit_of_current_index
      then 0
      else min_inclusive mod Sys.int_size
    in
    let last_bit_of_current_index =
      !first_bit_of_current_index + Sys.int_size - 1
    in
    let last_bit_to_set_within_index =
      if max_inclusive > last_bit_of_current_index
      then Sys.int_size - 1
      else max_inclusive mod Sys.int_size
    in
    let mask =
      (min_int asr (Sys.int_size - first_bit_to_set_within_index - 1))
      lxor (min_int asr (Sys.int_size - last_bit_to_set_within_index - 1))
    in
    let word = Array.unsafe_get t !current_index in
    Array.unsafe_set t !current_index (word lor mask);
    first_bit_of_current_index := !first_bit_of_current_index + Sys.int_size;
    incr current_index
  done;
  t

let mem (t : t) point =
  let index = point / Sys.int_size in
  if index < 0 || index >= Array.length t
  then false
  else
    let word = Array.unsafe_get t index in
    let bit_offset_into_word = point mod Sys.int_size in
    word land (1 lsl bit_offset_into_word) <> 0
