(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Recognition of logical immediate arguments *)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )
 *
 *             0          1          0
 *            / \        / \        / \
 *            \ /        \ /        \ /
 *      -0--> [1] --1--> [2] --0--> [3]
 *     /
 *   [0]
 *     \
 *      -1--> [4] --0--> [5] --1--> [6]
 *            / \        / \        / \
 *            \ /        \ /        \ /
 *             1          0          1
 *
 * The accepting states are 2, 3, 5 and 6. *)
let auto_table =
  [| (* accepting?, next on 0, next on 1 *)
     (* state 0 *)
     false, 1, 4;
     (* state 1 *) false, 1, 2;
     (* state 2 *) true, 3, 2;
     (* state 3 *) true, 3, 7;
     (* state 4 *) false, 5, 4;
     (* state 5 *) true, 5, 6;
     (* state 6 *) true, 7, 6;
     (* state 7 *) false, 7, 7 (* error state *)
  |]

let rec run_automata nbits state input =
  let acc, next0, next1 = auto_table.(state) in
  if nbits <= 0
  then acc
  else
    run_automata (nbits - 1)
      (if Nativeint.equal (Nativeint.logand input 1n) 0n then next0 else next1)
      (Nativeint.shift_right_logical input 1)

(* The following function determines a length [e] such that [x] is a repetition
   [BB...B] of a bit pattern [B] of length [e]. [e] ranges over 64, 32, 16, 8,
   4, 2. The smaller [e] the better. *)

let logical_imm_length x =
  (* [test n] checks that the low [2n] bits of [x] are of the form [BB], that
     is, two occurrences of the same [n] bits *)
  let test n =
    let mask = Nativeint.(sub (shift_left 1n n) 1n) in
    let low_n_bits = Nativeint.(logand x mask) in
    let next_n_bits = Nativeint.(logand (shift_right_logical x n) mask) in
    Nativeint.equal low_n_bits next_n_bits
  in
  (* If [test n] fails, we know that the length [e] is at least [2n]. Hence we
     test with decreasing values of [n]: 32, 16, 8, 4, 2. *)
  if not (test 32)
  then 64
  else if not (test 16)
  then 32
  else if not (test 8)
  then 16
  else if not (test 4)
  then 8
  else if not (test 2)
  then 4
  else 2

(* A valid logical immediate is - neither [0] nor [-1]; - composed of a
   repetition [BBBBB] of a bit-pattern [B] of length [e] - the low [e] bits of
   the number, that is, [B], match [0+1+0*] or [1+0+1*]. *)

let is_logical_immediate x =
  (not (Nativeint.equal x 0n))
  && (not (Nativeint.equal x (-1n)))
  && run_automata (logical_imm_length x) 0 x

(* XXX mshinwell: this needs checking carefully *)
(* Encode a logical immediate into N, immr, imms fields for ARM64 instructions.
   Returns (N, immr, imms) tuple.

   The encoding works as follows:

   - N=1 for 64-bit patterns, N=0 for smaller element sizes

   - Element size is the smallest repeating pattern (2, 4, 8, 16, 32, or 64
   bits)

   - Within each element, we have a contiguous run of 1s (possibly rotated)

   - imms encodes both the element size and number of 1s

   - immr encodes the rotation amount *)
let encode_logical_immediate_fields (x : nativeint) : int * int * int =
  if not (is_logical_immediate x)
  then
    invalid_arg "encode_logical_immediate_fields: not a valid logical immediate";
  let len = logical_imm_length x in
  let pattern = Nativeint.(logand x (sub (shift_left 1n len) 1n)) in
  (* Find the rightmost set bit position (start of ones run if not rotated) *)
  let rec find_first_one p pos =
    if pos >= len
    then 0
    else if Nativeint.equal (Nativeint.logand p 1n) 1n
    then pos
    else find_first_one (Nativeint.shift_right_logical p 1) (pos + 1)
  in
  (* Count consecutive ones starting from position *)
  let rec count_ones p pos count =
    if pos >= len || Nativeint.equal (Nativeint.logand p 1n) 0n
    then count
    else count_ones (Nativeint.shift_right_logical p 1) (pos + 1) (count + 1)
  in
  (* Rotate pattern right to find canonical form (ones at LSB) *)
  let rec find_rotation p rot =
    if rot >= len
    then 0, 0 (* shouldn't happen for valid immediates *)
    else
      let first_one = find_first_one p 0 in
      if first_one = 0
      then
        let ones = count_ones p 0 0 in
        rot, ones
      else
        (* Rotate right by 1 *)
        let bit = Nativeint.(logand p 1n) in
        let p' =
          Nativeint.(logor (shift_right_logical p 1) (shift_left bit (len - 1)))
        in
        find_rotation p' (rot + 1)
  in
  let rotation, ones = find_rotation pattern 0 in
  (* Encode N based on element size *)
  let n = if len = 64 then 1 else 0 in
  (* immr is the rotation amount *)
  let immr = rotation in
  (* imms encodes the element size and number of ones Format:
     NOT(element_size_encoding) : (ones - 1) Element size encoding: 0=64, 10=32,
     110=16, 1110=8, 11110=4, 111110=2 *)
  let size_encoding =
    match len with
    | 64 -> 0b000000
    | 32 -> 0b100000
    | 16 -> 0b110000
    | 8 -> 0b111000
    | 4 -> 0b111100
    | 2 -> 0b111110
    | _ -> invalid_arg "invalid element size"
  in
  let imms = size_encoding lor (ones - 1) in
  n, immr, imms
