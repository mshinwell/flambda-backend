(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let hash_seed =
  let seed = Random.bits () in
  if seed mod 2 = 0 then seed + 1 else seed

let hash2 a b =
  let a = Hashtbl.hash a in
  let b = Hashtbl.hash b in
  let r = (a * hash_seed) + b in
  r lxor (r lsr 17)
