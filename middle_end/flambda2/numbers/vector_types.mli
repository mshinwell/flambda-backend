(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** SIMD vector numeric type layouts. *)

module Vec128 : sig
  module Bit_pattern : sig
    (** 128-bit value whose comparison and equality relations are lexicographically
      ordered by bit pattern. *)

    include Container_types.S

    val zero : t

    type bits =
      { high : int64;
        low : int64
      }

    val to_bits : t -> bits

    val of_bits : bits -> t
  end
end
