(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Unified interface for binary emitters (x86 and arm64).

    This module defines abstract signatures that both architecture-specific
    binary emitters implement, allowing ocaml-jit to work with either. *)

type data_size = B8 | B16 | B32 | B64

(** Signature for relocation types with operations to evaluate them. *)
module type Relocation = sig
  type t

  val offset_from_section_beginning : t -> int

  val size : t -> data_size

  val target_symbol : t -> string

  (** Is this a GOT relocation? (JIT needs to know for GOT table building) *)
  val is_got_reloc : t -> bool

  (** Is this a PLT relocation? (JIT needs to know for PLT table building) *)
  val is_plt_reloc : t -> bool

  (** Compute the value to patch given:
      - [place_address]: where the relocation site is in memory
      - [lookup_symbol]: resolve symbol name -> address
      Returns the value to write at the relocation site, or an error. *)
  val compute_value :
    t ->
    place_address:int64 ->
    lookup_symbol:(string -> int64 option) ->
    (int64, string) result
end

(** Signature for assembled section types with operations. *)
module type Assembled_section = sig
  type t

  type relocation

  val size : t -> int

  val contents : t -> string

  val contents_mut : t -> bytes

  val relocations : t -> relocation list

  (** Find the offset of a symbol within this section *)
  val find_symbol_offset : t -> string -> int option

  (** Find the offset of a label within this section *)
  val find_label_offset : t -> string -> int option

  (** Iterate over all symbols defined in this section *)
  val iter_symbols : t -> f:(name:string -> offset:int -> unit) -> unit

  (** Patch bytes at the given offset *)
  val add_patch : t -> offset:int -> size:data_size -> data:int64 -> unit
end

(** Combined signature that each architecture's binary emitter provides. *)
module type S = sig
  module Relocation : Relocation

  module Assembled_section :
    Assembled_section with type relocation = Relocation.t
end
