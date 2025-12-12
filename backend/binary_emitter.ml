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

type data_size = B8 | B16 | B32 | B64

module type Relocation = sig
  type t

  val offset_from_section_beginning : t -> int

  val size : t -> data_size

  val target_symbol : t -> string

  val is_got_reloc : t -> bool

  val is_plt_reloc : t -> bool

  val compute_value :
    t ->
    place_address:int64 ->
    lookup_symbol:(string -> int64 option) ->
    (int64, string) result
end

module type Assembled_section = sig
  type t

  type relocation

  val size : t -> int

  val contents : t -> string

  val contents_mut : t -> bytes

  val relocations : t -> relocation list

  val find_symbol_offset : t -> string -> int option

  val find_label_offset : t -> string -> int option

  val iter_symbols : t -> f:(name:string -> offset:int -> unit) -> unit

  val add_patch : t -> offset:int -> size:data_size -> data:int64 -> unit
end

module type S = sig
  module Relocation : Relocation

  module Assembled_section :
    Assembled_section with type relocation = Relocation.t
end

type arch =
  | Amd64
  | Arm64

let arch =
  match Target_system.architecture () with
  | X86_64 -> Amd64
  | AArch64 -> Arm64
  | _ -> failwith "Binary_emitter: unsupported architecture"
