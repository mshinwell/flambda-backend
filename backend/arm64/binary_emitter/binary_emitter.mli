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

open Arm64_ast

module Relocation : sig
  module Kind : sig
    type t = private
      | R_AARCH64_ADR_PREL_LO21 of string
      | R_AARCH64_ADR_PREL_PG_HI21 of string
      | R_AARCH64_LD64_GOT_LO12_NC of string
      | R_AARCH64_ADD_ABS_LO12_NC of string
      | R_AARCH64_CALL26 of string
      | R_AARCH64_JUMP26 of string
      | R_AARCH64_ABS64 of string
      | R_AARCH64_PREL32_PAIR of
          { plus_symbol : string;
            minus_symbol : string
          }
  end

  type t = private
    { offset_from_section_beginning : int;
      kind : Kind.t
    }
end

module Section_state : sig
  type t

  val buffer : t -> Buffer.t

  val find_symbol_offset_in_bytes : t -> string -> int option

  val find_label_offset_in_bytes : t -> string -> int option

  val relocations : t -> Relocation.t list

  val symbols : t -> (string, int) Hashtbl.t

  (** Returns mutable bytes with all patches applied. *)
  val contents_mut : t -> bytes

  (** Returns string with all patches applied. *)
  val contents : t -> string
end

type t

val create : unit -> t

val add_instruction : t -> Instruction.t -> unit

val add_directive : t -> Asm_targets.Asm_directives.Directive.t -> unit

val emit : t -> Section_state.t Asm_targets.Asm_section.Tbl.t

(** Module implementing Binary_emitter.S for use by ocaml-jit *)
module For_jit :
  Binary_emitter.S
    with type Assembled_section.t = Section_state.t
     and type Relocation.t = Relocation.t
