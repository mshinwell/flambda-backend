(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

(** Generic relocation using the unified Binary_emitter interface *)
module Generic : sig
  (** Type for looking up symbol addresses in GOT/PLT tables *)
  type table_lookup = string -> Address.t option

  val all :
    (module Binary_emitter.S
       with type Assembled_section.t = 'a
        and type Relocation.t = 'r) ->
    symbols:Symbols.t ->
    got_lookup:table_lookup option ->
    plt_lookup:table_lookup option ->
    section_name:string ->
    'a addressed ->
    (unit, string list) result
end

(** X86-specific relocations (legacy interface) *)

val all_text :
  symbols:Symbols.t ->
  got:Bin_table.filled Jit_got.t addressed ->
  plt:Bin_table.filled Jit_plt.t addressed ->
  X86_binary_emitter.buffer addressed ->
  (unit, string list) result
(** Apply all relocations to the given .text binary section as patches.
    Symbols' absolute addresses are looked up using the provided tables for PLT and GOT based
    relocations or the symbol map for other relocations.
    It will return an error before applying any relocation if one or more of them can't properly be parsed
    It will also return an error if a GOT or PLT relocation is marked as absolute but will still apply
    other relocations.
    Errors for either of the above mentioned cases are aggregated into a list. *)

val all :
  symbols:Symbols.t ->
  section_name:string ->
  X86_binary_emitter.buffer addressed ->
  (unit, string list) result
(** Same as [apply_all_text] but for any other section.
    The section is expected to contain no GOT nor PLT based relocations. If any such relocation is found
    the function will return an error but still apply other relocations. *)
