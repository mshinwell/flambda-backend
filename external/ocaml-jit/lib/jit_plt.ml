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

type 'a t = 'a Bin_table.t

let from_binary_section binary_section =
  let module E = X86_binary_emitter.For_jit in
  Bin_table.from_binary_section (module E)
    ~name:"PLT"
    ~entry_size:E.Plt.entry_size
    ~is_relevant_reloc:E.Relocation.is_plt_reloc
    ~write_entry:(fun buf addr -> E.Plt.write_entry buf (Address.to_int64 addr))
    binary_section

let fill = Bin_table.fill

let in_memory_size = Bin_table.in_memory_size

let content = Bin_table.content

let symbol_address = Bin_table.symbol_address
