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

type need_reloc = Bin_table.empty

type relocated = Bin_table.filled

let name = ".text"

type 'a t = {
  binary_section : X86_binary_emitter.buffer;
  got : 'a Jit_got.t;
  plt : 'a Jit_plt.t;
}

let from_binary_section binary_section =
  let got = Jit_got.from_binary_section binary_section in
  let plt = Jit_plt.from_binary_section binary_section in
  { binary_section; got; plt }

let in_memory_size { binary_section; got; plt } =
  let section_size = X86_binary_emitter.size binary_section in
  let got_size = Jit_got.in_memory_size got in
  let plt_size = Jit_plt.in_memory_size plt in
  section_size + got_size + plt_size

let relocate ~symbols (t : need_reloc t addressed) =
  let open Result.Op in
  let got_address =
    Address.add_int t.address (X86_binary_emitter.size t.value.binary_section)
  in
  let got = Jit_got.fill symbols t.value.got in
  let plt_address = Address.add_int got_address (Jit_got.in_memory_size got) in
  let plt = Jit_plt.fill symbols t.value.plt in
  let+ () =
    Relocate.all_text ~symbols
      ~got:{ address = got_address; value = got }
      ~plt:{ address = plt_address; value = plt }
      { address = t.address; value = t.value.binary_section }
  in
  let value = { t.value with got; plt } in
  { t with value }

let content t =
  X86_binary_emitter.contents t.binary_section
  ^ Jit_got.content t.got ^ Jit_plt.content t.plt

let symbols { address; value = t } =
  Symbols.from_binary_section { address; value = t.binary_section }

(* Generic implementation using the unified Binary_emitter interface.
   Note: This doesn't type-erase the binary section - the type parameters
   must be consistent across all operations. *)
module Generic = struct
  type need_reloc = Bin_table.empty

  type relocated = Bin_table.filled

  type ('section, 'reloc_state) t = {
    binary_section : 'section;
    got : 'reloc_state Bin_table.Generic.t;
    plt : 'reloc_state Bin_table.Generic.t;
  }

  let from_binary_section (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      (section : a) : (a, need_reloc) t =
    let got =
      Bin_table.Generic.from_binary_section
        (module E) ~name:"GOT" ~entry_size:Address.size
        ~is_relevant_reloc:E.Relocation.is_got_reloc
        ~write_entry:Address.emit section
    in
    let plt =
      Bin_table.Generic.from_binary_section
        (module E) ~name:"PLT" ~entry_size:E.Plt.entry_size
        ~is_relevant_reloc:E.Relocation.is_plt_reloc
        ~write_entry:(fun buf addr -> E.Plt.write_entry buf (Address.to_int64 addr))
        section
    in
    { binary_section = section; got; plt }

  let in_memory_size (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      (t : (a, _) t) =
    E.Assembled_section.size t.binary_section
    + Bin_table.Generic.in_memory_size t.got
    + Bin_table.Generic.in_memory_size t.plt

  let relocate (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      ~symbols (t : (a, need_reloc) t addressed) =
    let open Result.Op in
    let section_size = E.Assembled_section.size t.value.binary_section in
    let got_address = Address.add_int t.address section_size in
    let got = Bin_table.Generic.fill symbols t.value.got in
    let plt_address =
      Address.add_int got_address (Bin_table.Generic.in_memory_size got)
    in
    let plt = Bin_table.Generic.fill symbols t.value.plt in
    let got_lookup =
      Some (fun name -> Bin_table.Generic.symbol_address { address = got_address; value = got } name)
    in
    let plt_lookup =
      Some (fun name -> Bin_table.Generic.symbol_address { address = plt_address; value = plt } name)
    in
    let+ () =
      Relocate.Generic.all (module E) ~symbols
        ~got_lookup ~plt_lookup
        ~section_name:name
        { address = t.address; value = t.value.binary_section }
    in
    let value = { t.value with got; plt } in
    { t with value }

  let content (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      (t : (a, relocated) t) =
    E.Assembled_section.contents t.binary_section
    ^ Bin_table.Generic.content t.got
    ^ Bin_table.Generic.content t.plt

  let symbols (type a r)
      (module E : Binary_emitter.S
        with type Assembled_section.t = a
         and type Relocation.t = r)
      { address; value = t } =
    Symbols.from_binary_section_generic (module E) { address; value = t.binary_section }
end
