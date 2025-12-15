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

type t =
  { igot : Igot.t;
    iplt : Iplt.t;
    plt_symbols : string list;
    got_symbols : string list
  }

let build ~prefix (relocations : Extract_relocations.t) =
  (* Extract unique symbol names from PLT32 relocations *)
  let plt_symbols =
    List.map
      (fun (r : Extract_relocations.relocation_entry) -> r.symbol_name)
      relocations.convert_to_plt
  in
  (* Extract unique symbol names from GOTPCRELX relocations *)
  let got_only_symbols =
    List.map
      (fun (r : Extract_relocations.relocation_entry) -> r.symbol_name)
      relocations.convert_to_got
  in
  (* IGOT needs entries for both PLT symbols (PLT jumps through GOT) and
     GOT-only symbols. Combine the lists - Igot.build will deduplicate. *)
  let all_got_symbols = plt_symbols @ got_only_symbols in
  (* Build IGOT first (IPLT depends on it) *)
  let igot = Igot.build ~prefix all_got_symbols in
  (* Build IPLT for PLT symbols only *)
  let iplt = Iplt.build ~prefix ~igot plt_symbols in
  { igot; iplt; plt_symbols; got_symbols = got_only_symbols }

let igot_symbol_for_got_reloc t (reloc : Extract_relocations.relocation_entry) =
  match Igot.find_entry t.igot reloc.symbol_name with
  | None -> None
  | Some entry -> Some entry.igot_symbol

let iplt_symbol_for_plt_reloc t (reloc : Extract_relocations.relocation_entry) =
  match Iplt.find_entry t.iplt reloc.symbol_name with
  | None -> None
  | Some entry -> Some entry.iplt_symbol
