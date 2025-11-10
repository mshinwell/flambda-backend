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

module S = Asm_targets.Asm_symbol
module String = Misc.Stdlib.String

type semaphore_data = string * Asm_targets.Asm_symbol.t * bool option

let probe_semaphores = ref String.Map.empty

let reset () =
  probe_semaphores := String.Map.empty

let find_or_add_semaphore name enabled_at_init dbg =
  match String.Map.find_opt name !probe_semaphores with
  | Some (label, symbol, e) ->
    (match e, enabled_at_init with
    | None, None -> ()
    | None, Some _ ->
      let d = label, symbol, enabled_at_init in
      probe_semaphores
        := String.Map.remove name !probe_semaphores |> String.Map.add name d
    | Some _, None ->
      (* [find_or_add_semaphore] is called with None for Iprobe_is_enabled
         during code emission only. [find_or_add_semaphore] is called with Some
         to emit probe notes only after all code is emitted. *)
      assert false
    | Some b, Some b' ->
      if not (Bool.equal b b')
      then raise (Emitaux.Error (Inconsistent_probe_init (name, dbg))));
    label
  | None ->
    let sym = "caml_probes_semaphore_" ^ name in
    let symbol = S.Predef.caml_probes_semaphore ~name in
    let d = sym, symbol, enabled_at_init in
    probe_semaphores := String.Map.add name d !probe_semaphores;
    sym

let iter f =
  String.Map.iter f !probe_semaphores

let is_empty () =
  String.Map.is_empty !probe_semaphores

let iter_excluding names f =
  let filtered =
    List.fold_left
      (fun acc name -> String.Map.remove name acc)
      !probe_semaphores names
  in
  String.Map.iter f filtered
