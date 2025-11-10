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

(** Probe semaphore management for code emission *)

(** Semaphore data: (label_string, symbol, enabled_at_init option) *)
type semaphore_data = private string * Asm_targets.Asm_symbol.t * bool option

(** Reset the probe semaphore registry *)
val reset : unit -> unit

(** Find or add a semaphore for a probe name.
    Returns the label string for the semaphore symbol.
    - [name]: probe name
    - [enabled_at_init]: whether the probe is enabled at initialization
    - [dbg]: debug info for error reporting
    Raises Emitaux.Error (Inconsistent_probe_init ...) if the same probe
    is used with different enabled_at_init values. *)
val find_or_add_semaphore : string -> bool option -> Debuginfo.t -> string

(** Iterate over all registered semaphores *)
val iter : (string -> semaphore_data -> unit) -> unit

(** Check if there are any registered semaphores *)
val is_empty : unit -> bool

(** Iterate over semaphores excluding those with names in the given list *)
val iter_excluding : string list -> (string -> semaphore_data -> unit) -> unit
