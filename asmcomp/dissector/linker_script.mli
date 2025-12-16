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

(** Generate linker scripts for the dissector.

    This module generates a linker script that places partition sections
    in the correct output sections. It optionally incorporates an existing
    linker script provided via --script= on the linker command line. *)

(** [generate ~existing_script ~blocks] generates a linker script string.

    @param existing_script Optional path to an existing linker script to include.
      TODO: This should be extracted from the linker command line flags
      (looking for --script=<path>).
    @param blocks List of block/partition names. The "main" block is skipped
      as it uses standard section names. *)
val generate : existing_script:string option -> blocks:string list -> string

(** [write ~output_file ~existing_script ~blocks] generates a linker script
    and writes it to the specified file.

    @param output_file Path to write the linker script to. *)
val write :
  output_file:string ->
  existing_script:string option ->
  blocks:string list ->
  unit
