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

(** Build linker arguments from dissector results.

    After the dissector runs, all original object files (.o, .a) have been
    partitioned and partially linked into partition object files. This module
    constructs the replacement linker arguments that should be used instead
    of the original files.

    The original linker invocation combines:
    - startup_obj (the startup code)
    - ml_objfiles (OCaml .o files derived from .cmx/.cmxa)
    - ccobjs (C object files from -cclib)
    - runtime_libs (runtime library files)

    After dissector processing, all of these are baked into the rewritten
    partition .o files, so we replace them all with just the partition files
    plus a linker script. *)

(** The linker arguments produced by the dissector. *)
type t =
  { object_files : string list;
        (** The rewritten partition .o files that replace all original object
            files. These are the partially-linked partition files after
            IGOT/IPLT rewriting. *)
    linker_script : string
        (** Path to the generated linker script. This should be passed to the
            linker via -T flag. *)
  }

(** [build result] constructs linker arguments from a dissector result.

    Returns the list of rewritten partition object files (with .rewritten
    suffix) and the linker script path. The caller should:
    1. Pass all [object_files] to the linker instead of the original files
    2. Pass [-T linker_script] to include the generated linker script *)
val build : Dissector.result -> t
