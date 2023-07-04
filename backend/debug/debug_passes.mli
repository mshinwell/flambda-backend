(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The sequence of passes required after [Linearize] code generation in
    order to generate debugging information.  These passes may rewrite the
    [Linearize] code.

    There is one other pass required for debugging information generation,
    [Available_regs].  This is run directly from [Asmgen] as the pass operates
    on the [Mach] language.  The output of the pass is transported to this
    module via annotations on the [Mach] and [Linearize] instructions.
*)

type result = private
  { fundecl : Linear.fundecl;
    available_ranges_vars : Available_ranges_all_vars.t
  }

val passes_for_fundecl_and_emit :
  emit:(Linear.fundecl -> unit) -> Linear.fundecl -> result
