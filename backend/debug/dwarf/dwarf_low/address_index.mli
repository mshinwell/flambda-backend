(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Indexes into the .debug_addr table.

    Note that these indexes might refer to a constant (c.f. DW_OP_constx) whose
    computation required relocation, rather than to a single address.  The
    name "address index" is thus potentially confusing, but we keep it to
    match the DWARF specification.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets

(* XXX should this have submodules or a phantom param? *)
type t

val zero : t

val succ : t -> t

include Identifiable.S with type t := t

val size : t -> Dwarf_int.t

val emit : asm_directives:Asm_directives_dwarf.t -> ?comment:string -> t -> unit

module Pair : Identifiable.S with type t = t * t
