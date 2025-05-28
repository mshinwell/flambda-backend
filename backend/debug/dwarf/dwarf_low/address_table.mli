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

(** Management of the .debug_addr table (DWARF-5 spec section 7.2.7, page 241). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets

module Entry : sig
  type t

  val address_in_section :
    ?offset:Targetint.t -> section_symbol:Asm_symbol.t -> Asm_label.t -> t

  val distance_between_labels :
    ?offset_upper:Targetint.t ->
    lower:Asm_label.t ->
    upper:Asm_label.t ->
    unit ->
    t
end

type t

val create : unit -> t

val add : t -> Entry.t -> Address_index.t

(** The label to be used as the value of the [DW_AT_base] attribute (DWARF-5
    spec page 66 line 14). *)
val base_addr : t -> Asm_label.t

include Dwarf_emittable.S with type t := t
