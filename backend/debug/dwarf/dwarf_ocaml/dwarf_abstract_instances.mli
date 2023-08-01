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

(** Management of DWARF "abstract instances" for functions. *)

open! Asm_targets
open! Dwarf_low
open! Dwarf_high

val attributes : Linear.fundecl -> Dwarf_attribute_values.Attribute_value.t list

val find_or_add :
  Dwarf_state.t ->
  function_proto_die:Proto_die.t ->
  Linear.fundecl ->
  Proto_die.t * Asm_symbol.t
(* val find_maybe_in_another_unit_or_add : Dwarf_state.t ->
   function_proto_die:Proto_die.t -> Linear.fundecl -> Asm_symbol.t option *)
