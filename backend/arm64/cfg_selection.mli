(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*   Copyright 2025 Jane Street Group LLC.                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val is_immediate : int -> bool

val select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth

val is_immediate : 'a -> 'b -> Select_utils.is_immediate_result

val is_immediate_test : 'a -> 'b -> 'c -> Select_utils.is_immediate_result

val is_simple_expr : Cmm.expression -> Select_utils.is_simple_expr_result

val effects_of : Cmm.expression -> Select_utils.effects_of_result

val select_addressing :
  Cmm.memory_chunk ->
  Cmm.expression ->
  Arch.addressing_mode * Cmm.expression ->
  Select_utils.select_addressing_result

val select_operation :
  Cmm.operation ->
  Cmm.expression list ->
  label_after:Label.t ->
  Select_utils.select_operation_result

val select_store :
  is_assign:'a -> 'b -> 'c -> byte_offset:'d -> Select_utils.select_store_result

val insert_move_extcall_arg :
  'a -> 'b -> 'c -> Reg.t array -> Select_utils.insert_move_extcall_arg_result
