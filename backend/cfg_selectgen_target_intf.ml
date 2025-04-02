(* -------------------------------------------------------------------------- *
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

(** Interface to be satisfied by target-specific code, for instruction
    selection. *)

module type S = sig
  val is_immediate :
    Simple_operation.integer_operation ->
    int ->
    Select_utils.is_immediate_result

  val is_immediate_test :
    Simple_operation.integer_comparison ->
    int ->
    Select_utils.is_immediate_result

  val is_simple_expr : Cmm.expression -> Select_utils.is_simple_expr_result

  val effects_of : Cmm.expression -> Select_utils.effects_of_result

  val select_addressing :
    Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

  val select_operation :
    Cmm.operation ->
    Cmm.expression list ->
    label_after:Label.t ->
    Select_utils.select_operation_result

  val select_store :
    is_assign:bool ->
    Arch.addressing_mode ->
    Cmm.expression ->
    Select_utils.select_store_result

  val is_store_out_of_range :
    Cmm.memory_chunk ->
    byte_offset:int ->
    Select_utils.is_store_out_of_range_result

  val insert_move_extcall_arg :
    Cmm.exttype ->
    Reg.t array ->
    Reg.t array ->
    Select_utils.insert_move_extcall_arg_result
end
