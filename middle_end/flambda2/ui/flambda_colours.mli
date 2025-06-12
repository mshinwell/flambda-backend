(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** A colour directive. Can be passed as an argument to [Format.printf] and
    frients using the "%t" specifier. Each directive (besides [pop]) acts by
    pushing a new state onto a stack, allowing the previous state to be restored
    using [pop]. *)
type directive = Format.formatter -> unit

(** Undo the most recent colour directive, restoring the previous state. Raises
    a fatal error if the stack is empty. *)
val pop : directive

(** Push a copy of the current state onto the stack. Useful when setting a
    colour conditionally so that a following [pop] will always be matched. *)
val none : directive

val prim_constructive : directive

val prim_destructive : directive

val prim_neither : directive

val naked_number : directive

val tagged_immediate : directive

val constructor : directive

val kind : directive

val subkind : directive

val top_or_bottom_type : directive

val debuginfo : directive

val discriminant : directive

val name : directive

val parameter : directive

val symbol : directive

val variable : directive

val function_slot : directive

val value_slot : directive

val code_id : directive

val expr_keyword : directive

val invalid_keyword : directive

val static_keyword : directive

val static_part : directive

val continuation : directive

val continuation_definition : directive

val continuation_annotation : directive

val name_abstraction : directive

val rec_info : directive

val coercion : directive

val depth_variable : directive

val elide : directive

val error : directive

val each_file : directive

val lambda : directive

val unboxed_product : directive

val effect : directive

val without_colours : f:(unit -> 'a) -> 'a
