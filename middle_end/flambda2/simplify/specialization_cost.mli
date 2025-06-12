(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** {Continuation Specialization Cost}

    Continuation specialization is done in simplify on the way down.
    Consider a term of the form: *)

(** * let_cont k x =
    *   let_cont k' y =
    *     ...
    *   in
    *   ..
    *   switch x with
    *   | 0 -> k' 0
    *   | 1 -> k' 1
    * in
    * ..
    * switch .. with
    * | 0 -> k 0
    * | 1 -> k 1
    *)

(** The decision to specialize continuation k
    is made once Simplify has reached the bottom of the handler of k.
    This allows to know all of the relevant
    information before deciding whether to specialize [k]: all uses of [k] have been
    seen, and we have also seen the code for the handler of [ĸ].

    The type [t] represents an accumulator updated while doing the downwards pass
    on the handler of [k] to record enough information to estimate the cost of a
    specialization of [k], and whether there was any reason to **not** specialize
    [k] regardless of the cost *)

type reason =
  | At_toplevel
  | Contains_static_consts
  | Contains_set_of_closures

type cost = { size_of_primitives : int }

(** The current specialization status, stored in the dacc. *)
type t = private
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }
(**)

(** Printing function. *)
val print : Format.formatter -> t -> unit

(** {2 Creation} *)

(** Create a [Can_specialize] value with zero intial cost. *)
val can_specialize : t

(** Create a value preventing any specialization of the current continuation. *)
val cannot_specialize : reason -> t

(** {2 Updating Costs} *)

(** Add a primitive of the given size to the cost of specialization *)
val add_prim : Flambda_primitive.t -> t -> t

(** Add a set of closure containing [~num] closures to the cost of specialization. *)
val add_set_of_closures : Set_of_closures.t -> t -> t
