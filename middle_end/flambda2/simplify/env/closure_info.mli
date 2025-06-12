(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  | Not_in_a_closure
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure
  | Closure of
      { code_id : Code_id.t;
        return_continuation : Continuation.t;
        exn_continuation : Continuation.t;
        my_closure : Variable.t
      }

val print : Format.formatter -> t -> unit

val not_in_a_closure : t

val in_a_set_of_closures : t

val in_a_closure :
  Code_id.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  my_closure:Variable.t ->
  t

type in_or_out_of_closure =
  | In_a_closure
  | Not_in_a_closure

val in_or_out_of_closure : t -> in_or_out_of_closure
