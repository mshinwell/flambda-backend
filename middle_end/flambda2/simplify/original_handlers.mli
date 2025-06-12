(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = private
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Lifted_cont_params.t;
        continuation_handlers : One_recursive_handler.t Continuation.Lmap.t
      }
  | Non_recursive of Non_recursive_handler.t

val create_recursive :
  invariant_params:Bound_parameters.t ->
  lifted_params:Lifted_cont_params.t ->
  continuation_handlers:One_recursive_handler.t Continuation.Lmap.t ->
  t

val create_non_recursive : Non_recursive_handler.t -> t

val print : Format.formatter -> t -> unit

val bound_continuations : t -> Continuation.t list

val add_params_to_lift : t -> Lifted_cont_params.t -> t
