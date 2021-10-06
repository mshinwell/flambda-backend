(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Flambda.Function_params_and_body.t Code0.t

let code_id = Code0.code_id

let params_and_body = Code0.params_and_body

let params_and_body_opt = Code0.params_and_body_opt

let params_and_body_must_be_present = Code0.params_and_body_must_be_present

let newer_version_of = Code0.newer_version_of

let params_arity = Code0.params_arity

let result_arity = Code0.result_arity

let stub = Code0.stub

let inline = Code0.inline

let is_a_functor = Code0.is_a_functor

let recursive = Code0.recursive

let cost_metrics = Code0.cost_metrics

let inlining_arguments = Code0.inlining_arguments

let dbg = Code0.dbg

let is_tupled = Code0.is_tupled

let inlining_decision = Code0.inlining_decision

let create = Code0.create

let with_code_id = Code0.with_code_id

let with_params_and_body = Code0.with_params_and_body

let with_newer_version_of = Code0.with_newer_version_of

let make_deleted = Code0.make_deleted

let is_deleted = Code0.is_deleted

let free_names = Code0.free_names Flambda.Function_params_and_body.free_names

let apply_renaming =
  Code0.apply_renaming Flambda.Function_params_and_body.free_names

let print = Code0.print Flambda.Function_params_and_body.print

let all_ids_for_export = Code0.all_ids_for_export

let compare = Code0.compare
