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

module Typing_env = struct
  include Typing_env

  let add_equation t name ty =
    add_equation t name ty ~meet_type:Meet_and_join.meet

  let add_equations_on_params t ~params ~param_types =
    add_equations_on_params t ~params ~param_types ~meet_type:Meet_and_join.meet

  let add_env_extension t extension =
    add_env_extension t extension ~meet_type:Meet_and_join.meet

  let add_env_extension_with_extra_variables t extension =
    add_env_extension_with_extra_variables t extension
      ~meet_type:Meet_and_join.meet
end

module Typing_env_extension = Typing_env_extension

type typing_env = Typing_env.t

type typing_env_extension = Typing_env_extension.t

include Type_grammar
include More_type_creators
include Expand_head
include Meet_and_join
include Provers
include Reify
include Join_levels
