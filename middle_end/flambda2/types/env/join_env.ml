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

module Make (Typing_env : sig
  type t

  val print : Format.formatter -> t -> unit
end) =
struct
  module Meet_env = Meet_env0.Make (Typing_env)

  type t =
    { central_env : Meet_env.t;
      left_join_env : Typing_env.t;
      right_join_env : Typing_env.t
    }

  let [@ocamlformat "disable"] print ppf
      { central_env; left_join_env; right_join_env } =
    let join_env name ppf env =
      Format.fprintf ppf "@ @[<hov 1>(%s@ %a)@]@" name Typing_env.print env
    in
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(central_env@ %a)@]\
        %a%a)@]"
      Meet_env.print central_env
      (join_env "left_join_env") left_join_env
      (join_env "right_join_env") right_join_env

  let create central_env ~left_env ~right_env =
    { central_env = Meet_env.create central_env;
      left_join_env = left_env;
      right_join_env = right_env
    }

  let target_join_env t = Meet_env.env t.central_env

  let left_join_env t = t.left_join_env

  let right_join_env t = t.right_join_env

  (* CR mshinwell: fix naming, it's odd at the moment to be using
     [already_meeting]... *)
  let now_joining t simple1 simple2 =
    { t with central_env = Meet_env.now_meeting t.central_env simple1 simple2 }

  let already_joining { central_env; _ } simple1 simple2 =
    Meet_env.already_meeting central_env simple1 simple2
end
