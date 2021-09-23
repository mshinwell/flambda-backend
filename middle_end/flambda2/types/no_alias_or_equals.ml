(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Make (Head : sig
  type t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit
end) =
struct
  type t =
    | No_alias of Head.t Or_unknown_or_bottom.t
    | Equals of Simple.t

  let print ppf t =
    let colour = Flambda_colours.top_or_bottom_type () in
    match t with
    | No_alias Unknown ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s" colour
          (Flambda_colours.normal ())
      else Format.fprintf ppf "@<0>%sT@<0>%s" colour (Flambda_colours.normal ())
    | No_alias Bottom ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s" colour
          (Flambda_colours.normal ())
      else
        Format.fprintf ppf "@<0>%s_|_@<0>%s" colour (Flambda_colours.normal ())
    | No_alias (Ok head) -> Head.print ppf head
    | Equals simple ->
      Format.fprintf ppf "@[(@<0>%s=@<0>%s %a)@]" (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Simple.print simple

  let apply_renaming t renaming =
    if Renaming.is_empty renaming
    then t
    else
      match t with
      | No_alias Bottom | No_alias Unknown -> t
      | No_alias (Ok head) ->
        let head' = Head.apply_renaming head renaming in
        if head == head' then t else No_alias (Ok head')
      | Equals simple ->
        let simple' = Simple.apply_renaming simple renaming in
        if simple == simple' then t else Equals simple'

  let free_names t =
    match t with
    | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
    | No_alias (Ok head) -> Head.free_names head
    | Equals simple ->
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Simple.free_names simple) Name_mode.in_types
end
