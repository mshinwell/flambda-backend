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

  include Contains_ids.S with type t := t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val apply_coercion : t -> Coercion.t -> t Or_bottom.t
end) =
struct
  module Descr = No_alias_or_equals.Make [@inlined hint] (Head)
  include With_delayed_permutation.Make [@inlined hint] (Descr)

  let all_ids_for_export t =
    match descr t with
    | No_alias Bottom | No_alias Unknown -> Ids_for_export.empty
    | No_alias (Ok head) -> Head.all_ids_for_export head
    | Equals simple -> Ids_for_export.from_simple simple

  let print ppf t = Descr.print ppf (descr t)

  let create_no_alias head = create (No_alias head)

  let create_equals simple = create (Equals simple)

  let bottom = lazy (create (No_alias Bottom))

  let unknown = lazy (create (No_alias Unknown))

  let bottom () = Lazy.force bottom

  let unknown () = Lazy.force unknown

  let create head = create_no_alias (Ok head)

  let is_obviously_bottom t =
    match peek_descr t with
    | No_alias Bottom -> true
    | No_alias (Ok _ | Unknown) | Equals _ -> false

  let is_obviously_unknown t =
    match peek_descr t with
    | No_alias Unknown -> true
    | No_alias (Ok _ | Bottom) | Equals _ -> false

  let get_alias_exn t =
    match peek_descr t with
    | No_alias _ -> raise Not_found
    | Equals _ -> (
      match descr t with Equals alias -> alias | No_alias _ -> assert false)

  let apply_coercion t coercion : _ Or_bottom.t =
    match descr t with
    | Equals simple -> begin
      match Simple.apply_coercion simple coercion with
      | None -> Bottom
      | Some simple -> Ok (create_equals simple)
    end
    | No_alias Unknown -> Ok t
    | No_alias Bottom -> Bottom
    | No_alias (Ok head) ->
      Or_bottom.map (Head.apply_coercion head coercion) ~f:(fun head ->
          create head)
end
