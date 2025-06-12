(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type reason =
  | At_toplevel
  | Contains_static_consts
  | Contains_set_of_closures

type cost = { size_of_primitives : int }

type t =
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }

let [@ocamlformat "disable"] print_reason ppf reason =
  match reason with
  | At_toplevel ->
    Format.fprintf ppf "at_top_level"
  | Contains_static_consts ->
    Format.fprintf ppf "contains_static_consts"
  | Contains_set_of_closures ->
    Format.fprintf ppf "contains_set_of_closures"

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Can_specialize { size_of_primitives; } ->
    Format.fprintf ppf "@[<hov>(can_specialize@ \
        @[<hov 1>(size_of_primitives@ %d)@]\
      )@]"
      size_of_primitives
  | Cannot_specialize { reason } ->
    Format.fprintf ppf "@[<hov>(cannot_specialize@ \
        @[<hov 1>(reason@ %a)@]\
      )@]"
    print_reason reason

(* Creations *)

let can_specialize = Can_specialize { size_of_primitives = 0 }

let cannot_specialize reason = Cannot_specialize { reason }

(* Updating costs *)

let update_cost ~f = function
  | Can_specialize cost -> Can_specialize (f cost)
  | Cannot_specialize _ as res -> res

let add_prim prim t =
  let size = Code_size.to_int (Code_size.prim prim) in
  update_cost t ~f:(fun { size_of_primitives = s } ->
      { size_of_primitives = size + s })

let add_set_of_closures _soc _t =
  Cannot_specialize { reason = Contains_set_of_closures }
