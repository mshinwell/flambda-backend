(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Component = struct
  type t =
    | Singleton of Flambda_kind.With_subkind.t
    (* The nullary unboxed product is called "void". *)
    | Unboxed_product of t list

  let rec equal_ignoring_subkinds t1 t2 =
    match t1, t2 with
    | Singleton kind1, Singleton kind2 ->
      Flambda_kind.With_subkind.equal kind1 kind2
    | Unboxed_product ts1, Unboxed_product ts2 ->
      List.equal equal_ignoring_subkinds ts1 ts2
    | Singleton _, Unboxed_product _ | Unboxed_product _, Singleton _ -> false

  let rec print ~product_above ppf t =
    match t with
    | Singleton kind -> Flambda_kind.With_subkind.print ppf kind
    | Unboxed_product [] -> Format.pp_print_string ppf "void"
    | Unboxed_product ts ->
      Format.fprintf ppf "@[<hov 1>%s%a%s@]"
        (if product_above then "(" else "")
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
           (print ~product_above:true))
        ts
        (if product_above then ")" else "")

  let rec unarize t =
    match t with
    | Singleton kind -> [kind]
    | Unboxed_product [] -> []
    | Unboxed_product ts -> List.concat_map unarize ts
end

(* One component per function or continuation parameter, for example. Each
   component may in turn have an arity describing an unboxed product. *)
type t = Component.t list

type for_creation = Component.t =
  | Singleton of Flambda_kind.With_subkind.t
  | Unboxed_product of Component.t list

let nullary = []

let create t = t

let create_singletons t = List.map (fun kind -> Component.Singleton kind) t

let print ppf t =
  Component.print ~product_above:false ppf (Component.Unboxed_product t)

let to_list_not_unarized t = t

let equal_ignoring_subkinds t1 t2 =
  List.equal Component.equal_ignoring_subkinds t1 t2

let is_singleton_value_not_unarized t =
  match t with
  | [Singleton kind]
    when Flambda_kind.equal
           (Flambda_kind.With_subkind.kind kind)
           Flambda_kind.value ->
    true
  | [] | Singleton _ :: _ | Unboxed_product _ :: _ -> false

let cardinal_not_unarized t = List.length t

let unarize t = List.concat_map Component.unarize t
