(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type string_contents =
  | Contents of string
  | Unknown_or_mutable

type t =
  { contents : string_contents;
    size : Targetint_31_63.t
  }

let create ~contents ~size = { contents; size }

let contents t = t.contents

let size t = t.size

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    let c =
      match t1.contents, t2.contents with
      | Contents s1, Contents s2 -> String.compare s1 s2
      | Unknown_or_mutable, Unknown_or_mutable -> 0
      | Contents _, Unknown_or_mutable -> -1
      | Unknown_or_mutable, Contents _ -> 1
    in
    if c <> 0 then c else Stdlib.compare t1.size t2.size

  let equal t1 t2 = compare t1 t2 = 0

  let hash t = Hashtbl.hash t

  let [@ocamlformat "disable"] print ppf { contents; size; } =
    match contents with
    | Unknown_or_mutable ->
      Format.fprintf ppf "(size %a)"
        Targetint_31_63.print size
    | Contents s ->
      let s, dots =
        let max_size = Targetint_31_63.ten in
        let long = Targetint_31_63.compare size max_size > 0 in
        if long then String.sub s 0 8, "..."
        else s, ""
      in
      Format.fprintf ppf "(size %a) (contents \"%S\"%s)"
        Targetint_31_63.print size
        s dots
end)
