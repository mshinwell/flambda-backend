(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  { name : Name.t;
    name_mode : Name_mode.t
  }

let [@ocamlformat "disable"] print ppf { name; name_mode; } =
  Format.fprintf ppf "@[<hov 1>)\
      @[<hov 1>(name@ %a)@]@ \
      @[<hov 1>(name_mode@ %a)@]\
      )@]"
    Name.print name
    Name_mode.print name_mode

let create name name_mode =
  (* See note about name modes in [Bound_var.create]. *)
  { name; name_mode }

let create_var v =
  { name = Name.var (Bound_var.var v); name_mode = Bound_var.name_mode v }

let create_symbol sym = { name = Name.symbol sym; name_mode = Name_mode.normal }

let name t = t.name

let name_mode t = t.name_mode

let is_symbol t = Name.is_symbol t.name
