(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t =
  | Always_inline
  | Available_inline
  | Never_inline
  | Unroll of int
  | Default_inline

let [@ocamlformat "disable"] print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_inline -> fprintf ppf "Always_inline"
  | Available_inline -> fprintf ppf "Available_inline"
  | Never_inline -> fprintf ppf "Never_inline"
  | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inline -> fprintf ppf "Default_inline"

let equal t1 t2 =
  match t1, t2 with
  | Always_inline, Always_inline
  | Available_inline, Available_inline
  | Never_inline, Never_inline
  | Default_inline, Default_inline ->
    true
  | Unroll n1, Unroll n2 -> n1 = n2
  | ( ( Always_inline | Available_inline | Never_inline | Unroll _
      | Default_inline ),
      _ ) ->
    false

let is_default t =
  match t with
  | Default_inline -> true
  | Always_inline | Available_inline | Never_inline | Unroll _ -> false

let number_of_unrolls = function
  | Unroll n -> n
  | Always_inline | Available_inline | Never_inline | Default_inline -> 0

let from_lambda (attr : Lambda.inline_attribute) =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Available_inline -> Available_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline
