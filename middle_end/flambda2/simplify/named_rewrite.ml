(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* Rewriting of primitives. Currently this is only used by the mutable unboxing
   (aka ref-to-var) analysis, and these rewrite are applied when going upwards
   and rebuilding expressions. *)
module Prim_rewrite = struct
  type t =
    | Remove_prim
    | Invalid of Flambda_kind.t
    | Replace_by_binding of
        { var : Variable.t;
          bound_to : Simple.t
        }

  let print ppf = function
    | Invalid _ -> Format.fprintf ppf "Invalid"
    | Remove_prim -> Format.fprintf ppf "Remove_prim"
    | Replace_by_binding { var; bound_to } ->
      Format.fprintf ppf "Replace_by_binding { %a = %a }" Variable.print var
        Simple.print bound_to

  let invalid k = Invalid k

  let remove_prim = Remove_prim

  let replace_by_binding ~var ~bound_to = Replace_by_binding { var; bound_to }
end

(* We currently only rewrite primitives *)
type t = Prim_rewrite of Prim_rewrite.t

let print ppf = function
  | Prim_rewrite prim_rewrite -> Prim_rewrite.print ppf prim_rewrite

let prim_rewrite prim_rewrite = Prim_rewrite prim_rewrite
