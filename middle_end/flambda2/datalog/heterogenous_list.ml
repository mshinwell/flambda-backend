(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type nil = Nil

module type S = sig
  type 'a t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist
end

module Make (X : sig
  type 'a t
end) : S with type 'a t := 'a X.t = struct
  type 'a t = 'a X.t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist
end

module Constant = Make (struct
  type 'a t = 'a
end)
