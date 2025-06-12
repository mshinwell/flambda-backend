(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a t = 'a -> 'a Seq.t

val unshrinkable : 'a t

val option : 'a t -> 'a option t

val function_ : ?const:'b -> 'b t -> ('a, 'b) Function.t t

val function_w_id : ?const:'a -> 'a t -> ('a, 'a) Function.t t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val list : 'a t -> 'a list t

module T : sig
  type nonrec 'a t = 'a t
end

val tuple : ('a, 'b) Tuple.Of(T).t -> ('a, 'b) Tuple.t t
