(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Constants that fit in registers on the target machine. *)

include module type of struct
  include Int_ids.Const
end

val of_descr : Descr.t -> t

val is_naked_immediate : t -> Targetint_31_63.t option

val is_tagged_immediate : t -> Targetint_31_63.t option

(** Create a numeric constant of the given kind ([Region] and [Rec_info] are
    forbidden). *)
val of_int_of_kind : Flambda_kind.t -> int -> t
