(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(* Effects, coeffects and placements *)

(** A triple of an effect, a coeffect, and a placement. *)
type t = Effects.t * Coeffects.t * Placement.t

(** Print *)
val print : Format.formatter -> t -> unit

(** Comparison. *)
val compare : t -> t -> int

(** The value stating that no effects or coeffects take place, with a strict
    placement. This is exactly [No_effects, No_coeffects, Strict]. *)
val pure : t

(** The value stating that no effects of coeffects take place, and that the
    expression can be moved and duplicated if needed. This is exactly
    [No_effects, No_coeffects, Delay]. *)
val pure_can_be_duplicated : t

(** The value stating that any effects and/or coeffects may take place (with
    strict placement). This is exactly [Arbitrary_effects, Has_coeffects,
    Strict]. *)
val all : t

(** The value stating that a read (i.e only a coeffect) takes place (with strict
    placement). This is [No_effects, Has_coeffects, Strict]. *)
val read : t

(** Join two effects, coeffects and placements. *)
val join : t -> t -> t
