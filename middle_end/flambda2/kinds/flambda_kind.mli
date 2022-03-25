(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Kinds and subkinds of Flambda types. *)

module Naked_number_kind : sig
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val print : Format.formatter -> t -> unit
end

(** The kinds themselves. *)
type t = private
  | Value  (** OCaml values, either immediates or pointers. *)
  | Naked_number of Naked_number_kind.t
      (** The kind of unboxed numbers and untagged immediates. *)
  | Region
      (** Values which have been introduced by Flambda and are never accessible
          at the source language level (for example sets of closures). *)
  | Rec_info
      (** Recursion depths of identifiers. Like [Region], not accessible at the
          source level, but also not accessible at run time. *)

type kind = t

(** Constructors for the various kinds. *)
val value : t

val naked_immediate : t

val naked_float : t

val naked_int32 : t

val naked_int64 : t

val naked_nativeint : t

val region : t

val rec_info : t

val is_value : t -> bool

val is_naked_float : t -> bool

include Container_types.S with type t := t

module Standard_int : sig
  (** "Standard" because these correspond to the usual representations of tagged
      immediates, 32-bit, 64-bit and native integers as expected by the
      operations in [Flambda_primitive]. *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end

module Standard_int_or_float : sig
  (** The same as [Standard_int], but also permitting naked floats. *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end

module Boxable_number : sig
  (** These kinds are those of the numbers for which a tailored boxed
      representation exists. *)

  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val unboxed_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  val print_lowercase_short : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end

module With_subkind : sig
  module Subkind : sig
    type t =
      | Anything
      | Boxed_float
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Tagged_immediate
      | Block of
          { tag : Tag.t;
            fields : t list
          }
      | Float_block of { num_fields : int }
      | Float_array
      | Immediate_array
      | Value_array
      | Generic_array

    include Container_types.S with type t := t
  end

  type kind = t

  type t

  val create : kind -> Subkind.t -> t

  val kind : t -> kind

  val subkind : t -> Subkind.t

  val has_useful_subkind_info : t -> bool

  val any_value : t

  val naked_immediate : t

  val naked_float : t

  val naked_int32 : t

  val naked_int64 : t

  val naked_nativeint : t

  val boxed_float : t

  val boxed_int32 : t

  val boxed_int64 : t

  val boxed_nativeint : t

  val tagged_immediate : t

  val rec_info : t

  val float_array : t

  val immediate_array : t

  val value_array : t

  val generic_array : t

  val block : Tag.t -> t list -> t

  val float_block : num_fields:int -> t

  val of_naked_number_kind : Naked_number_kind.t -> t

  val from_lambda : Lambda.value_kind -> t

  type descr = private
    | Any_value
    | Naked_number of Naked_number_kind.t
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint
    | Tagged_immediate
    | Rec_info
    | Block of
        { tag : Tag.t;
          fields : descr list
        }
    | Float_block of { num_fields : int }
    | Float_array
    | Immediate_array
    | Value_array
    | Generic_array

  val descr : t -> descr

  val compatible : t -> when_used_at:t -> bool

  include Container_types.S with type t := t
end
