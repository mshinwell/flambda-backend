(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type set

type +!'a map

module Make (_ : sig
  val print : Format.formatter -> int -> unit
end) : sig
  module Set : sig
    include Container_types.Set with type elt = int and type t = set

    (** For testing; should always return [true] *)
    val valid : t -> bool
  end

  module Map : sig
    include
      Container_types.Map_plus_iterator
        with type key = int
         and type 'a t = 'a map
        with module Set = Set

    (** For testing; should always return [true] *)
    val valid : _ t -> bool
  end
end
