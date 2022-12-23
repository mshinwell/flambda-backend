(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Pierrick Couderc, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of the names of compilation units, including associated "-for-pack"
   prefixes.

   By "compilation unit" we mean the code and data associated with the
   compilation of a single .ml source file: that is to say, file-level entities
   having OCaml semantics. The notion neither includes the special "startup"
   files nor external libraries. *)

[@@@ocaml.warning "+a-9-40-41-42"]

module Name : sig
  (** The name of a compilation unit without any "-for-pack" prefix. *)
  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  (** [dummy] is a placeholder for units that does not have a valid name, as
      in the, or during initialisation of the compiler.  It is not a valid
      identifier and thus cannot be generated through [of_string]. *)
  val dummy : t

  (** [of_string s] checks the given module name is a valid compilation unit
      name and generates its representation. *)
  val of_string : string -> t

  val to_string : t -> string

  (** The name of the distinguished compilation unit for predefined exceptions. *)
  val predef_exn : t
end

module Prefix : sig
  (** A pack name prefix, as specified to "-for-pack".  Such a prefix may
      be empty. *)
  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  val empty : t

  (** [parse_for_pack p] returns the list of nested packed modules, as expressed
      in the syntax of the "-for-pack" argument. *)
  val parse_for_pack : string -> t

  (** Return the prefix specified to "-for-pack". Returns the empty prefix if
      no "-for-pack" was passed. *)
  val from_clflags : unit -> t

  (** Return the list of names comprising the prefix, outermost first. *)
  val to_list : t -> Name.t list

  val to_string : t -> string

  val is_empty : t -> bool
end

(** The name of a compilation unit qualified with any "-for-pack" prefix that
    was specified when the unit was compiled.  For example if compiling foo.ml
    with "-for-pack Baz.Bar", the corresponding value of type [t] would
    represent "Baz.Bar.Foo", with its [name] representing "Foo" and its
    [prefix] representing "Baz.Bar". *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Print only the name of the given compilation unit. *)
val print_name : Format.formatter -> t -> unit

val print_debug : Format.formatter -> t -> unit

(** Create a compilation unit with the given [name] (which is not encoded or
    mangled in any way). *)
val create : Prefix.t -> Name.t -> t

(** Create a compilation unit contained by another. Effectively uses the
    parent compilation unit as the prefix. *)
val create_child : t -> Name.t -> t

(** Create a compilation unit from the given [name]. No prefix is allowed;
    throws a fatal error if there is a "." in the name. (As a special case,
    a "." is allowed as the first character, to handle compilation units
    which take their names from hidden files.) *)
val of_string : string -> t

(** Create a global [Ident.t] representing this compilation unit. Only intended
    for use in bytecode; most uses of [Ident.t]s that are known to be global
    should simply use [t] instead. *)
val to_global_ident_for_bytecode : t -> Ident.t

(** Find whether one compilation unit has another as a child. That is, whether
    the other unit has this one as its path prefix. *)
val is_parent : t -> child:t -> bool

(** Find whether one compilation unit can access another directly, without going
    through a pack. Equivalently, find whether one unit's .cmx file is visible
    while compiling another. Access to a packed unit is allowed only "from
    inside the pack," which is to say, a unit can only access its own members
    and those of its ancestors, though not the ancestors themselves. Thought of
    as a node in a tree, this means a module can access its own children, its
    own siblings, and its ancestors' siblings.

    In terms of paths, in order for [X] to access [Y],

    (1) [Y]'s prefix must be equal to or a prefix of [X]'s full path, and
    (2) [Y] itself must not be (strictly) a prefix of [X] (though [X] and [Y] may
        be equal).

    For example:

    * [A.B.C] _can_ access [A.Q] because [A.Q] is a member of [A] and [A] is
      an ancestor of [A.B.C]. In other words, [A.Q]'s prefix is [A] and [A] is a
      prefix of [A.B.C].
    * [A.Q] _cannot_ access [A.B.C] because [A.B] is not a prefix of [A.Q].
    * [A.Q] _can_ however access [A.B], because [A] _is_ a prefix of [A.Q].
    * [A.Q] _can_ also access its own member, [A.Q.R], because [A.Q.R]'s prefix
      is exactly [A.Q].
    * [A.Q] _cannot_ access [A.Q.R.S], because [A.Q.R] is not a prefix of [A.Q].
    * [A.Q] _can_ access [F], since [F]'s prefix is the empty path, which is
      trivially a prefix of [A.Q].
    * [A.Q] _cannot_ access [F.G] (by criterion 1) or [A] (by criterion 2). *)
val can_access_by_name : t -> accessed_by:t -> bool

(** Determine which .cmx file to load for a given compilation unit.
    This is tricky in the case of packs.  It can be done by lining up the
    desired compilation unit's full path (i.e. pack prefix then unit name)
    against the accessing unit's full path and observing when/if they
    diverge.

    This is only used for native code compilation. *)
val which_cmx_file : t -> accessed_by:t -> Name.t

(** A distinguished compilation unit for initialisation of mutable state. *)
val dummy : t

(** A distinguished compilation unit for predefined exceptions. *)
val predef_exn : t

(** The name of the compilation unit, excluding any [for_pack_prefix]. *)
val name : t -> Name.t

(** The name of the compilation unit, excluding any [for_pack_prefix], as
    as a string. *)

(* CR mshinwell: Try to delete this as soon as the functor packs work is
   finished. *)
val name_as_string : t -> string

(** The "-for-pack" prefix associated with the given compilation unit. *)
val for_pack_prefix : t -> Prefix.t

(** Replace the "-for-pack" prefix for the given compilation unit. *)
val with_for_pack_prefix : t -> Prefix.t -> t

(** Returns [true] iff the given compilation unit has a non-empty
    [for_pack_prefix]. *)
val is_packed : t -> bool

(** Returns the full path of the compilation unit.  The basename of the unit
    will be the last component of the returned list. *)
val full_path : t -> Name.t list

(** Returns the full path of the compilation unit, as a string, following
    usual conventions. *)
val full_path_as_string : t -> string

type error = private
  | Invalid_character of char * string
  | Bad_compilation_unit_name of string

(** The exception raised by conversion functions in this module. *)
exception Error of error

val set_current : t option -> unit

val get_current : unit -> t option

val get_current_or_dummy : unit -> t

val get_current_exn : unit -> t

val is_current : t -> bool
