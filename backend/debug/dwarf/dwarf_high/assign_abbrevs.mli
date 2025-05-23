(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Dwarf_low

(** Construction of abbreviation tables from proto-DIEs together with flattening
    of the proto-DIE tree to a list of DIEs. *)

type t = private
  { abbrev_table : Abbreviations_table.t;
    dies : Debugging_information_entry.t list;
    dwarf_4_location_lists : Dwarf_4_location_list.t list
  }

val create : unit -> t

val run : t -> proto_die_root:Proto_die.t -> t
