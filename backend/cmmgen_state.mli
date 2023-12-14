(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable state used by [Cmmgen]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type constant =
  | Const_table of Cmm.is_global * Cmm.data_item list

val add_constant : Misc.Stdlib.String.t -> constant -> unit

val add_data_items : Cmm.data_item list -> unit

val get_and_clear_constants : unit -> constant Misc.Stdlib.String.Map.t

val get_and_clear_data_items : unit -> Cmm.data_item list

val add_structured_constant : Cmm.symbol -> Clambda.ustructured_constant -> unit

val clear_local_structured_constants : unit -> unit

val add_global_structured_constant : string -> Clambda.ustructured_constant -> unit

val get_structured_constant : string -> (Cmm.is_global * Clambda.ustructured_constant) option

val structured_constant_of_sym : string -> Clambda.ustructured_constant option
