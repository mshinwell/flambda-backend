(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CU = Compilation_unit

(* CR mshinwell: maybe there should be a phantom type allowing to distinguish
   the .cmx case from the others. Unclear it's worth it. *)
type t

val create : CU.Name.t -> crc_with_unit:(CU.t * string) option -> t

val create_normal : CU.t -> crc:string option -> t

val name : t -> CU.Name.t

(** This function will cause a fatal error if a [CU.t] was not provided when the
    supplied value of type [t] was created. *)
val cu : t -> CU.t

val crc : t -> string option

val crc_with_unit : t -> (CU.t * string) option

val has_name : t -> name:CU.Name.t -> bool

val dummy : t
