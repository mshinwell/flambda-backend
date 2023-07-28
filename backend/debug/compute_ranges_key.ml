(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: add mli *)

module Available_regs = struct
  type t = Reg_with_debug_info.t

  type key = t

  module Raw_set = Reg_with_debug_info.Set

  module Set = struct
    include Reg_availability_set

    let print ppf t = print ~print_reg:Printmach.reg ppf t
  end

  module Map = Map.Make (struct
    type t = Reg_with_debug_info.t

    let compare = Reg_with_debug_info.compare
  end)

  let print ppf t = Reg_with_debug_info.print ~print_reg:Printmach.reg ppf t

  (* XXX - maybe not needed until lexical ranges are in? *)
  let all_parents _ = []
end
