(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare
open Asm_targets
module Uint8 = Numbers.Uint8
module A = Asm_directives

module Lower_address = struct
  type t =
    | Label of Asm_label.t
    | Symbol of Asm_symbol.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Label label1, Label label2 -> Asm_label.compare label1 label2
      | Symbol symbol1, Symbol symbol2 -> Asm_symbol.compare symbol1 symbol2
      | Label _, Symbol _ -> -1
      | Symbol _, Label _ -> 1

    let equal t1 t2 = compare t1 t2 = 0

    let hash t =
      match t with
      | Label label -> Asm_label.hash label
      | Symbol symbol -> Asm_symbol.hash symbol

    let print ppf t =
      match t with
      | Label label -> Asm_label.print ppf label
      | Symbol symbol -> Asm_symbol.print ppf symbol

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Upper_address = struct
  type t =
    { addr : Asm_label.t;
      offset : Targetint.t
    }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { addr = addr1; offset = offset1 }
        { addr = addr2; offset = offset2 } =
      let c = Asm_label.compare addr1 addr2 in
      if c <> 0 then c else Stdlib.compare offset1 offset2

    let equal t1 t2 = compare t1 t2 = 0

    let hash { addr; offset } = Hashtbl.hash (Asm_label.hash addr, offset)

    let print ppf { addr; offset } =
      if Targetint.equal offset Targetint.zero
      then Asm_label.print ppf addr
      else
        Format.fprintf ppf "%a + %a" Asm_label.print addr Targetint.print offset

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Entry = struct
  type t =
    { lower : Lower_address.t;
      upper : Upper_address.t
    }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { lower = lower1; upper = upper1 }
        { lower = lower2; upper = upper2 } =
      let c = Lower_address.compare lower1 lower2 in
      if c <> 0 then c else Upper_address.compare upper1 upper2

    let equal t1 t2 = compare t1 t2 = 0

    let hash { lower; upper } =
      Hashtbl.hash (Lower_address.hash lower, Upper_address.hash upper)

    let print ppf { lower; upper } =
      Format.fprintf ppf "@[<hov 1>((lower@ %a)@ (upper@ %a))@]"
        Lower_address.print lower Upper_address.print upper

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)

  let code_address ?offset ~start_of_code_symbol label =
    { lower = Symbol start_of_code_symbol;
      upper =
        { addr = label; offset = Option.value offset ~default:Targetint.zero }
    }

  let distance_between_labels ?offset_upper ~lower ~upper () =
    { lower = Label lower;
      upper =
        { addr = upper;
          offset = Option.value offset_upper ~default:Targetint.zero
        }
    }

  let to_dwarf_value { lower; upper = { addr = upper; offset = offset_upper } }
      =
    match lower with
    | Label lower ->
      Dwarf_value.address_table_entry_from_label_label_diff
        ~comment:"ending address" ~lower ~upper ~offset_upper ()
    | Symbol lower ->
      Dwarf_value.address_table_entry_from_label_symbol_diff
        ~comment:"ending address" ~lower ~upper ~offset_upper ()
end

type t =
  { base_addr : Asm_label.t;
    (* [base_addr] is the start address of the table (see below), not anything
       to do with the addresses within it. *)
    mutable next_index : Address_index.t;
    mutable table : Entry.t Address_index.Map.t;
    mutable rev_table : Address_index.t Entry.Map.t
  }

let create () =
  { base_addr = Asm_label.create (DWARF Debug_addr);
    next_index = Address_index.zero;
    table = Address_index.Map.empty;
    rev_table = Entry.Map.empty
  }

let add t entry =
  match Entry.Map.find entry t.rev_table with
  | exception Not_found ->
    let index = t.next_index in
    t.next_index <- Address_index.succ index;
    t.rev_table <- Entry.Map.add entry index t.rev_table;
    t.table <- Address_index.Map.add index entry t.table;
    index
  | index -> index

let base_addr t = t.base_addr

let initial_length t =
  let num_entries = Int64.of_int (Address_index.Map.cardinal t.table) in
  let size_entries =
    Int64.mul num_entries (Int64.of_int Dwarf_arch_sizes.size_addr)
  in
  Initial_length.create (Dwarf_int.of_int64_exn (Int64.add 4L size_entries))

let size t =
  let initial_length = initial_length t in
  Dwarf_int.add
    (Initial_length.size initial_length)
    (Initial_length.to_dwarf_int initial_length)

let emit ~asm_directives t =
  Initial_length.emit ~asm_directives (initial_length t);
  Dwarf_version.emit ~asm_directives Dwarf_version.five;
  A.uint8 (Uint8.of_nonnegative_int_exn Dwarf_arch_sizes.size_addr);
  A.uint8 Uint8.zero;
  A.define_label t.base_addr;
  Address_index.Map.iter
    (fun _index entry ->
      Dwarf_value.emit ~asm_directives (Entry.to_dwarf_value entry))
    t.table
