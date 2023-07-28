(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module ARV = Available_ranges_vars
module L = Linearize
module V = Backend_var

module Subrange_info = struct
  type t =
    | Non_phantom of
        { reg : Reg.t;
          offset_from_cfa_in_bytes : int option
        }
end

module Range_info = struct
  type t = Var of ARV.Range_info.t

  let create_var info = Var info

  let provenance t =
    match t with Var range_info -> ARV.Range_info.provenance range_info

  (* XXX let debuginfo t = match provenance t with | None -> Debuginfo.none |
     Some provenance -> Backend_var.Provenance.debuginfo provenance *)

  let is_parameter t =
    match t with Var range_info -> ARV.Range_info.is_parameter range_info

  type phantom_defining_expr = Non_phantom

  let phantom_defining_expr (t : t) : phantom_defining_expr =
    match t with Var _ -> Non_phantom
end

module Subrange = struct
  type t = Var of ARV.Subrange.t

  let create_var subrange = Var subrange

  let info t : Subrange_info.t =
    match t with
    | Var subrange ->
      let subrange_info = ARV.Subrange.info subrange in
      Non_phantom
        { reg = ARV.Subrange_info.reg subrange_info;
          offset_from_cfa_in_bytes =
            ARV.Subrange_info.offset_from_cfa_in_bytes subrange_info
        }

  let start_pos t =
    match t with Var subrange -> ARV.Subrange.start_pos subrange

  let start_pos_offset t =
    match t with Var subrange -> ARV.Subrange.start_pos_offset subrange

  let end_pos t = match t with Var subrange -> ARV.Subrange.end_pos subrange

  let end_pos_offset t =
    match t with Var subrange -> ARV.Subrange.end_pos_offset subrange
end

module Range = struct
  type t = Var of ARV.Range.t

  let create_var range = Var range

  let info t =
    match t with Var range -> Range_info.create_var (ARV.Range.info range)

  (* XXX let extremities t = match t with Var range -> Some
     (ARV.Range.extremities range) *)

  let fold t ~init ~f =
    match t with
    | Var range ->
      ARV.Range.fold range ~init ~f:(fun acc subrange ->
          f acc (Subrange.create_var subrange))
end

type t = { available_ranges_vars : Available_ranges_vars.t }

let empty = { available_ranges_vars = Available_ranges_vars.empty }

let create ~available_ranges_vars =
  (* XXX let available_ranges_vars = ARV.all_indexes available_ranges_vars in *)
  { available_ranges_vars }

let iter t ~f =
  ARV.iter t.available_ranges_vars ~f:(fun index range ->
      let range = Range.create_var range in
      f index range)

let fold t ~init ~f =
  ARV.fold t.available_ranges_vars ~init ~f:(fun acc index range ->
      let range = Range.create_var range in
      f acc index range)
