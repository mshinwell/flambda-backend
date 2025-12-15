(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type error =
  | Measure_error of Measure_object_files.error
  | Partition_error of Partition_object_files.error

exception Error of error

let report_error ppf = function
  | Measure_error err -> Measure_object_files.report_error ppf err
  | Partition_error err -> Partition_object_files.report_error ppf err

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

type result =
  { ml_objfiles : string list;
    startup_obj : string;
    partitions : Partition.t list
  }

let dump_sizes file_sizes =
  Printf.eprintf "Dissector: allocated section sizes:\n";
  let total =
    List.fold_left
      (fun acc (entry : Measure_object_files.file_size) ->
        Printf.eprintf "  %12Ld  %s\n" entry.size entry.filename;
        Int64.add acc entry.size)
      0L file_sizes
  in
  Printf.eprintf "  %12Ld  TOTAL\n%!" total

let run ~(unix : (module Compiler_owee.Unix_intf.S)) ~ml_objfiles ~startup_obj
    ~ccobjs ~runtime_libs ~cached_genfns =
  (* Check that we're targeting Linux *)
  (match Target_system.system () with
  | Linux -> ()
  | Windows _ | MacOS_like | FreeBSD | NetBSD | OpenBSD | Generic_BSD | Solaris
  | Dragonfly | GNU | BeOS | Unknown ->
    Misc.fatal_error "The dissector pass is only supported on Linux targets");
  (* Collect all files to analyze *)
  let files =
    ml_objfiles @ [startup_obj] @ ccobjs @ runtime_libs
    @ match cached_genfns with None -> [] | Some f -> [f]
  in
  (* Measure file sizes *)
  let file_sizes =
    try Measure_object_files.measure_files unix ~files
    with Measure_object_files.Error err -> raise (Error (Measure_error err))
  in
  (* Dump sizes if requested *)
  if !Clflags.ddissector_sizes then dump_sizes file_sizes;
  (* Compute partition threshold *)
  let threshold =
    match !Clflags.dissector_partition_size with
    | Some gb -> Partition_object_files.bytes_of_gb gb
    | None -> Partition_object_files.default_partition_size
  in
  (* Partition files *)
  let partitions =
    try Partition_object_files.partition_files ~threshold file_sizes
    with Partition_object_files.Error err ->
      raise (Error (Partition_error err))
  in
  (* Print partition summary *)
  let total =
    List.fold_left
      (fun acc (entry : Measure_object_files.file_size) ->
        Int64.add acc entry.size)
      0L file_sizes
  in
  Printf.eprintf "Dissector: total allocated section size = %Ld bytes\n%!" total;
  Printf.eprintf "Dissector: partitioned into %d partition(s)\n%!"
    (List.length partitions);
  (* Return result *)
  { ml_objfiles; startup_obj; partitions }
