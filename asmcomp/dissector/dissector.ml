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
  | File_exceeds_partition_size of
      { filename : string;
        size : int64;
        threshold : int64
      }

exception Error of error

let report_error ppf = function
  | Measure_error err -> Measure_object_files.report_error ppf err
  | File_exceeds_partition_size { filename; size; threshold } ->
    Format.fprintf ppf
      "Dissector: file %s has allocated section size %Ld bytes, which exceeds \
       partition threshold %Ld bytes"
      filename size threshold

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

(* Default partition size: 1.5 GB *)
let default_partition_size_gb = 1.5

let bytes_of_gb gb = Int64.of_float (gb *. 1024. *. 1024. *. 1024.)

type partition = Measure_object_files.file_size list

type result =
  { ml_objfiles : string list;
    startup_obj : string;
    partitions : partition list
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

let partition_files ~threshold file_sizes =
  (* Partition files into buckets, starting a new bucket when adding the next
     file would exceed the threshold. The order of files is preserved. *)
  let rec loop current_partition current_size partitions = function
    | [] ->
      (* Finish: add current partition if non-empty *)
      let partitions =
        if current_partition = []
        then partitions
        else List.rev current_partition :: partitions
      in
      List.rev partitions
    | (entry : Measure_object_files.file_size) :: rest ->
      (* Check if this file exceeds the threshold by itself *)
      if entry.size > threshold
      then
        raise
          (Error
             (File_exceeds_partition_size
                { filename = entry.filename; size = entry.size; threshold }));
      (* Check if adding this file would exceed the threshold *)
      let new_size = Int64.add current_size entry.size in
      if new_size > threshold && current_partition <> []
      then
        (* Start a new partition *)
        let partitions = List.rev current_partition :: partitions in
        loop [entry] entry.size partitions rest
      else
        (* Add to current partition *)
        loop (entry :: current_partition) new_size partitions rest
  in
  loop [] 0L [] file_sizes

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
  let partition_size_gb =
    match !Clflags.dissector_partition_size with
    | Some gb -> gb
    | None -> default_partition_size_gb
  in
  let threshold = bytes_of_gb partition_size_gb in
  (* Partition files *)
  let partitions = partition_files ~threshold file_sizes in
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
