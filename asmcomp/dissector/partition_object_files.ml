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
  | File_exceeds_partition_size of
      { filename : string;
        size : int64;
        threshold : int64
      }

exception Error of error

let report_error ppf = function
  | File_exceeds_partition_size { filename; size; threshold } ->
    Format.fprintf ppf
      "Dissector: file %s has allocated section size %Ld bytes, which exceeds \
       partition threshold %Ld bytes"
      filename size threshold

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let bytes_of_gb gb = Int64.of_float (gb *. 1024. *. 1024. *. 1024.)

(* Default partition size in bytes, derived from Clflags *)
let default_partition_size = bytes_of_gb Clflags.dissector_partition_size_default

(* Check if a file must go into the Main partition based on its origin.
   C stubs, runtime, startup, and cached generic functions must all be in Main
   to avoid complications with cross-partition references and special sections
   like .gcc_except_table. *)
let must_be_in_main entry =
  match Measure_object_files.File_size.origin entry with
  | Measure_object_files.OCaml -> false
  | Measure_object_files.C_stub
  | Measure_object_files.Runtime
  | Measure_object_files.Startup
  | Measure_object_files.Cached_genfns -> true

let partition_files ~threshold file_sizes =
  (* Separate files that must go into Main partition:
     - Files with probes (to keep probes together)
     - C stubs, runtime, startup, cached genfns (by origin) *)
  let main_files, partitionable_files =
    List.partition
      (fun entry ->
        Measure_object_files.File_size.has_probes entry || must_be_in_main entry)
      file_sizes
  in
  (* Calculate the size of main_files - this will be added to the first
     partition, so we need to account for it when partitioning. *)
  let main_files_size =
    List.fold_left
      (fun acc entry -> Int64.add acc (Measure_object_files.File_size.size entry))
      0L main_files
  in
  (* Partition regular OCaml files into buckets, starting a new bucket when
     adding the next file would exceed the threshold. The first partition has
     reduced capacity since main_files will be added to it. The order of files
     is preserved. *)
  let rec loop current_partition current_size partitions = function
    | [] ->
      (* Finish: add current partition if non-empty *)
      let partitions =
        if current_partition = []
        then partitions
        else List.rev current_partition :: partitions
      in
      List.rev partitions
    | entry :: rest ->
      let entry_size = Measure_object_files.File_size.size entry in
      (* Check if this file exceeds the threshold by itself *)
      if entry_size > threshold
      then
        raise
          (Error
             (File_exceeds_partition_size
                { filename = Measure_object_files.File_size.filename entry;
                  size = entry_size;
                  threshold
                }));
      (* For the first partition, account for main_files size *)
      let effective_threshold =
        if partitions = []
        then Int64.sub threshold main_files_size
        else threshold
      in
      (* Check if adding this file would exceed the effective threshold *)
      let new_size = Int64.add current_size entry_size in
      if new_size > effective_threshold && current_partition <> []
      then
        (* Start a new partition *)
        let partitions = List.rev current_partition :: partitions in
        loop [entry] entry_size partitions rest
      else
        (* Add to current partition *)
        loop (entry :: current_partition) new_size partitions rest
  in
  let file_lists = loop [] 0L [] partitionable_files in
  (* Add main_files to the first (Main) partition *)
  let file_lists =
    match file_lists with
    | [] -> if main_files = [] then [] else [main_files]
    | first :: rest -> (main_files @ first) :: rest
  in
  List.mapi
    (fun i files ->
      let kind : Partition.kind = if i = 0 then Main else Large_code i in
      Partition.create ~kind files)
    file_lists
