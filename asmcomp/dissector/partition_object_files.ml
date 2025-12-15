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

(* Default partition size: 1 GiB *)
let default_partition_size = Int64.shift_left 1L 30

let bytes_of_gb gb = Int64.of_float (gb *. 1024. *. 1024. *. 1024.)

type partition = Measure_object_files.file_size list

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
