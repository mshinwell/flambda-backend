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
  | Partial_link_error of Partial_link.error

exception Error of error

let report_error ppf = function
  | Measure_error err -> Measure_object_files.report_error ppf err
  | Partition_error err -> Partition_object_files.report_error ppf err
  | Partial_link_error err -> Partial_link.report_error ppf err

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let log fmt =
  if !Clflags.ddissector
  then Printf.eprintf ("Dissector: " ^^ fmt ^^ "\n%!")
  else Printf.ifprintf stderr fmt

type result =
  { ml_objfiles : string list;
    startup_obj : string;
    partitions : Partition.t list;
    linked_partitions : Partition.linked list;
    relocations : Extract_relocations.t;
    linker_script : string
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

let run ~(unix : (module Compiler_owee.Unix_intf.S)) ~temp_dir ~ml_objfiles
    ~startup_obj ~ccobjs ~runtime_libs ~cached_genfns =
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
  let total =
    List.fold_left
      (fun acc (entry : Measure_object_files.file_size) ->
        Int64.add acc entry.size)
      0L file_sizes
  in
  log "total allocated section size = %Ld bytes" total;
  log "partitioned into %d partition(s)" (List.length partitions);
  let linked_partitions =
    try Partial_link.link_partitions ~temp_dir partitions
    with Partial_link.Error err -> raise (Error (Partial_link_error err))
  in
  log "partially linked %d partition(s)" (List.length linked_partitions);
  let relocations =
    Extract_relocations.extract_from_linked_partitions unix linked_partitions
  in
  log "found %d PLT relocations and %d GOT relocations"
    (List.length relocations.convert_to_plt)
    (List.length relocations.convert_to_got);
  List.iter
    (fun (partition : Partition.linked) ->
      let igot_and_iplt =
        Build_igot_and_iplt.build ~prefix:"__dissector_" relocations
      in
      log "built IGOT with %d entries, IPLT with %d entries"
        (List.length (Igot.entries igot_and_iplt.igot))
        (List.length (Iplt.entries igot_and_iplt.iplt));
      let output_file = partition.linked_object ^ ".rewritten" in
      Rewrite_sections.rewrite unix ~input_file:partition.linked_object
        ~output_file ~igot_and_iplt ~relocations;
      log "rewrote %s -> %s" partition.linked_object output_file)
    linked_partitions;
  (* TODO: Extract existing_script from linker command line flags
     (--script=<path>) *)
  let linker_script = Filename.concat temp_dir "linker.script" in
  Linker_script.write ~output_file:linker_script ~existing_script:None
    ~partitions:linked_partitions;
  log "generated linker script: %s" linker_script;
  { ml_objfiles;
    startup_obj;
    partitions;
    linked_partitions;
    relocations;
    linker_script
  }
