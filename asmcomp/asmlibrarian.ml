(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Build libraries of .cmx files *)

open Misc
open Config
open Cmx_format
module String = Misc.Stdlib.String

type error =
    File_not_found of string
  | Archiver_error of string
  | Link_error of Linkdeps.error

exception Error of error

let read_info name =
  let filename =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Compilenv.read_unit_info filename in
  info.ui_force_link <- info.ui_force_link || !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmxa file,
     since the compiler will go looking directly for .cmx files.
     The linker, which is the only one that reads .cmxa files, does not
     need the approximation. *)
<<<<<<< HEAD
  info.ui_export_info <- None;
  (Filename.chop_suffix filename ".cmx" ^ ext_obj, (info, crc))
||||||| 23e84b8c4d
  info.ui_export_info <- default_ui_export_info;
  (Filename.chop_suffix filename ".cmx" ^ ext_obj, (info, crc))
=======
  info.ui_export_info <- default_ui_export_info;
  filename, (info, crc)
>>>>>>> ocaml/5.4

let create_archive file_list lib_name =
  let archive_name = Filename.remove_extension lib_name ^ ext_lib in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name; remove_file archive_name)
    (fun () ->
       output_string outchan cmxa_magic_number;
       let units = List.map read_info file_list in
       let objfiles = List.map (fun (filename,_) ->
           Filename.chop_suffix filename ".cmx" ^ ext_obj)
           units in
       List.iter
         (fun (file_name, (unit, crc)) ->
            Asmlink.check_consistency file_name unit crc)
<<<<<<< HEAD
         file_list descr_list;
       let cmis = Asmlink.extract_crc_interfaces () in
       let cmxs = Asmlink.extract_crc_implementations () in
       (* CR mshinwell: see comment in compilenv.ml
       let cmxs =
         Compilenv.ensure_sharing_between_cmi_and_cmx_imports cmis cmxs
       in
       *)
       let cmis = Array.of_list cmis in
       let cmxs = Array.of_list cmxs in
       let cmi_index = Compilation_unit.Name.Tbl.create 42 in
       Array.iteri (fun i import ->
           Compilation_unit.Name.Tbl.add cmi_index (Import_info.name import) i)
         cmis;
       let cmx_index = Compilation_unit.Tbl.create 42 in
       Array.iteri (fun i import ->
           Compilation_unit.Tbl.add cmx_index (Import_info.cu import) i)
         cmxs;
       let genfns = Generic_fns.Tbl.make () in
       let mk_bitmap arr ix entries ~find ~get_name =
         let module B = Misc.Bitmap in
         let b = B.make (Array.length arr) in
         List.iter (fun import -> B.set b (find ix (get_name import))) entries;
         b
       in
       let units =
         List.map (fun (unit, crc) ->
           ignore (Generic_fns.Tbl.add
                                  ~imports:Generic_fns.Partition.Set.empty
                                  genfns
                                  unit.ui_generic_fns);
           { li_name = unit.ui_unit;
             li_crc = crc;
             li_defines = unit.ui_defines;
             li_force_link = unit.ui_force_link;
             li_imports_cmi =
               mk_bitmap cmis cmi_index unit.ui_imports_cmi
                 ~find:Compilation_unit.Name.Tbl.find
                 ~get_name:Import_info.name;
             li_imports_cmx =
               mk_bitmap cmxs cmx_index unit.ui_imports_cmx
                 ~find:Compilation_unit.Tbl.find
                 ~get_name:Import_info.cu;
             li_external_symbols = Array.of_list unit.ui_external_symbols })
         descr_list
       in
||||||| 23e84b8c4d
         file_list descr_list;
=======
         units;
       let ldeps = Linkdeps.create ~complete:false in
       List.iter
         (fun (filename, (unit, _crc)) ->
            Linkdeps.add ldeps
              ~filename ~compunit:unit.ui_name
              ~provides:[unit.ui_name]
              ~requires:(List.map fst unit.ui_imports_cmx))
         (List.rev units);
       (match Linkdeps.check ldeps with
        | None -> ()
        | Some e -> raise (Error (Link_error e)));
>>>>>>> ocaml/5.4
       let infos =
<<<<<<< HEAD
         { lib_units = units;
           lib_imports_cmi = cmis;
           lib_imports_cmx = cmxs;
           lib_generic_fns = Generic_fns.Tbl.entries genfns;
||||||| 23e84b8c4d
         { lib_units = descr_list;
=======
         { lib_units = List.map snd units;
>>>>>>> ocaml/5.4
           lib_ccobjs = !Clflags.ccobjs;
           lib_ccopts = !Clflags.all_ccopts } in
       output_value outchan infos;
       if Ccomp.create_archive archive_name objfiles <> 0
       then raise(Error(Archiver_error archive_name));
    )

<<<<<<< HEAD
open Format
||||||| 23e84b8c4d
module Style = Misc.Style
open Format
=======
module Style = Misc.Style
open Format_doc
>>>>>>> ocaml/5.4

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Archiver_error name ->
<<<<<<< HEAD
      fprintf ppf "Error while creating the library %s" name
||||||| 23e84b8c4d
      fprintf ppf "Error while creating the library %a" Style.inline_code name
=======
      fprintf ppf "Error while creating the library %a" Style.inline_code name
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e
>>>>>>> ocaml/5.4

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
