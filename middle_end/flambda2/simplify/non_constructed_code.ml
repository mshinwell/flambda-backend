(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type t = unit Code0.t

let code_metadata = Code0.code_metadata

module Metadata_view = struct
  type nonrec 'a t = t

  let metadata = code_metadata
end

include Code_metadata.Code_metadata_accessors [@inlined hint] (Metadata_view)

let create_with_metadata =
  Code0.create_with_metadata ~print_function_params_and_body:Unit.print
    ~params_and_body:()

let create =
  Code0.create ~print_function_params_and_body:Unit.print ~params_and_body:()

let print = Code0.print ~print_function_params_and_body:Unit.print

let free_names = Code0.free_names

let apply_renaming =
  Code0.apply_renaming ~apply_renaming_function_params_and_body:(fun () _ -> ())
