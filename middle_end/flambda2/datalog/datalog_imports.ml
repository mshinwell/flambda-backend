(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

type 'a with_name =
  { value : 'a;
    name : string
  }

type 'a with_names =
  { values : 'a;
    names : string list
  }

include Heterogenous_list

module Option_ref = struct
  include Make (struct
    type 'a t = 'a option ref
  end)

  let rec get : type s. s hlist -> s Constant.hlist = function
    | [] -> []
    | r :: rs -> Option.get r.contents :: get rs

  let rec set : type s. s hlist -> s Constant.hlist -> unit =
   fun refs values ->
    match refs, values with
    | [], [] -> ()
    | r :: rs, v :: vs ->
      r.contents <- Some v;
      set rs vs
end
