(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

type t =
  { dbg : Debuginfo.t;
    function_symbol : Linkage_name.t;
    uid : string
  }

let print_debuginfo ppf dbg =
  if Debuginfo.is_none dbg
  then Format.pp_print_string ppf "None"
  else Debuginfo.print_compact ppf dbg

let print ppf { dbg; function_symbol; uid } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(dbg@ %a)@]@ @[<hov 1>(function_symbol@ %a)@]@ @[<hov \
     1>(uid@ %a)@])@]"
    print_debuginfo dbg Linkage_name.print function_symbol
    Format.pp_print_string uid

let none =
  { dbg = Debuginfo.none;
    function_symbol = Linkage_name.of_string "";
    uid = ""
  }

let is_none t = Debuginfo.compare t.dbg Debuginfo.none = 0

let inlining_counter = ref 0

let create ~called_code_id ~apply_dbg =
  let function_symbol = Code_id.linkage_name called_code_id in
  let uid =
    incr inlining_counter;
    (* XXX hmm.... *)
    Hashtbl.hash (apply_dbg, function_symbol, !inlining_counter)
    |> string_of_int
  in
  { dbg = apply_dbg; function_symbol; uid }

let rewrite t dbg =
  if is_none t
  then dbg
  else
    let dbg =
      (* uids of _all_ [Debuginfo.t] values in the body of the function being
         inlined get freshened (consistently, for any given inlining) *)
      List.mapi
        (fun n ({ dinfo_function_symbol; _ } as item : Debuginfo.item) ->
          let dinfo_function_symbol =
            if n = 0
            then Some (Linkage_name.to_string t.function_symbol)
            else dinfo_function_symbol
          in
          { item with dinfo_uid = Some t.uid; dinfo_function_symbol })
        (Debuginfo.to_items dbg)
    in
    let inlined_debuginfo =
      Debuginfo.with_function_symbol_on_first_item t.dbg ~function_symbol:None
    in
    Debuginfo.inline inlined_debuginfo (Debuginfo.of_items dbg)

let function_symbol t = t.function_symbol

let uid t = t.uid

let compare t1 t2 =
  let c = Debuginfo.compare t1.dbg t2.dbg in
  if c <> 0
  then c
  else
    let c = Linkage_name.compare t1.function_symbol t2.function_symbol in
    if c <> 0 then c else String.compare t1.uid t2.uid
