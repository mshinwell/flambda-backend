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

open Local_store

let lowest_scope  = 0
let highest_scope = 100000000

type t =
  | Local of { name: string; stamp: int }
  | Scoped of { name: string; stamp: int; scope: int }
  | Global of string
  | Predef of { name: string; stamp: int }
      (* the stamp is here only for fast comparison, but the name of
         predefined identifiers is always unique. *)

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = s_ref 0
let predefstamp = s_ref 0

let create_scoped ~scope s =
  incr currentstamp;
  Scoped { name = s; stamp = !currentstamp; scope }

let create_local s =
  incr currentstamp;
  Local { name = s; stamp = !currentstamp }

let create_predef s =
  incr predefstamp;
  Predef { name = s; stamp = !predefstamp }

let create_persistent s =
  Global s

let name = function
  | Local { name; _ }
  | Scoped { name; _ }
  | Global name
  | Predef { name; _ } -> name

let rename = function
  | Local { name; stamp = _ }
  | Scoped { name; stamp = _; scope = _ } ->
      incr currentstamp;
      Local { name; stamp = !currentstamp }
  | id ->
      Misc.fatal_errorf "Ident.rename %s" (name id)

let unique_name = function
  | Local { name; stamp }
  | Scoped { name; stamp } -> name ^ "_" ^ Int.to_string stamp
  | Global name ->
      (* we're adding a fake stamp, because someone could have named his unit
         [Foo_123] and since we're using unique_name to produce symbol names,
         we might clash with an ident [Local { "Foo"; 123 }]. *)
      name ^ "_0"
  | Predef { name; _ } ->
      (* we know that none of the predef names (currently) finishes in
         "_<some number>", and that their name is unique. *)
      name

let unique_toplevel_name = function
  | Local { name; stamp }
  | Scoped { name; stamp } -> name ^ "/" ^ Int.to_string stamp
  | Global name
  | Predef { name; _ } -> name

let persistent = function
  | Global _ -> true
  | _ -> false

let equal i1 i2 =
  match i1, i2 with
  | Local { name = name1; _ }, Local { name = name2; _ }
  | Scoped { name = name1; _ }, Scoped { name = name2; _ }
  | Global name1, Global name2 ->
      name1 = name2
  | Predef { stamp = s1; _ }, Predef { stamp = s2 } ->
      (* if they don't have the same stamp, they don't have the same name *)
      s1 = s2
  | _ ->
      false

let same i1 i2 =
  match i1, i2 with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ }
  | Scoped { stamp = s1; _ }, Scoped { stamp = s2; _ }
  | Predef { stamp = s1; _ }, Predef { stamp = s2 } ->
      s1 = s2
  | Global name1, Global name2 ->
      name1 = name2
  | _ ->
      false

let stamp = function
  | Local { stamp; _ }
  | Scoped { stamp; _ } -> stamp
  | _ -> 0

let scope = function
  | Scoped { scope; _ } -> scope
  | Local _ -> highest_scope
  | Global _ | Predef _ -> lowest_scope

let reinit_level = ref (-1)

let reinit () =
  if !reinit_level < 0
  then reinit_level := !currentstamp
  else currentstamp := !reinit_level

let global = function
  | Local _
  | Scoped _ -> false
  | Global _
  | Predef _ -> true

let is_predef = function
  | Predef _ -> true
  | _ -> false

let print ~with_scope ppf =
  let open Format in
  function
  | Global name -> fprintf ppf "%s!" name
  | Predef { name; stamp = n } ->
      fprintf ppf "%s%s!" name
        (if !Clflags.unique_ids then sprintf "/%i" n else "")
  | Local { name; stamp = n } ->
      fprintf ppf "%s%s" name
        (if !Clflags.unique_ids then sprintf "/%i" n else "")
  | Scoped { name; stamp = n; scope } ->
      fprintf ppf "%s%s%s" name
        (if !Clflags.unique_ids then sprintf "/%i" n else "")
        (if with_scope then sprintf "[%i]" scope else "")

let print_with_scope ppf id = print ~with_scope:true ppf id

let print ppf id = print ~with_scope:false ppf id

module Int = struct
  include Int
  module Map = Numbers.Int.Map
end

module String = Misc.Stdlib.String

type 'a tbl = {
  by_stamp : 'a Int.Map.t;
  by_name : (t * 'a) list Int.Map.t;
}

let empty = {
  by_stamp = Int.Map.empty;
  by_name = Int.Map.empty;
}

let add_to_by_name by_name id name data =
  let name_hash = Hashtbl.hash name in
  match Int.Map.find name_hash by_name with
  | exception Not_found ->
    Int.Map.add name_hash [id, data] by_name
  | entries ->
    Int.Map.add (* replace *) name_hash ((id, data) :: entries) by_name

let add id data { by_stamp; by_name } =
  match id with
  | Global name ->
    let by_name = add_to_by_name by_name id name data in
    { by_stamp; by_name }
  | Local { name; stamp } | Scoped { name; stamp } | Predef { name; stamp } ->
    let by_stamp = Int.Map.add stamp data by_stamp in
    let by_name = add_to_by_name by_name id name data in
    { by_stamp; by_name }

let remove id ({ by_stamp; by_name } as tbl) =
  match id with
  | Global name -> (
    let name_hash = Hashtbl.hash name in
    match Int.Map.find name_hash by_name with
    | exception Not_found -> tbl
    | entries ->
      let entries =
        List.filter (fun (id', _data) -> not (same id id')) entries
      in
      let by_name =
        match entries with
        | [] -> Int.Map.remove name_hash by_name
        | _::_ -> Int.Map.add (* replace *) name_hash entries by_name
      in
      { by_stamp; by_name })
  | Local _ | Scoped _ | Predef _ ->
    Misc.fatal_error "Can only remove [Global] identifiers"

let find_name name_to_find tbl =
  let entries =
    (* This might raise [Not_found] which is intended to escape. *)
    Int.Map.find (Hashtbl.hash name_to_find) tbl.by_name
  in
  let rec loop entries name_to_find =
    match entries with
    | [] -> raise Not_found
    | ((id, _data) as entry)::entries ->
      if String.equal (name id) name_to_find then entry
      else loop entries name_to_find
  in
  loop entries name_to_find

let find_same id ({ by_stamp; by_name = _ } as tbl) =
  match id with
  | Global name -> snd (find_name name tbl)
  | Local { stamp } | Scoped { stamp } | Predef { stamp } ->
    Int.Map.find stamp by_stamp

let find_all name_to_find tbl =
  match Int.Map.find (Hashtbl.hash name_to_find) tbl.by_name with
  | exception Not_found -> []
  | entries ->
    let rec loop entries name_to_find results =
      match entries with
      | [] -> results
      | ((id, _data) as entry)::entries ->
        let results =
          if String.equal (name id) name_to_find then entry :: results
          else results
        in
        loop entries name_to_find results
    in
    loop entries name_to_find []

let add_keys_to_list { by_stamp = _; by_name } acc =
  Int.Map.fold (fun _name_hash entries acc ->
      List.fold_right (fun (id, _data) acc -> id :: acc) entries acc)
    by_name acc

let fold_name f { by_stamp = _; by_name } acc =
  Int.Map.fold (fun _name_hash entries acc ->
      let _names_seen, acc =
        List.fold_left (fun (names_seen, acc) (id, data) ->
            (* Only the most recent occurrence counts here. *)
            let name = name id in
            if String.Set.mem name names_seen then names_seen, acc
            else
              let names_seen = String.Set.add name names_seen in
              let acc = f id data acc in
              names_seen, acc)
          (String.Set.empty, acc) entries
      in
      acc)
    by_name acc

(* Idents for sharing keys *)

(* They should be 'totally fresh' -> neg numbers *)
let key_name = ""

let make_key_generator () =
  let c = ref 1 in
  function
  | Local _
  | Scoped _ ->
      let stamp = !c in
      decr c ;
      Local { name = key_name; stamp = stamp }
  | global_id ->
      Misc.fatal_errorf "Ident.make_key_generator () %s" (name global_id)

let compare x y =
  match x, y with
  | Local x, Local y ->
      let c = x.stamp - y.stamp in
      if c <> 0 then c
      else compare x.name y.name
  | Local _, _ -> 1
  | _, Local _ -> (-1)
  | Scoped x, Scoped y ->
      let c = x.stamp - y.stamp in
      if c <> 0 then c
      else compare x.name y.name
  | Scoped _, _ -> 1
  | _, Scoped _ -> (-1)
  | Global x, Global y -> compare x y
  | Global _, _ -> 1
  | _, Global _ -> (-1)
  | Predef { stamp = s1; _ }, Predef { stamp = s2; _ } -> compare s1 s2

let output oc id = output_string oc (unique_name id)
let hash i = (Char.code (name i).[0]) lxor (stamp i)

let original_equal = equal
include Identifiable.Make (struct
  type nonrec t = t
  let compare = compare
  let output = output
  let print = print
  let hash = hash
  let equal = same
end)
let equal = original_equal
