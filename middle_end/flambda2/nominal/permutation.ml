(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* The invariant checks are extremely slow and unlikely to be generally
   useful. *)
let _check_invariants = false

module Make (N : Container_types.S) = struct
  type t =
    | Both of
        { forwards : N.t N.Map.t;
          backwards : N.t N.Map.t
        }
    | Forwards_only of { forwards : N.t N.Map.t }

  let empty = Forwards_only { forwards = N.Map.empty }

  let [@ocamlformat "disable"] print ppf t =
  match t with Both { forwards; backwards; } ->
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      (N.Map.print N.print) backwards
      | Forwards_only {forwards }->
    Format.fprintf ppf "@[((forwards %a))@]"
      (N.Map.print N.print) forwards

  let forwards t =
    match t with Both { forwards; _ } | Forwards_only { forwards } -> forwards

  let backwards t =
    match t with
    | Both { backwards; _ } -> backwards
    | Forwards_only { forwards } -> forwards

  let is_empty t = N.Map.is_empty (forwards t)

  let invariant _ = ()

  (* let[@inline always] invariant { forwards; backwards } = if check_invariants
     then begin let is_bijection map = let domain = N.Map.keys map in let
     range_list = N.Map.data map in let range = N.Set.of_list range_list in
     N.Set.equal domain range in assert (is_bijection forwards); assert
     (N.Map.cardinal forwards = N.Map.cardinal backwards); assert (
     N.Map.for_all (fun n1 n2 -> assert (not (N.equal n1 n2)); match N.Map.find
     n2 backwards with | exception Not_found -> false | n1' -> N.equal n1 n1')
     forwards) end*)

  let apply t n =
    match N.Map.find n (forwards t) with exception Not_found -> n | n -> n

  let apply_backwards t n =
    match N.Map.find n (backwards t) with exception Not_found -> n | n -> n

  let add_to_map n1 n2 map =
    if N.equal n1 n2 then N.Map.remove n1 map else N.Map.add n1 n2 map

  let[@inline always] flip t =
    match t with
    | Both { forwards; backwards } ->
      Both { forwards = backwards; backwards = forwards }
    | Forwards_only _ -> t

  let[@inline always] post_swap t n1 n2 =
    let n1' = apply_backwards t n1 in
    let n2' = apply_backwards t n2 in
    if N.equal n1 n1' && N.equal n2 n2'
    then (
      match t with
      | Both { forwards; backwards } ->
        let forwards = add_to_map n1' n2 (add_to_map n2' n1 forwards) in
        let backwards = add_to_map n2 n1' (add_to_map n1 n2' backwards) in
        let t = Both { forwards; backwards } in
        invariant t;
        t
      | Forwards_only { forwards } ->
        let forwards = add_to_map n1 n2 (add_to_map n2 n1 forwards) in
        let t = Forwards_only { forwards } in
        invariant t;
        t)
    else
      let forwards = add_to_map n1' n2 (add_to_map n2' n1 (forwards t)) in
      let backwards = add_to_map n2 n1' (add_to_map n1 n2' (backwards t)) in
      let t = Both { forwards; backwards } in
      invariant t;
      t

  let pre_swap t n1 n2 = flip (post_swap (flip t) n1 n2)

  let[@inline available] rec compose ~second ~first =
    if is_empty first
    then second
    else
      match N.Map.choose (forwards second) with
      | exception Not_found ->
        invariant first;
        first
      | n1, n2 ->
        (* let () = Format.eprintf "compose: second %a, first %a,
           backtrace:\n%s\n\n%!" print second print first
           (Printexc.raw_backtrace_to_string (Printexc.get_callstack 10)) in *)
        let first = post_swap first n1 n2 in
        let second = pre_swap second n1 n2 in
        compose ~second ~first

  let compose_one ~first n1 n2 = post_swap first n1 n2

  let compose_one_fresh t n1 ~fresh:n2 =
    let n1' = apply_backwards t n1 in
    match t with
    | Both { forwards; backwards } ->
      let forwards = add_to_map n1' n2 (add_to_map n2 n1 forwards) in
      let backwards = add_to_map n2 n1' (add_to_map n1 n2 backwards) in
      let t = Both { forwards; backwards } in
      invariant t;
      t
    | Forwards_only { forwards } ->
      if N.equal n1 n1'
      then (
        let forwards = add_to_map n1 n2 (add_to_map n2 n1 forwards) in
        let t = Forwards_only { forwards } in
        invariant t;
        t)
      else
        let forwards = add_to_map n1' n2 (add_to_map n2 n1 forwards) in
        let backwards = add_to_map n2 n1' (add_to_map n1 n2 forwards) in
        let t = Both { forwards; backwards } in
        invariant t;
        t
end
[@@inline always]
