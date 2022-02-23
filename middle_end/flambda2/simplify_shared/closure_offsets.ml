(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import
module EO = Exported_offsets

type used_names =
  { closure_ids_normal : Closure_id.Set.t;
    closure_ids_in_types : Closure_id.Set.t;
    closure_vars_normal : Var_within_closure.Set.t;
    closure_vars_in_types : Var_within_closure.Set.t
  }

let[@inline] closure_var_is_used ~used_closure_vars v =
  if Compilation_unit.is_current (Var_within_closure.get_compilation_unit v)
  then Var_within_closure.Set.mem v used_closure_vars
  else true

let keep_closure_var ~used_closure_vars v =
  match (used_closure_vars : _ Or_unknown.t) with
  | Unknown -> true
  | Known used_closure_vars -> closure_var_is_used ~used_closure_vars v

(* CR gbury: considering that the goal is to have `offsets` significantly
   smaller than the `imported_offsets`, it might be better for performance to
   check whether the closure_id/var is already in the offsets before looking it
   up in the imported offsets ? *)
let reexport_closure_ids closure_id_set offsets =
  let imported_offsets = EO.imported_offsets () in
  Closure_id.Set.fold
    (fun closure_id offsets ->
      if Compilation_unit.is_current
           (Closure_id.get_compilation_unit closure_id)
      then offsets
      else
        match EO.closure_offset imported_offsets closure_id with
        | None ->
          Misc.fatal_errorf
            "Closure id %a is used in the current compilation unit, but not \
             present in the imported offsets."
            Closure_id.print closure_id
        | Some info -> EO.add_closure_offset offsets closure_id info)
    closure_id_set offsets

let reexport_closure_vars closure_var_set offsets =
  let imported_offsets = EO.imported_offsets () in
  Var_within_closure.Set.fold
    (fun env_var offsets ->
      if Compilation_unit.is_current
           (Var_within_closure.get_compilation_unit env_var)
      then offsets
      else
        match EO.env_var_offset imported_offsets env_var with
        | None ->
          Misc.fatal_errorf
            "Env var %a is used in the current compilation unit, but not \
             present in the imported offsets."
            Var_within_closure.print env_var
        | Some info -> EO.add_env_var_offset offsets env_var info)
    closure_var_set offsets

(* Compute offsets for elements within a closure block

   Closure_ids and environment values within a closure block can occur in more
   than one closure blocks, but need to have the same offset in all blocks where
   they appear.

   We assume an ideal (i.e. compact) layout for a block containing a set of
   closures is the following: *)
(*
 * |----------------------|
 * | header               |
 * |----------------------| offset 0
 * | fn_block_0           |
 * | (pos 0, size 2 or 3) |
 * |----------------------|
 * | Infix_header         |
 * |----------------------|
 * | fn_block_1           |
 * | (pos x, size 2 or 3) |  (x=3 if fn_block_0 is of size 2 for instance)
 * |----------------------|
 * | Infix_header         |
 * |----------------------|
 * .                      .
 * .                      .
 * .                      .
 * |----------------------|
 * | Infix_header         |
 * |----------------------|
 * | fn_block             |
 * | (last closure slot)  |
 * |----------------------|
 * | Env_value (slot 0)   | <- start of env
 * |----------------------|
 * | Env_value (slot 1)   |
 * |----------------------|
 * .                      .
 * .                      .
 * |----------------------|
 * | Env_value (last slot)|
 * |----------------------|
 *)
(* However, that ideal layout may not be possible in certain circumstances, as
   there may be arbitrary holes between slots (i.e. unused words in the block).

   Starting from ocaml 4.12, all closure slots must occur before all env var
   slots, since the start of the environment is recorded in the arity field of
   each function slot arity field.

   This additional requirement makes more constraints impossible to satisfy (to
   be clear, there are situations impossible to satisfy regardless of this
   requirement; it's just that this requirement makes some situations that were
   previously possible to satisfy be now unsatisfiable).

   For instance, it is perfectly possible to have a situation where an env_value
   has been fixed at offset 3 (because it is in a simple closure with one
   function of arity > 1 in another cmx), however it is in a closure set with
   more than one closure in the current compilation unit. In this case, it is
   impossible to make all the closures fit before the env_value in the closure
   block. *)

type layout_slot =
  | Env_var of Var_within_closure.t
  | Infix_header
  | Closure of Closure_id.t

type layout =
  { startenv : int;
    empty_env : bool;
    slots : (int * layout_slot) list
  }

let order_closures env l acc =
  List.fold_left
    (fun acc closure ->
      match EO.closure_offset env closure with
      | Some Dead_id -> acc
      | Some (Id_slot { size = _; offset }) ->
        Numeric_types.Int.Map.add offset (Closure closure) acc
      | None ->
        Misc.fatal_errorf "No closure offset for %a" Closure_id.print closure)
    acc l

let order_env_vars env l acc =
  List.fold_left
    (fun acc env_var ->
      match EO.env_var_offset env env_var with
      | Some Dead_var -> acc
      | Some (Var_slot { offset }) ->
        Numeric_types.Int.Map.add offset (Env_var env_var) acc
      | None ->
        Misc.fatal_errorf "No closure var offset for %a"
          Var_within_closure.print env_var)
    acc l

let layout_aux j slot (startenv, acc_slots) =
  match slot with
  (* Starting from ocaml 4.12, all closure slots *must* precede all env var
     slots. The algorithms in this file should thus only generate slot
     assignments that respect this invariant. If that is not the case, this is a
     fatal error given that the start of the environment (i.e. the offset of the
     first env var slot, with the added property that all slots after that are
     env vars slots (or at least scannable by the GC)), is needed by the GC when
     scanning the block. Thus, if we see a closure slot, we check that then the
     environment has not started yet (i.e. we have not seen any env var
     slots). *)
  | Closure _ when j = 0 ->
    assert (acc_slots = []);
    assert (startenv = None);
    (* see comment above *)
    let acc_slots = [0, slot] in
    startenv, acc_slots
  | Closure _ ->
    assert (startenv = None);
    (* see comment above *)
    let acc_slots = (j, slot) :: (j - 1, Infix_header) :: acc_slots in
    startenv, acc_slots
  | Env_var _ ->
    let startenv =
      match startenv with
      | Some i ->
        assert (i < j);
        startenv
      | None -> Some j
    in
    let acc_slots = (j, slot) :: acc_slots in
    startenv, acc_slots
  | Infix_header ->
    (* Internal invariant: such layout slots are not generated by the {order}
       function, so they should not appear. *)
    assert false

let layout env closures env_vars =
  let map =
    Numeric_types.Int.Map.empty
    |> order_env_vars env env_vars
    |> order_closures env closures
  in
  let startenv_opt, acc_slots =
    Numeric_types.Int.Map.fold layout_aux map (None, [])
  in
  let startenv, empty_env =
    (* If there are no env vars, the start of env is considered to be the
       (non-existing) slot after the last slot used, and if the set is empty,
       the value does not matter. *)
    match startenv_opt, acc_slots with
    | Some i, _ -> i, false
    | None, [] -> 0, true
    | None, (j, Closure closure) :: _ -> begin
      match EO.closure_offset env closure with
      | Some (Id_slot { size; _ }) -> j + size, true
      | Some Dead_id | None ->
        (* the closure was found earlier during the call to order_closures *)
        assert false
    end
    | None, (_, Infix_header) :: _ ->
      (* Cannot happen because a infix header is *always* preceded by a Closure
         slot (because the slot list is reversed) *)
      assert false
    | None, (_, Env_var _) :: _ ->
      (* Cannot happen because if there is an env var in the acc, then
         startenv_opt should be Some _ *)
      assert false
  in
  let slots = List.rev acc_slots in
  (* The Gc assumes that a set of closures block actually starts with a closure
     slot at offset 0. Or more precisely, the GC unconditionally reads the
     second field of a set of closures to find out the start of environment.
     Thus we add a check here to ensure that the slots start with a closure at
     offset 0. *)
  match slots with
  | (0, Closure _) :: _ -> { startenv; slots; empty_env }
  | _ ->
    Misc.fatal_error
      "Sets of closures must start with a closure slot at offset 0"

let print_layout_slot fmt = function
  | Env_var v -> Format.fprintf fmt "var %a" Var_within_closure.print v
  | Infix_header -> Format.fprintf fmt "infix_header"
  | Closure cid -> Format.fprintf fmt "closure %a" Closure_id.print cid

let print_layout fmt l =
  Format.fprintf fmt "@[<v>startenv: %d;@ " l.startenv;
  List.iter
    (fun (i, slot) ->
      Format.fprintf fmt "@[<h>%d %a@]@," i print_layout_slot slot)
    l.slots;
  Format.fprintf fmt "@]"

(* Greedy algorithm *)

module Greedy = struct
  (** Greedy algorithm for assigning slots to closures and environment
      variables.

      Slots are assigned using a "first comes, first served" basis, filling
      upwards from 0.

      As much as is possible, the algorithm tries and put first all the closure
      slots, and then all the env_var slots, however, that may be impossible
      because of constraints read from a cmx.

      This strategy should be able to correctly compute offsets for all
      legitimate situations, with no expected blowup of computation time.
      However the generated offsets can be far from optimal (i.e. leave more
      holes than necessary). *)

  (* Internal types *)

  type slot_desc =
    | Closure of Closure_id.t
    | Env_var of Var_within_closure.t

  type slot_pos =
    | Assigned of int
    | Unassigned
    | Removed

  type set_of_closures =
    { id : int;
      (* Info about start of environment *)
      mutable first_slot_used_by_envvar : int;
      mutable first_slot_after_closures : int;
      (* Slots to be allocated *)
      mutable unallocated_closure_slots : slot list;
      mutable unallocated_env_var_slots : slot list;
      mutable allocated_slots : slot Numeric_types.Int.Map.t
    }

  and slot =
    { desc : slot_desc;
      mutable pos : slot_pos;
      mutable size : int;
      mutable sets : set_of_closures list
    }

  (** Intermediate state to store slots for closures and environment variables
      before computing the actual offsets of these elements within a block. *)
  type state =
    { used_offsets : EO.t;
      closures : slot Closure_id.Map.t;
      env_vars : slot Var_within_closure.Map.t;
      sets_of_closures : set_of_closures list
    }

  (* Create structures *)

  (* create a fresh slot (with no position allocated yet) *)
  let create_slot size desc = { desc; size; pos = Unassigned; sets = [] }

  let make_set =
    let c = ref 0 in
    fun _ ->
      incr c;
      { id = !c;
        first_slot_after_closures = 0;
        first_slot_used_by_envvar = max_int;
        unallocated_closure_slots = [];
        unallocated_env_var_slots = [];
        allocated_slots = Numeric_types.Int.Map.empty
      }

  let create_initial_state () =
    { used_offsets = EO.empty;
      closures = Closure_id.Map.empty;
      env_vars = Var_within_closure.Map.empty;
      sets_of_closures = []
    }

  (* debug printing *)
  let print_set_id fmt s = Format.fprintf fmt "%d" s.id

  let print_set_ids fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_set_id s) l

  let print_desc fmt = function
    | Closure c -> Format.fprintf fmt "%a" Closure_id.print c
    | Env_var v -> Format.fprintf fmt "%a" Var_within_closure.print v

  let print_slot_desc fmt s = print_desc fmt s.desc

  let print_slot_descs fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_slot_desc s) l

  let print_slot_pos fmt = function
    | Assigned i -> Format.fprintf fmt "%d" i
    | Unassigned -> Format.fprintf fmt "?"
    | Removed -> Format.fprintf fmt "x"

  let print_slot fmt s =
    Format.fprintf fmt "@[<h>[pos: %a;@ size: %d;@ sets: %a;@ desc: %a]@]@,"
      print_slot_pos s.pos s.size print_set_ids s.sets print_desc s.desc

  let print_slots fmt map =
    Numeric_types.Int.Map.iter (fun _ slot -> print_slot fmt slot) map

  let print_set fmt s =
    Format.fprintf fmt
      "@[<v 2>%d:@ @[<v>first_slot_after_closures: %d;@ \
       first_slot_used_by_envvar: %d;@ unallocated_closures: @[<v>%a@];@ \
       unallocated_env_vars: @[<v>%a@];@ allocated: @[<v>%a@]@]@]"
      s.id s.first_slot_after_closures s.first_slot_used_by_envvar
      print_slot_descs s.unallocated_closure_slots print_slot_descs
      s.unallocated_env_var_slots print_slots s.allocated_slots

  let print_sets fmt l =
    List.iter (function s -> Format.fprintf fmt "%a@ " print_set s) l

  let [@ocamlformat "disable"] print fmt state =
    Format.fprintf fmt
      "@[<v 2>Sets of closures:@ %a@]"
      print_sets state.sets_of_closures
  [@@warning "-32"]

  (* Keep the env vars offsets in sets up-to-date *)

  let update_set_for_slot slot set =
    begin
      match slot.pos with
      | Unassigned | Removed -> ()
      | Assigned i -> begin
        match slot.desc with
        | Env_var _ ->
          set.first_slot_used_by_envvar <- min set.first_slot_used_by_envvar i
        | Closure _ ->
          set.first_slot_after_closures
            <- max set.first_slot_after_closures (i + slot.size)
      end
    end;
    if set.first_slot_used_by_envvar < set.first_slot_after_closures
    then
      Misc.fatal_errorf
        "Set of closure invariant (all closures before all env vars) is broken"

  (* Slots *)

  let is_closure_slot slot =
    match slot.desc with
    | Closure _ ->
      assert (slot.size = 2 || slot.size = 3);
      true
    | Env_var _ ->
      assert (slot.size = 1);
      false

  let add_slot_offset_to_set offset slot set =
    update_set_for_slot slot set;
    let map = set.allocated_slots in
    assert (not (Numeric_types.Int.Map.mem offset map));
    let map = Numeric_types.Int.Map.add offset slot map in
    set.allocated_slots <- map

  let add_slot_offset env slot offset =
    assert (slot.pos = Unassigned);
    slot.pos <- Assigned offset;
    List.iter (add_slot_offset_to_set offset slot) slot.sets;
    match slot.desc with
    | Closure c ->
      let (info : EO.closure_info) = EO.Id_slot { offset; size = slot.size } in
      EO.add_closure_offset env c info
    | Env_var v ->
      let (info : EO.env_var_info) = EO.Var_slot { offset } in
      EO.add_env_var_offset env v info

  let mark_slot_as_removed env slot =
    match slot.pos with
    | Removed -> env
    | Unassigned -> (
      slot.pos <- Removed;
      match slot.desc with
      | Closure _ -> Misc.fatal_error "Closure cannot be removed currently"
      | Env_var v ->
        let (info : EO.env_var_info) = EO.Dead_var in
        EO.add_env_var_offset env v info)
    | Assigned _ ->
      Misc.fatal_error "Cannot remove closure var which is already assigned"

  (* Sets of Closures *)

  let add_set_to_state state set =
    { state with sets_of_closures = set :: state.sets_of_closures }

  let add_unallocated_slot_to_set slot set =
    slot.sets <- set :: slot.sets;
    update_set_for_slot slot set;
    match slot.desc with
    | Closure _ ->
      set.unallocated_closure_slots <- slot :: set.unallocated_closure_slots
    | Env_var _ ->
      set.unallocated_env_var_slots <- slot :: set.unallocated_env_var_slots

  (* Accumulator state *)

  let use_closure_info state c info =
    let used_offsets = EO.add_closure_offset state.used_offsets c info in
    { state with used_offsets }

  let add_closure_slot state closure slot =
    let closures = Closure_id.Map.add closure slot state.closures in
    { state with closures }

  let use_env_var_info state var info =
    let used_offsets = EO.add_env_var_offset state.used_offsets var info in
    { state with used_offsets }

  let add_env_var_slot state var slot =
    { state with env_vars = Var_within_closure.Map.add var slot state.env_vars }

  let find_closure_slot state closure =
    Closure_id.Map.find_opt closure state.closures

  let find_env_var_slot state var =
    Var_within_closure.Map.find_opt var state.env_vars

  (* Create slots (and create the cross-referencing). *)

  let rec create_closure_slots set state all_code = function
    | [] -> state
    | (c, code_id) :: r ->
      let s, state =
        match find_closure_slot state c with
        | Some s -> s, state
        | None -> (
          if Compilation_unit.is_current (Closure_id.get_compilation_unit c)
          then
            let code_metadata =
              Code_id.Map.find code_id all_code
              |> Code_or_metadata.code_metadata
            in
            let module CM = Code_metadata in
            let is_tupled = CM.is_tupled code_metadata in
            let params_arity = CM.params_arity code_metadata in
            let arity = List.length params_arity in
            let size = if arity = 1 && not is_tupled then 2 else 3 in
            let s = create_slot size (Closure c) in
            s, add_closure_slot state c s
          else
            (* We should be guaranteed that the corresponding compilation unit's
               cmx file has been read during the downward traversal. *)
            let imported_offsets = EO.imported_offsets () in
            match EO.closure_offset imported_offsets c with
            | None ->
              (* This means that there is no cmx for the given closure id
                 (either because of opaque, (or missing cmx ?), or that the
                 offset is missing from the cmx. In any case, this is a hard
                 error: the closure id must have been given an offset by its own
                 compilation unit, and we must know it to avoid choosing a
                 different one. *)
              Misc.fatal_errorf
                "Could not find the offset for closure id %a from another \
                 compilation unit (because of -opaque, or missing cmx)."
                Closure_id.print c
            | Some Dead_id ->
              Misc.fatal_errorf
                "The closure id %a is dead in its original compilation unit, \
                 it should not occur in a set of closures in this compilation \
                 unit."
                Closure_id.print c
            | Some (Id_slot { offset; size } as info) ->
              let s =
                { desc = Closure c; size; pos = Assigned offset; sets = [] }
              in
              s, add_closure_slot (use_closure_info state c info) c s)
      in
      let () = add_unallocated_slot_to_set s set in
      create_closure_slots set state all_code r

  let rec create_env_var_slots set state = function
    | [] -> state
    | v :: r ->
      let s, state =
        match find_env_var_slot state v with
        | Some s -> s, state
        | None -> (
          if Compilation_unit.is_current
               (Var_within_closure.get_compilation_unit v)
          then
            let s = create_slot 1 (Env_var v) in
            s, add_env_var_slot state v s
          else
            (* Same as the comments for the closure_ids *)
            let imported_offsets = EO.imported_offsets () in
            match EO.env_var_offset imported_offsets v with
            | None ->
              (* See comment for the closure_id *)
              Misc.fatal_errorf
                "Could not find the offset for env var %a from another \
                 compilation unit (because of -opaque, or missing cmx)."
                Var_within_closure.print v
            | Some Dead_var ->
              Misc.fatal_errorf
                "The closure var %a has been removed by its original \
                 compilation unit, it should not occur in a set of closures in \
                 this compilation unit."
                Var_within_closure.print v
            | Some (Var_slot { offset } as info) ->
              let s =
                { desc = Env_var v; size = 1; pos = Assigned offset; sets = [] }
              in
              s, add_env_var_slot (use_env_var_info state v info) v s)
      in
      let () = add_unallocated_slot_to_set s set in
      create_env_var_slots set state r

  let create_slots_for_set state all_code set_id =
    let set = make_set set_id in
    let state = add_set_to_state state set in
    (* Fill closure slots *)
    let function_decls = Set_of_closures.function_decls set_id in
    let closure_map = Function_declarations.funs function_decls in
    let closures = Closure_id.Map.bindings closure_map in
    let state = create_closure_slots set state all_code closures in
    (* Fill env var slots *)
    let env_map = Set_of_closures.closure_elements set_id in
    let env_vars = List.map fst (Var_within_closure.Map.bindings env_map) in
    let state = create_env_var_slots set state env_vars in
    state

  (* Folding functions. To avoid pathological cases in allocating slots to
     offsets, folding on slots is done by consuming the first unallocated slot
     of each set of closures, and then repeating until all slots have been
     consumed. *)

  let rec fold_on_unallocated_closure_slots f acc state =
    let has_work_been_done = ref false in
    let aux acc set =
      match set.unallocated_closure_slots with
      | [] -> acc
      | slot :: r ->
        has_work_been_done := true;
        set.unallocated_closure_slots <- r;
        f acc slot
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done
    then res
    else fold_on_unallocated_closure_slots f res state

  let rec fold_on_unallocated_env_var_slots ~used_closure_vars f_kept f_removed
      acc state =
    let has_work_been_done = ref false in
    let rec aux acc set =
      match set.unallocated_env_var_slots with
      | [] -> acc
      | ({ desc = Env_var v; _ } as slot) :: r ->
        set.unallocated_env_var_slots <- r;
        if keep_closure_var ~used_closure_vars v
        then begin
          has_work_been_done := true;
          f_kept acc slot
        end
        else aux (f_removed acc slot) set
      | { desc = Closure _; _ } :: _ -> assert false
      (* invariant *)
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done
    then res
    else
      fold_on_unallocated_env_var_slots ~used_closure_vars f_kept f_removed res
        state

  (* Find the first space available to fit a given slot.

     This function returns the first free offset with enough space to fit the
     slot (potential header included), but points at the start of the free space
     (so the header word for closure which need it). Function {assign_offset} is
     here to compute the actual offset/position from this free space start
     position.

     This function is abit more compicated than necessary because each slot's
     size does not include the header for closures. There are two reasons for
     that choice: - the closure slot at offset 0 does not need a header since it
     uses the header of the whole block, so the necessity of a header is
     actually dependant on the position of the closure slot. - that way, the
     offset/position of a slot corresponds to the actual ocaml pointer (which
     points at the first field of a block rather than the header). *)

  let first_free_offset slot set start =
    let map = set.allocated_slots in
    (* space needed to fit a slot at the current offset. *)
    let needed_space curr =
      if is_closure_slot slot && curr <> 0 then slot.size + 1 else slot.size
    in
    (* first offset used by a slot *)
    let first_used_by s =
      match s.pos with
      | Unassigned | Removed -> assert false
      | Assigned pos -> if is_closure_slot s && pos <> 0 then pos - 1 else pos
    in
    (* first potentially free offset after a slot *)
    let first_free_after slot =
      match slot.pos with
      | Unassigned | Removed -> assert false
      | Assigned i -> i + slot.size
    in
    (* Adjust a starting position to not point in the middle of a block.
       Additionally, ensure the env var slots are put after the closure
       slots. *)
    let adjust (curr : int) =
      let curr =
        if is_closure_slot slot
        then curr
        else max curr set.first_slot_after_closures
      in
      match Numeric_types.Int.Map.find_last (fun i -> i <= curr) map with
      | exception Not_found -> curr
      | j, s ->
        assert (Assigned j = s.pos);
        max curr (first_free_after s)
    in
    (* find the first available space for the slot. *)
    let rec loop curr =
      match Numeric_types.Int.Map.find_first (fun i -> i >= curr) map with
      | exception Not_found -> curr
      | _, next_slot ->
        let available_space = first_used_by next_slot - curr in
        assert (available_space >= 0);
        if available_space >= needed_space curr
        then curr
        else loop (first_free_after next_slot)
    in
    loop (adjust start)

  (** Assign an offset using the current offset, assuming there is enough space *)
  let assign_offset slot offset =
    if not (is_closure_slot slot)
    then
      offset
      (* closure need a header (infix_tag) before them, except for the first one
         (which uses the closure block header). *)
    else if offset = 0
    then offset
    else offset + 1

  (* Loop to find the first free offset available for a slot given the set of
     sets in which it appears. *)
  let rec first_available_offset slot start first_set other_sets =
    let aux ((_, offset) as acc) s =
      let new_offset = first_free_offset slot s offset in
      assert (new_offset >= offset);
      if new_offset = offset then acc else true, new_offset
    in
    let start = first_free_offset slot first_set start in
    let changed, offset = List.fold_left aux (false, start) other_sets in
    if not changed
    then assign_offset slot offset
    else first_available_offset slot offset first_set other_sets

  let first_available_offset slot start = function
    | s :: r -> first_available_offset slot start s r
    | [] ->
      (* Internal invariant: a slot cannot have an empty list of sets it belongs
         to (at least not slots for which we need to assign an offset), thus
         this case cannot happen. *)
      assert false

  (* Assign offsets to closure slots *)

  let assign_slot_offset env slot =
    match slot.pos with
    | Unassigned ->
      let offset = first_available_offset slot 0 slot.sets in
      add_slot_offset env slot offset
    | Assigned _pos -> env
    | Removed ->
      Misc.fatal_error
        "Slot has been explicitly removed, it cannot be assigned anymore"

  let assign_closure_offsets state env =
    fold_on_unallocated_closure_slots assign_slot_offset env state

  let assign_env_var_offsets ~used_closure_vars state env =
    fold_on_unallocated_env_var_slots ~used_closure_vars assign_slot_offset
      mark_slot_as_removed env state

  (* Ensure closure ids/env vars that are used in projections in the current
     compilation unit are present in the offsets returned by finalize *)
  let imported_and_used_offsets ~used_names state =
    match (used_names : _ Or_unknown.t) with
    | Known
        { closure_ids_normal;
          closure_ids_in_types;
          closure_vars_normal;
          closure_vars_in_types
        } ->
      state.used_offsets
      |> reexport_closure_ids closure_ids_normal
      |> reexport_closure_ids closure_ids_in_types
      |> reexport_closure_vars closure_vars_normal
      |> reexport_closure_vars closure_vars_in_types
    | Unknown -> EO.imported_offsets ()

  (* We only want to keep closure vars that appear in the creation of a set of
     closures, *and* appear as projection (at normal name mode). And we need to
     mark closure_vars/ids that are not alive, as dead in the exported_offsets,
     so that later compilation unit do not mistake that for a missing offset
     info on a closure_var/id. *)
  let alive_closure_elts state offsets used_names =
    match (used_names : used_names Or_unknown.t) with
    | Unknown -> Or_unknown.Unknown, offsets
    | Known { closure_vars_normal; closure_ids_normal; _ } ->
      let offsets =
        Closure_id.Set.fold
          (fun closure_id acc ->
            if Compilation_unit.is_current
                 (Closure_id.get_compilation_unit closure_id)
            then
              match find_closure_slot state closure_id with
              | Some _ -> acc
              | None -> EO.add_closure_offset acc closure_id Dead_id
            else acc)
          closure_ids_normal offsets
      in
      let offsets = ref offsets in
      let used_closure_vars =
        Var_within_closure.Set.filter
          (fun closure_var ->
            if Compilation_unit.is_current
                 (Var_within_closure.get_compilation_unit closure_var)
            then (
              (* a closure var appears in a set of closures iff it has a slot *)
              match find_env_var_slot state closure_var with
              | Some _ -> true
              | None ->
                offsets := EO.add_env_var_offset !offsets closure_var Dead_var;
                false)
            else true)
          closure_vars_normal
      in
      Or_unknown.Known used_closure_vars, !offsets

  (* Transform an internal accumulator state for slots into an actual mapping
     that assigns offsets. *)
  let finalize ~used_names state =
    let offsets = imported_and_used_offsets ~used_names state in
    let used_closure_vars, offsets =
      alive_closure_elts state offsets used_names
    in
    let offsets = assign_closure_offsets state offsets in
    let offsets = assign_env_var_offsets ~used_closure_vars state offsets in
    used_closure_vars, offsets
end

type t = Set_of_closures.t list

let print fmt l =
  Format.fprintf fmt "@[<hv>%a@]" (Format.pp_print_list Set_of_closures.print) l

let create () = []

let add_set_of_closures l ~is_phantom ~all_code:_ set_of_closures =
  if is_phantom then l else set_of_closures :: l

let add_offsets_from_function l1 ~from_function:l2 = l2 @ l1

let finalize_offsets ~all_code ~used_names l =
  let state = ref (Greedy.create_initial_state ()) in
  Misc.try_finally
    (fun () ->
      List.iter
        (fun set_of_closures ->
          state := Greedy.create_slots_for_set !state all_code set_of_closures)
        l;
      Greedy.finalize ~used_names !state)
    ~always:(fun () ->
      if Flambda_features.dump_closure_offsets ()
      then Format.eprintf "%a@." Greedy.print !state)

let closure_name id =
  let compunit = Closure_id.get_compilation_unit id in
  let name = Compilation_unit.get_linkage_name compunit in
  Format.asprintf "%a__%s" Linkage_name.print name (Closure_id.to_string id)

let closure_code s = Format.asprintf "%s_code" s
