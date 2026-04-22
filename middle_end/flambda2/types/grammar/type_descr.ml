(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Or_bottom.Let_syntax

(* This module conceals the implementation of type ['head t]. Functions such as
   [T.descr] can be inlined with return values unboxed by Flambda 2. *)
module T : sig
  module Descr : sig
    type 'head t = private
      | No_alias of 'head
      | Equals of Simple.t

    val print :
      print_head:(Format.formatter -> 'head -> unit) ->
      Format.formatter ->
      'head t ->
      unit

    val apply_renaming :
      apply_renaming_head:('head -> Renaming.t -> 'head) ->
      'head t ->
      Renaming.t ->
      'head t

    val free_names :
      free_names_head:('head -> Name_occurrences.t) ->
      'head t ->
      Name_occurrences.t
  end

  type 'head t

  val create : 'head -> 'head t

  val create_equals : Simple.t -> _ t

  val bottom : _ t

  val unknown : _ t

  val descr :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    'head t ->
    'head Descr.t Or_unknown_or_bottom.t

  val is_obviously_bottom : _ t -> bool

  val is_obviously_unknown : _ t -> bool

  val get_alias_exn : 'head t -> Simple.t

  val apply_renaming :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Renaming.t ->
    'head t

  val free_names :
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t

  val free_names_no_cache :
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t

  val remove_unused_value_slots_and_shortcut_aliases :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    remove_unused_value_slots_and_shortcut_aliases_head:
      ('head ->
      used_value_slots:Value_slot.Set.t ->
      canonicalise:(Simple.t -> Simple.t) ->
      'head) ->
    'head t ->
    used_value_slots:Value_slot.Set.t ->
    canonicalise:(Simple.t -> Simple.t) ->
    'head t

  val project_variables_out :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    to_project:Variable.Set.t ->
    expand:(Variable.t -> coercion:Coercion.t -> 'head t) ->
    project_head:('head -> 'head) ->
    'head t ->
    'head t
end = struct
  module Descr = struct
    type 'head t =
      | No_alias of 'head
      | Equals of Simple.t

    let print ~print_head ppf t =
      match t with
      | No_alias head -> print_head ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(%t=%t %a)@]" Flambda_colours.error
          Flambda_colours.pop Simple.print simple

    let[@inline always] apply_renaming ~apply_renaming_head t renaming =
      if Renaming.is_identity renaming
      then t
      else
        match t with
        | No_alias head ->
          let head' = apply_renaming_head head renaming in
          if head == head' then t else No_alias head'
        | Equals simple ->
          let simple' = Simple.apply_renaming simple renaming in
          if simple == simple' then t else Equals simple'

    let[@inline always] free_names ~free_names_head t =
      match t with
      | No_alias head -> free_names_head head
      | Equals simple ->
        Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
          (Simple.free_names simple) Name_mode.in_types
  end

  module WCFN = With_cached_free_names

  (* [t] pulls the [Unknown] and [Bottom] cases out of the [WCFN.t] wrapper so
     that [bottom] and [unknown] can be fully polymorphic constants (the value
     restriction would otherwise make them weakly polymorphic, since [WCFN.t]
     has mutable fields).

     The [WCFN.t] wraps a ['head descr_payload] and carries the delayed
     renaming; therefore [apply_renaming] on an [Ok _] value is O(1): it just
     composes the new renaming into the existing delayed renaming stored on the
     [WCFN.t]. *)
  type 'head descr_payload =
    | Equals of Simple.t
    | No_alias of 'head

  type 'head t =
    | Unknown
    | Bottom
    | Ok of 'head descr_payload WCFN.t

  let apply_renaming_descr_payload ~apply_renaming_head t renaming =
    match t with
    | Equals simple ->
      let simple' = Simple.apply_renaming simple renaming in
      if simple == simple' then t else Equals simple'
    | No_alias head ->
      let head' = apply_renaming_head head renaming in
      if head == head' then t else No_alias head'

  let free_names_descr_payload ~free_names_head t =
    match t with
    | Equals simple ->
      Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
        (Simple.free_names simple) Name_mode.in_types
    | No_alias head -> free_names_head head

  let[@inline always] descr ~apply_renaming_head (t : 'head t) :
      'head Descr.t Or_unknown_or_bottom.t =
    match t with
    | Unknown -> Unknown
    | Bottom -> Bottom
    | Ok wcfn ->
      let payload =
        WCFN.descr
          ~apply_renaming_descr:
            (apply_renaming_descr_payload ~apply_renaming_head)
          wcfn
      in
      let descr : _ Descr.t =
        match payload with
        | Equals simple -> Equals simple
        | No_alias head -> No_alias head
      in
      Ok descr

  let create head : _ t = Ok (WCFN.create (No_alias head))

  let create_equals simple : _ t = Ok (WCFN.create (Equals simple))

  let bottom : _ t = Bottom

  let unknown : _ t = Unknown

  let is_obviously_bottom (t : _ t) =
    match t with Bottom -> true | Unknown | Ok _ -> false

  let is_obviously_unknown (t : _ t) =
    match t with Unknown -> true | Bottom | Ok _ -> false

  (* [get_alias_exn] peeks at the [WCFN.t]'s raw descr so that we only have to
     traverse the head in the (unusual) [Equals] case, not the common [No_alias]
     case. *)
  let[@inline always] get_alias_exn (t : _ t) =
    match t with
    | Unknown | Bottom -> raise Not_found
    | Ok wcfn -> (
      match WCFN.peek_descr wcfn with
      | No_alias _ -> raise Not_found
      | Equals simple ->
        let renaming = WCFN.peek_delayed_renaming wcfn in
        if Renaming.is_identity renaming
        then simple
        else Simple.apply_renaming simple renaming)

  let apply_renaming ~apply_renaming_head ~free_names_head (t : _ t) renaming :
      _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok wcfn ->
      let wcfn' =
        WCFN.apply_renaming
          ~apply_renaming_descr:
            (apply_renaming_descr_payload ~apply_renaming_head)
          ~free_names_descr:(free_names_descr_payload ~free_names_head)
          wcfn renaming
      in
      if wcfn == wcfn' then t else Ok wcfn'

  let free_names ~free_names_head (t : _ t) =
    match t with
    | Unknown | Bottom -> Name_occurrences.empty
    | Ok wcfn ->
      WCFN.free_names
        ~free_names_descr:(free_names_descr_payload ~free_names_head)
        wcfn

  let free_names_no_cache ~free_names_head (t : _ t) =
    match t with
    | Unknown | Bottom -> Name_occurrences.empty
    | Ok wcfn ->
      WCFN.free_names_no_cache
        ~free_names_descr:(free_names_descr_payload ~free_names_head)
        wcfn

  let remove_unused_value_slots_and_shortcut_aliases_descr_payload
      ~remove_unused_value_slots_and_shortcut_aliases_head t ~used_value_slots
      ~canonicalise =
    match t with
    | Equals alias ->
      let canonical = canonicalise alias in
      if alias == canonical then t else Equals canonical
    | No_alias head ->
      let head' =
        remove_unused_value_slots_and_shortcut_aliases_head head
          ~used_value_slots ~canonicalise
      in
      if head == head' then t else No_alias head'

  let remove_unused_value_slots_and_shortcut_aliases ~apply_renaming_head
      ~remove_unused_value_slots_and_shortcut_aliases_head (t : _ t)
      ~used_value_slots ~canonicalise : _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok wcfn ->
      let wcfn' =
        WCFN.remove_unused_value_slots_and_shortcut_aliases
          ~apply_renaming_descr:
            (apply_renaming_descr_payload ~apply_renaming_head)
          ~remove_unused_value_slots_and_shortcut_aliases_descr:
            (remove_unused_value_slots_and_shortcut_aliases_descr_payload
               ~remove_unused_value_slots_and_shortcut_aliases_head)
          wcfn ~used_value_slots ~canonicalise
      in
      if wcfn == wcfn' then t else Ok wcfn'

  type ('head, 'descr) project_payload_result =
    | Unchanged
    | Changed of 'head descr_payload
    | Expanded of 'descr

  let project_variables_out_descr_payload ~to_project ~expand ~project_head t =
    match t with
    | Equals simple ->
      Simple.pattern_match' simple
        ~const:(fun _ -> Unchanged)
        ~symbol:(fun symbol ~coercion ->
          if Coercion.is_id coercion
          then Unchanged
          else
            (* Coercions might contain variables. Removing any coercion happens
               to fix all potential problems. *)
            Changed (Equals (Simple.symbol symbol)))
        ~var:(fun var ~coercion ->
          if Variable.Set.mem var to_project
          then Expanded (expand var ~coercion)
          else Unchanged)
    | No_alias head ->
      let head' = project_head head in
      if head == head' then Unchanged else Changed (No_alias head')

  let project_variables_out ~apply_renaming_head ~free_names_head ~to_project
      ~expand ~project_head (t : _ t) : _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok wcfn -> (
      let free_names =
        WCFN.free_names
          ~free_names_descr:(free_names_descr_payload ~free_names_head)
          wcfn
      in
      let has_variable_to_project =
        Variable.Set.fold
          (fun var acc -> acc || Name_occurrences.mem_var free_names var)
          to_project false
      in
      if not has_variable_to_project
      then t
      else
        let payload =
          WCFN.descr
            ~apply_renaming_descr:
              (apply_renaming_descr_payload ~apply_renaming_head)
            wcfn
        in
        match
          project_variables_out_descr_payload ~to_project ~expand ~project_head
            payload
        with
        | Unchanged -> t
        | Changed payload' -> Ok (WCFN.create payload')
        | Expanded t' -> t')
end

include T

let print ~apply_renaming_head ~print_head ppf t =
  Or_unknown_or_bottom.print (Descr.print ~print_head) ppf
    (descr ~apply_renaming_head t)

let[@inline always] apply_coercion ~apply_renaming_head ~apply_coercion_head
    coercion t : _ t Or_bottom.t =
  match descr ~apply_renaming_head t with
  | Unknown | Bottom -> Ok t
  | Ok (Equals simple) -> (
    match Simple.apply_coercion simple coercion with
    | None -> Bottom
    | Some simple -> Ok (create_equals simple))
  | Ok (No_alias head) ->
    let<+ head = apply_coercion_head head coercion in
    create head

let ids_for_export ~apply_renaming_head ~ids_for_export_head (t : _ t) =
  match descr ~apply_renaming_head t with
  | Unknown | Bottom -> Ids_for_export.empty
  | Ok (No_alias head) -> ids_for_export_head head
  | Ok (Equals simple) -> Ids_for_export.from_simple simple
