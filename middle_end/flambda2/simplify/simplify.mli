(****************************************************************************)
(* This file forms part of OxCaml, https://github.com/oxcaml/oxcaml/        *)
(* Please see the CONTRIBUTORS and LICENSING files at the root of the repo. *)
(* SPDX-License-Identifier: MIT                                             *)
(****************************************************************************)

(** Simplification of Flambda programs: inlining, specialisation, unboxing and
    so forth.

    Readers interested in the function inlining strategy should read the
    [Inlining_decision] module first. *)

type simplify_result = private
  { free_names : Name_occurrences.t;
    final_typing_env : Typing_env.t option;
    all_code : Exported_code.t;
    slot_offsets : Slot_offsets.t;
    unit : Flambda_unit.t
  }

val run :
  cmx_loader:Flambda_cmx.loader ->
  round:int ->
  code_slot_offsets:Slot_offsets.t Code_id.Map.t ->
  Flambda_unit.t ->
  simplify_result
