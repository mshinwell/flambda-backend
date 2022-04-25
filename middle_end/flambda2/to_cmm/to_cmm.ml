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

module Env = To_cmm_env
module R = To_cmm_result

(* Notes:

   - an int64 on a 32-bit host is represented across two registers, hence most
   operations on them will actually need to call C primitive that can handle
   them.

   - int32 on 64 bits are represented as an int64 in the range of 32-bit
   integers. Thus we insert sign extensions after every operation on 32-bits
   integers that may have a result outside of the range. *)

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Get constant definitions from Cmmgen_state. (The To_cmm translation uses
   functions from Cmm_helpers which populate some mutable state in
   Cmmgen_state.) *)

let flush_cmm_helpers_state () =
  let aux name cst acc =
    match (cst : Cmmgen_state.constant) with
    | Const_table (Local, l) ->
      C.cdata (C.define_symbol ~global:false name @ l) :: acc
    | Const_table (Global, l) ->
      C.cdata (C.define_symbol ~global:true name @ l) :: acc
    | Const_closure _ ->
      Misc.fatal_errorf
        "There shouldn't be any closure in cmmgen_state during flambda to cmm \
         translation"
  in
  match Cmmgen_state.get_and_clear_data_items () with
  | [] ->
    let cst_map = Cmmgen_state.get_and_clear_constants () in
    Misc.Stdlib.String.Map.fold aux cst_map []
  | _ ->
    Misc.fatal_errorf
      "There shouldn't be any data item in cmmgen_state during flambda to cmm \
       translation"

(* Note about the root symbol: it does not need any particular treatment.
   Concerning gc_roots, it's like any other statically allocated symbol: if it
   has an associated computation, then it will already be included in the list
   of gc_roots; otherwise it does not *have* to be a root. *)

let unit0 ~offsets ~make_symbol flambda_unit ~all_code =
  let dummy_k = Continuation.create () in
  (* The dummy continuation is passed here since we're going to manually arrange
     that the return continuation turns into "return unit". (Module initialisers
     return the unit value). *)
  let env =
    Env.create offsets all_code dummy_k
      ~exn_continuation:(Flambda_unit.exn_continuation flambda_unit)
  in
  let _env, return_cont_params =
    (* The environment is dropped because the handler for the dummy continuation
       (which just returns unit) doesn't use any of the parameters. *)
    C.bound_parameters env
      (Bound_parameters.create
         [ Bound_parameter.create (Variable.create "*ret*")
             Flambda_kind.With_subkind.any_value ])
  in
  let return_cont, env =
    Env.add_jump_cont env
      (List.map snd return_cont_params)
      (Flambda_unit.return_continuation flambda_unit)
  in
  let r = R.empty ~module_symbol:(Flambda_unit.module_symbol flambda_unit) in
  let body, res = To_cmm_expr.expr env r (Flambda_unit.body flambda_unit) in
  let body =
    let dbg = Debuginfo.none in
    let unit_value = C.targetint ~dbg Targetint_32_64.one in
    C.create_ccatch ~rec_flag:false ~body
      ~handlers:[C.handler ~dbg return_cont return_cont_params unit_value]
  in
  let entry =
    let dbg = Debuginfo.none in
    let fun_name = Compilenv.make_symbol (Some "entry") in
    let fun_codegen =
      let fun_codegen = [Cmm.Reduce_code_size; Cmm.Use_linscan_regalloc] in
      if Flambda_features.backend_cse_at_toplevel ()
      then fun_codegen
      else Cmm.No_CSE :: fun_codegen
    in
    C.cfunction (C.fundecl fun_name [] body fun_codegen dbg)
  in
  let data, gc_roots, functions = R.to_cmm res in
  let cmm_helpers_data = flush_cmm_helpers_state () in
  let gc_root_data =
    C.gc_root_table ~make_symbol
      (List.map
         (fun sym -> Linkage_name.to_string (Symbol.linkage_name sym))
         gc_roots)
  in
  (gc_root_data :: data) @ cmm_helpers_data @ functions @ [entry]

let unit ~offsets ~make_symbol flambda_unit ~all_code =
  Profile.record_call "flambda_to_cmm" (fun () ->
      unit0 ~offsets ~make_symbol flambda_unit ~all_code)
