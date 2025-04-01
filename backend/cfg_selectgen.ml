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

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Cmm
open Select_utils
module DLL = Flambda_backend_utils.Doubly_linked_list
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

module Or_never_returns = struct
  type 'a t =
    | Ok of 'a
    | Never_returns
end

let debug = false

let float_test_of_float_comparison :
    Cmm.float_width ->
    Cmm.float_comparison ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.float_test =
 fun width comparison ~label_false ~label_true ->
  let lt, eq, gt, uo =
    match comparison with
    | CFeq -> label_false, label_true, label_false, label_false
    | CFneq -> label_true, label_false, label_true, label_true
    | CFlt -> label_true, label_false, label_false, label_false
    | CFnlt -> label_false, label_true, label_true, label_true
    | CFgt -> label_false, label_false, label_true, label_false
    | CFngt -> label_true, label_true, label_false, label_true
    | CFle -> label_true, label_true, label_false, label_false
    | CFnle -> label_false, label_false, label_true, label_true
    | CFge -> label_false, label_true, label_true, label_false
    | CFnge -> label_true, label_false, label_false, label_true
  in
  { width; lt; eq; gt; uo }

let int_test_of_integer_comparison :
    Cmm.integer_comparison ->
    signed:bool ->
    immediate:int option ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.int_test =
 fun comparison ~signed:is_signed ~immediate:imm ~label_false ~label_true ->
  let lt, eq, gt =
    match comparison with
    | Ceq -> label_false, label_true, label_false
    | Cne -> label_true, label_false, label_true
    | Clt -> label_true, label_false, label_false
    | Cgt -> label_false, label_false, label_true
    | Cle -> label_true, label_true, label_false
    | Cge -> label_false, label_true, label_true
  in
  { lt; eq; gt; is_signed; imm }

let terminator_of_test :
    Simple_operation.test ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.terminator =
 fun test ~label_false ~label_true ->
  let int_test comparison immediate =
    let signed, comparison =
      match comparison with
      | Simple_operation.Isigned comparison -> true, comparison
      | Simple_operation.Iunsigned comparison -> false, comparison
    in
    int_test_of_integer_comparison comparison ~signed ~immediate ~label_false
      ~label_true
  in
  match test with
  | Itruetest -> Truth_test { ifso = label_true; ifnot = label_false }
  | Ifalsetest -> Truth_test { ifso = label_false; ifnot = label_true }
  | Iinttest comparison -> Int_test (int_test comparison None)
  | Iinttest_imm (comparison, value) ->
    Int_test (int_test comparison (Some value))
  | Ifloattest (w, comparison) ->
    Float_test
      (float_test_of_float_comparison w comparison ~label_false ~label_true)
  | Ioddtest -> Parity_test { ifso = label_false; ifnot = label_true }
  | Ieventest -> Parity_test { ifso = label_true; ifnot = label_false }

let invalid_stack_offset = -1

module Stack_offset_and_exn = struct
  (* This module relies on the field `can_raise` of basic blocks but does not
     rely on this field of individual Cfg instructions. This may need to be
     revisited when we remove dead trap handlers and the associated
     pushtrap/poptrap operations. *)
  type handler_stack = Label.t list

  let compute_stack_offset ~stack_offset ~traps =
    stack_offset + (Proc.trap_size_in_bytes * List.length traps)

  let check_and_set_stack_offset :
      'a Cfg.instruction -> stack_offset:int -> traps:handler_stack -> unit =
   fun instr ~stack_offset ~traps ->
    assert (instr.stack_offset = invalid_stack_offset);
    Cfg.set_stack_offset instr (compute_stack_offset ~stack_offset ~traps)

  let process_terminator :
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.terminator Cfg.instruction ->
      int * handler_stack =
   fun ~stack_offset ~traps term ->
    check_and_set_stack_offset term ~stack_offset ~traps;
    match term.desc with
    | Tailcall_self _
      when stack_offset <> 0 || List.compare_length_with traps 0 <> 0 ->
      Misc.fatal_errorf
        "Cfgize.Stack_offset_and_exn.process_terminator: unexpected handler on \
         self tailcall (id=%a)"
        InstructionId.format term.id
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Call _ | Prim _
    | Specific_can_raise _ ->
      stack_offset, traps

  let rec process_basic :
      Cfg.t ->
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.basic Cfg.instruction ->
      int * handler_stack =
   fun cfg ~stack_offset ~traps instr ->
    check_and_set_stack_offset instr ~stack_offset ~traps;
    match instr.desc with
    | Pushtrap { lbl_handler } ->
      update_block cfg lbl_handler ~stack_offset ~traps;
      stack_offset, lbl_handler :: traps
    | Poptrap -> (
      match traps with
      | [] ->
        Misc.fatal_errorf
          "Cfgize.Stack_offset_and_exn.process_basic: trying to pop from an \
           empty stack (id=%a)"
          InstructionId.format instr.id
      | _ :: traps -> stack_offset, traps)
    | Op (Stackoffset n) -> stack_offset + n, traps
    | Op
        ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
        | Const_symbol _ | Const_vec128 _ | Load _ | Store _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _
        | Reinterpret_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll
        | Alloc _ )
    | Reloadretaddr | Prologue ->
      stack_offset, traps
    | Stack_check _ ->
      Misc.fatal_error
        "Cfgize.Stack_offset_and_exn.process_basic: unexpected stack check"

  (* The argument [stack_offset] has a different meaning from the field
     [stack_offset] of Cfg's basic_blocks and instructions. The argument
     [stack_offset] refers to the offset derived from Istackoffset operations
     only, whereas the field also includes the trap stack depth in bytes. Both
     are non-negative. See [compute_stack_offset]. *)
  and update_block :
      Cfg.t -> Label.t -> stack_offset:int -> traps:handler_stack -> unit =
   fun cfg label ~stack_offset ~traps ->
    let block = Cfg.get_block_exn cfg label in
    let was_invalid =
      if block.stack_offset = invalid_stack_offset
      then true
      else (
        if debug
        then
          assert (block.stack_offset = compute_stack_offset ~stack_offset ~traps);
        false)
    in
    if was_invalid
    then (
      block.stack_offset <- compute_stack_offset ~stack_offset ~traps;
      let stack_offset, traps =
        DLL.fold_left block.body ~init:(stack_offset, traps)
          ~f:(fun (stack_offset, traps) instr ->
            process_basic cfg ~stack_offset ~traps instr)
      in
      let stack_offset, traps =
        process_terminator ~stack_offset ~traps block.terminator
      in
      (* non-exceptional successors *)
      Label.Set.iter
        (update_block cfg ~stack_offset ~traps)
        (Cfg.successor_labels ~normal:true ~exn:false block);
      (* exceptional successor *)
      if block.can_raise
      then (
        assert (Option.is_none block.exn);
        match traps with
        | [] -> ()
        | handler_label :: _ -> block.exn <- Some handler_label))

  let update_cfg : Cfg.t -> unit =
   fun cfg ->
    update_block cfg cfg.entry_label ~stack_offset:0 ~traps:[];
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        if block.stack_offset = invalid_stack_offset then block.dead <- true;
        assert (not (block.is_trap_handler && block.dead)))
end

let make_store mem_chunk addr_mode is_assignment =
  Cfg.Op (Operation.Store (mem_chunk, addr_mode, is_assignment))

let make_stack_offset stack_ofs = Cfg.Op (Stackoffset stack_ofs)

let make_name_for_debugger ~ident ~which_parameter ~provenance ~is_assignment
    ~regs =
  Cfg.Op
    (Operation.Name_for_debugger
       { ident; which_parameter; provenance; is_assignment; regs })

let make_const_int x = Operation.Const_int x

let make_const_float32 x = Operation.Const_float32 x

let make_const_float x = Operation.Const_float x

let make_const_vec128 x = Operation.Const_vec128 x

let make_const_symbol x = Operation.Const_symbol x

let make_opaque () = Operation.Opaque
(* Return an array of fresh registers of the given type. Normally implemented as
   Reg.createv, but some ports (e.g. Arm) can override this definition to store
   float values in pairs of integer registers. *)

let regs_for tys = Reg.createv tys

let basic_op x = Basic (Op x)

let insert_debug (_env : environment) sub_cfg basic dbg arg res =
  Sub_cfg.add_instruction sub_cfg basic arg res dbg

let insert_op_debug_returning_id (_env : environment) sub_cfg op dbg arg res =
  let instr = Cfg.make_instr (Cfg.Op op) arg res dbg in
  Sub_cfg.add_instruction' sub_cfg instr;
  instr.id

let insert (_env : environment) sub_cfg basic arg res =
  (* CR mshinwell: fix debuginfo *)
  Sub_cfg.add_instruction sub_cfg basic arg res Debuginfo.none

let insert' (_env : environment) sub_cfg term arg res =
  (* CR mshinwell: fix debuginfo *)
  Sub_cfg.set_terminator sub_cfg term arg res Debuginfo.none

let insert_debug' (_env : environment) sub_cfg basic dbg arg res =
  Sub_cfg.set_terminator sub_cfg basic arg res dbg

let insert_op_debug' (_env : environment) sub_cfg op dbg rs rd =
  Sub_cfg.set_terminator sub_cfg op rs rd dbg;
  rd

let insert_move env sub_cfg src dst =
  if src.Reg.stamp <> dst.Reg.stamp
  then insert env sub_cfg (Op Move) [| src |] [| dst |]

let insert_moves env sub_cfg src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    insert_move env sub_cfg src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

let insert_move_args env sub_cfg arg loc stacksize =
  if stacksize <> 0
  then insert env sub_cfg (make_stack_offset stacksize) [||] [||];
  insert_moves env sub_cfg arg loc

let insert_move_results env sub_cfg loc res stacksize =
  insert_moves env sub_cfg loc res;
  if stacksize <> 0
  then insert env sub_cfg (make_stack_offset (-stacksize)) [||] [||]

let insert_move_extcall_arg env _ty_arg src dst =
  (* The default implementation is one or two ordinary moves. (Two in the case
     of an int64 argument on a 32-bit platform.) It can be overridden to use
     special move instructions, for example a "32-bit move" instruction for
     int32 arguments. *)
  insert_moves env src dst

(* Add an Iop opcode. Can be overridden by processor description to insert moves
   before and after the operation, i.e. for two-address instructions, or
   instructions using dedicated registers. *)

let insert_op_debug env sub_cfg op dbg rs rd =
  insert_debug env sub_cfg (Op op) dbg rs rd;
  rd

let insert_op env sub_cfg op rs rd =
  insert_op_debug env sub_cfg op Debuginfo.none rs rd
(* CR xclerc for xclerc: maybe_emit_naming_op, join, and join_array below should
   be shared with the ones in Select_utils. *)

let maybe_emit_naming_op env sub_cfg ~bound_name regs =
  match bound_name with
  | None -> ()
  | Some bound_name ->
    let provenance = Backend_var.With_provenance.provenance bound_name in
    if Option.is_some provenance
    then
      let bound_name = Backend_var.With_provenance.var bound_name in
      let naming_op =
        Operation.Name_for_debugger
          { ident = bound_name;
            provenance;
            which_parameter = None;
            is_assignment = false;
            regs
          }
      in
      insert_debug env sub_cfg (Cfg.Op naming_op) Debuginfo.none [||] [||]

let join env (branch1 : _ Or_never_returns.t) (branch2 : _ Or_never_returns.t)
    ~bound_name : (Reg.t array * Sub_cfg.t * Sub_cfg.t) Or_never_returns.t =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  match branch1, branch2 with
  | Never_returns, Never_returns -> Never_returns
  | Never_returns, Ok (regs, sub_cfg) ->
    Ok (regs, Sub_cfg.make_empty (), sub_cfg)
  | Ok (regs, sub_cfg), Never_returns ->
    Ok (regs, sub_cfg, Sub_cfg.make_empty ())
  | Ok (r1, sub_cfg1), Ok (r2, sub_cfg2) ->
    let l1 = Array.length r1 in
    assert (l1 = Array.length r2);
    let r = Array.make l1 Reg.dummy in
    for i = 0 to l1 - 1 do
      if Reg.anonymous r1.(i) && Cmm.ge_component r1.(i).Reg.typ r2.(i).Reg.typ
      then (
        r.(i) <- r1.(i);
        insert_move env sub_cfg2 r2.(i) r1.(i);
        maybe_emit_naming_op sub_cfg2 [| r1.(i) |])
      else if Reg.anonymous r2.(i)
              && Cmm.ge_component r2.(i).Reg.typ r1.(i).Reg.typ
      then (
        r.(i) <- r2.(i);
        insert_move env sub_cfg1 r1.(i) r2.(i);
        maybe_emit_naming_op sub_cfg1 [| r2.(i) |])
      else
        let typ = Cmm.lub_component r1.(i).Reg.typ r2.(i).Reg.typ in
        r.(i) <- Reg.create typ;
        insert_move env sub_cfg1 r1.(i) r.(i);
        maybe_emit_naming_op sub_cfg1 [| r.(i) |];
        insert_move env sub_cfg2 r2.(i) r.(i);
        maybe_emit_naming_op sub_cfg2 [| r.(i) |]
    done;
    Ok (r, sub_cfg1, sub_cfg2)

let join_array env
    (rs_and_sub_cfgs : (Reg.t array * Sub_cfg.t) Or_never_returns.t array)
    ~bound_name : (Reg.t array * Sub_cfg.t array) Or_never_returns.t =
  let maybe_emit_naming_op sub_cfg =
    maybe_emit_naming_op env sub_cfg ~bound_name
  in
  let some_res = ref (Never_returns : _ Or_never_returns.t) in
  let sub_cfgs_rev = ref [] in
  for i = 0 to Array.length rs_and_sub_cfgs - 1 do
    match rs_and_sub_cfgs.(i) with
    | Never_returns -> sub_cfgs_rev := Sub_cfg.make_empty () :: !sub_cfgs_rev
    | Ok (r, sub_cfg) -> (
      sub_cfgs_rev := sub_cfg :: !sub_cfgs_rev;
      match !some_res with
      | Never_returns ->
        some_res := Or_never_returns.Ok (r, Array.map (fun r -> r.Reg.typ) r)
      | Ok (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.Reg.typ typ) r types
        in
        some_res := Or_never_returns.Ok (r', types))
  done;
  let sub_cfgs = List.rev !sub_cfgs_rev |> Array.of_list in
  match !some_res with
  | Never_returns -> Never_returns
  | Ok (template, types) ->
    let size_res = Array.length template in
    let res = Array.make size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.create types.(i)
    done;
    for i = 0 to Array.length rs_and_sub_cfgs - 1 do
      match rs_and_sub_cfgs.(i) with
      | Never_returns -> ()
      | Ok (r, sub_cfg) ->
        insert_moves env sub_cfg r res;
        maybe_emit_naming_op sub_cfg res
    done;
    Ok (res, sub_cfgs)

module Make (Target : sig
  val is_immediate : int -> bool

  val select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth

  val is_immediate : 'a -> 'b -> Select_utils.is_immediate_result

  val is_immediate_test :
    Simple_operation.integer_comparison ->
    int ->
    Select_utils.is_immediate_result

  val is_simple_expr : Cmm.expression -> Select_utils.is_simple_expr_result

  val effects_of : Cmm.expression -> Select_utils.effects_of_result

  val select_addressing :
    Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

  val select_operation :
    Cmm.operation ->
    Cmm.expression list ->
    label_after:Label.t ->
    Select_utils.select_operation_result

  val select_store :
    is_assign:bool ->
    Arch.addressing_mode ->
    Cmm.expression ->
    Select_utils.select_store_result

  val insert_move_extcall_arg :
    Cmm.exttype ->
    Reg.t array ->
    Reg.t array ->
    Select_utils.insert_move_extcall_arg_result
end) =
struct
  (* XXX BEGIN from select_utils *)

  (* A syntactic criterion used in addition to judgements about (co)effects as
     to whether the evaluation of a given expression may be deferred by
     [emit_parts]. This criterion is a property of the instruction selection
     algorithm in this file rather than a property of the Cmm language. *)
  let rec is_simple_expr0 (expr : Cmm.expression) =
    match expr with
    | Cconst_int _ -> true
    | Cconst_natint _ -> true
    | Cconst_float32 _ -> true
    | Cconst_float _ -> true
    | Cconst_symbol _ -> true
    | Cconst_vec128 _ -> true
    | Cvar _ -> true
    | Ctuple el -> List.for_all is_simple_expr el
    | Clet (_id, arg, body) -> is_simple_expr arg && is_simple_expr body
    | Cphantom_let (_var, _defining_expr, body) -> is_simple_expr body
    | Csequence (e1, e2) -> is_simple_expr e1 && is_simple_expr e2
    | Cop (op, args, _) -> (
      match op with
      (* Cextcall with neither effects nor coeffects is simple if its arguments
         are *)
      | Cextcall { effects = No_effects; coeffects = No_coeffects } ->
        List.for_all is_simple_expr args
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc _ | Cstore _ | Craise _ | Catomic _
      | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cpoll ->
        false
      | Cprefetch _ | Cbeginregion | Cendregion ->
        false
        (* avoid reordering *)
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
      | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccsel _ | Cabsf _ | Caddf _
      | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32 | Creinterpret_cast _
      | Cstatic_cast _ | Ctuple_field _ | Ccmpf _ | Cdls_get ->
        List.for_all is_simple_expr args)
    | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ -> false

  and is_simple_expr expr =
    match Target.is_simple_expr expr with
    | Simple_if_all_expressions_are exprs -> List.for_all is_simple_expr exprs
    | Use_default -> is_simple_expr0 expr

  (* Analyses the effects and coeffects of an expression. This is used across a
     whole list of expressions with a view to determining which expressions may
     have their evaluation deferred. The result of this function, modulo
     target-specific judgements if the [effects_of] val is overridden, is a
     property of the Cmm language rather than anything particular about the
     instruction selection algorithm in this file.

     In the case of e.g. an OCaml function call, the arguments whose evaluation
     cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
     order first with their results going into temporaries, then the block is
     allocated, then the remaining arguments are evaluated before being combined
     with the temporaries. *)
  let rec effects_of0 exp =
    let module EC = Select_utils.Effect_and_coeffect in
    match exp with
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ ->
      EC.none
    | Ctuple el -> EC.join_list_map el effects_of
    | Clet (_id, arg, body) -> EC.join (effects_of arg) (effects_of body)
    | Cphantom_let (_var, _defining_expr, body) -> effects_of body
    | Csequence (e1, e2) -> EC.join (effects_of e1) (effects_of e2)
    | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _kind) ->
      EC.join (effects_of cond) (EC.join (effects_of ifso) (effects_of ifnot))
    | Cop (op, args, _) ->
      let from_op =
        match op with
        | Cextcall { effects = e; coeffects = ce } ->
          EC.create (select_effects e) (select_coeffects ce)
        | Capply _ | Cprobe _ | Copaque | Cpoll -> EC.arbitrary
        | Calloc (Heap, _) -> EC.none
        | Calloc (Local, _) -> EC.coeffect_only Coeffect.Arbitrary
        | Cstore _ -> EC.effect_only Effect.Arbitrary
        | Cbeginregion | Cendregion -> EC.arbitrary
        | Cprefetch _ -> EC.arbitrary
        | Catomic _ -> EC.arbitrary
        | Craise _ -> EC.effect_only Effect.Raise
        | Cload { mutability = Asttypes.Immutable } -> EC.none
        | Cload { mutability = Asttypes.Mutable } | Cdls_get ->
          EC.coeffect_only Coeffect.Read_mutable
        | Cprobe_is_enabled _ -> EC.coeffect_only Coeffect.Arbitrary
        | Ctuple_field _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
        | Cand | Cor | Cxor | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cpopcnt
        | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
        | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
        | Creinterpret_cast _ | Cstatic_cast _ | Ccmpf _ ->
          EC.none
      in
      EC.join from_op (EC.join_list_map args effects_of)
    | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ -> EC.arbitrary

  and effects_of (expr : Cmm.expression) =
    match Target.effects_of expr with
    | Effects_of_all_expressions exprs ->
      Select_utils.Effect_and_coeffect.join_list_map exprs effects_of
    | Use_default -> effects_of0 expr

  (* Says whether an integer constant is a suitable immediate argument for the
     given integer operation *)

  let is_immediate (op : Simple_operation.integer_operation) n =
    match Target.is_immediate op n with
    | Is_immediate result -> result
    | Use_default -> (
      match op with
      | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
      | _ -> false)

  (* Instruction selection for conditionals *)

  let is_immediate_test cmp n =
    match Target.is_immediate_test cmp n with
    | Is_immediate result -> result
    | Use_default -> is_immediate (Icomp cmp) n

  let select_condition (arg : Cmm.expression) :
      Simple_operation.test * Cmm.expression =
    match arg with
    | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
      when is_immediate_test (Isigned cmp) n ->
      Iinttest_imm (Isigned cmp, n), arg1
    | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Isigned (swap_integer_comparison cmp)) n ->
      Iinttest_imm (Isigned (swap_integer_comparison cmp), n), arg2
    | Cop (Ccmpi cmp, args, _) -> Iinttest (Isigned cmp), Ctuple args
    | Cop (Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
      when is_immediate_test (Iunsigned cmp) n ->
      Iinttest_imm (Iunsigned cmp, n), arg1
    | Cop (Ccmpa cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Iunsigned (swap_integer_comparison cmp)) n ->
      Iinttest_imm (Iunsigned (swap_integer_comparison cmp), n), arg2
    | Cop (Ccmpa cmp, args, _) -> Iinttest (Iunsigned cmp), Ctuple args
    | Cop (Ccmpf (width, cmp), args, _) -> Ifloattest (width, cmp), Ctuple args
    | Cop (Cand, [arg1; Cconst_int (1, _)], _) -> Ioddtest, arg1
    | _ -> Itruetest, arg

  let is_store (op : Operation.t) =
    match op with Store (_, _, _) -> true | _ -> false

  let lift_op op = Cfg.Op op

  let bind_let (env : Select_utils.environment) sub_cfg v r1 =
    let env =
      if all_regs_anonymous r1
      then (
        name_regs v r1;
        env_add v r1 env)
      else
        let rv = Reg.createv_like r1 in
        name_regs v rv;
        insert_moves env sub_cfg r1 rv;
        env_add v rv env
    in
    let provenance = VP.provenance v in
    (if Option.is_some provenance
    then
      let naming_op =
        make_name_for_debugger ~ident:(VP.var v) ~which_parameter:None
          ~provenance ~is_assignment:false ~regs:r1
      in
      insert_debug env sub_cfg naming_op Debuginfo.none [||] [||]);
    env

  let bind_let_mut env sub_cfg v k r1 =
    let rv = regs_for k in
    name_regs v rv;
    insert_moves env sub_cfg r1 rv;
    let provenance = VP.provenance v in
    (if Option.is_some provenance
    then
      let naming_op =
        make_name_for_debugger ~ident:(VP.var v) ~which_parameter:None
          ~provenance:(VP.provenance v) ~is_assignment:false ~regs:r1
      in
      insert_debug env sub_cfg naming_op Debuginfo.none [||] [||]);
    env_add ~mut:Mutable v rv env

  (* Default instruction selection for stores (of words) *)

  let select_store is_assign addr arg : Operation.t * Cmm.expression =
    Store (Word_val, addr, is_assign), arg

  (* Default instruction selection for integer operations *)

  let select_arith_comm (op : Simple_operation.integer_operation)
      (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate op n ->
      basic_op (Intop_imm (op, n)), [arg]
    | [Cconst_int (n, _); arg] when is_immediate op n ->
      basic_op (Intop_imm (op, n)), [arg]
    | _ -> basic_op (Intop op), args

  let select_arith (op : Simple_operation.integer_operation)
      (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate op n ->
      basic_op (Intop_imm (op, n)), [arg]
    | _ -> basic_op (Intop op), args

  let select_arith_comp (cmp : Simple_operation.integer_comparison)
      (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate (Icomp cmp) n ->
      basic_op (Intop_imm (Icomp cmp, n)), [arg]
    | [Cconst_int (n, _); arg]
      when is_immediate (Icomp (Select_utils.swap_intcomp cmp)) n ->
      basic_op (Intop_imm (Icomp (Select_utils.swap_intcomp cmp), n)), [arg]
    | _ -> basic_op (Intop (Icomp cmp)), args

  (* Default instruction selection for operators *)

  let select_operation0 (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      basic_or_terminator * Cmm.expression list =
    let wrong_num_args n =
      Misc.fatal_errorf
        "Selection.select_operation: expected %d argument(s) for@ %s" n
        (Printcmm.operation dbg op)
    in
    let[@inline] single_arg () =
      match args with [arg] -> arg | [] | _ :: _ -> wrong_num_args 1
    in
    let[@inline] two_args () =
      match args with
      | [arg1; arg2] -> arg1, arg2
      | [] | _ :: _ -> wrong_num_args 2
    in
    let[@inline] three_args () =
      match args with
      | [arg1; arg2; arg3] -> arg1, arg2, arg3
      | [] | _ :: _ -> wrong_num_args 3
    in
    match[@ocaml.warning "+fragile-match"] op with
    | Capply _ -> (
      match[@ocaml.warning "-fragile-match"] args with
      | Cconst_symbol (func, _dbg) :: rem ->
        Terminator (Call { op = Direct func; label_after }), rem
      | _ -> Terminator (Call { op = Indirect; label_after }), args)
    | Cextcall { func; builtin = true } ->
      Misc.fatal_errorf "Selection.select_operation: builtin not recognized %s"
        func ()
    | Cextcall
        { func; alloc; ty; ty_args; returns; builtin = false; effects; _ } ->
      let external_call =
        { Cfg.func_symbol = func;
          alloc;
          effects;
          ty_res = ty;
          ty_args;
          stack_ofs = -1
        }
      in
      if returns
      then Terminator (Prim { op = External external_call; label_after }), args
      else Terminator (Call_no_return external_call), args
    | Cload { memory_chunk; mutability; is_atomic } ->
      let arg = single_arg () in
      let addressing_mode, eloc = Target.select_addressing memory_chunk arg in
      let mutability = select_mutable_flag mutability in
      ( basic_op (Load { memory_chunk; addressing_mode; mutability; is_atomic }),
        [eloc] )
    | Cstore (chunk, init) -> (
      let arg1, arg2 = two_args () in
      let addr, eloc = Target.select_addressing chunk arg1 in
      let is_assign =
        match init with Initialization -> false | Assignment -> true
      in
      match[@ocaml.warning "-fragile-match"] chunk with
      | Word_int | Word_val ->
        let op, newarg2 = select_store is_assign addr arg2 in
        basic_op op, [newarg2; eloc]
      | _ -> basic_op (Store (chunk, addr, is_assign)), [arg2; eloc]
      (* Inversion addr/datum in Istore *))
    | Cdls_get -> basic_op Dls_get, args
    | Calloc (mode, alloc_block_kind) ->
      let placeholder_for_alloc_block_kind =
        { alloc_words = 0; alloc_block_kind; alloc_dbg = Debuginfo.none }
      in
      ( basic_op
          (Alloc
             { bytes = 0; dbginfo = [placeholder_for_alloc_block_kind]; mode }),
        args )
    | Cpoll -> basic_op Poll, args
    | Caddi -> select_arith_comm Iadd args
    | Csubi -> select_arith Isub args
    | Cmuli -> select_arith_comm Imul args
    | Cmulhi { signed } -> select_arith_comm (Imulh { signed }) args
    | Cdivi -> basic_op (Intop Idiv), args
    | Cmodi -> basic_op (Intop Imod), args
    | Cand -> select_arith_comm Iand args
    | Cor -> select_arith_comm Ior args
    | Cxor -> select_arith_comm Ixor args
    | Clsl -> select_arith Ilsl args
    | Clsr -> select_arith Ilsr args
    | Casr -> select_arith Iasr args
    | Cclz { arg_is_non_zero } ->
      basic_op (Intop (Iclz { arg_is_non_zero })), args
    | Cctz { arg_is_non_zero } ->
      basic_op (Intop (Ictz { arg_is_non_zero })), args
    | Cpopcnt -> basic_op (Intop Ipopcnt), args
    | Ccmpi comp -> select_arith_comp (Isigned comp) args
    | Caddv -> select_arith_comm Iadd args
    | Cadda -> select_arith_comm Iadd args
    | Ccmpa comp -> select_arith_comp (Iunsigned comp) args
    | Ccmpf (w, comp) -> basic_op (Floatop (w, Icompf comp)), args
    | Ccsel _ ->
      let cond, ifso, ifnot = three_args () in
      let cond, earg = select_condition cond in
      basic_op (Csel cond), [earg; ifso; ifnot]
    | Cnegf w -> basic_op (Floatop (w, Inegf)), args
    | Cabsf w -> basic_op (Floatop (w, Iabsf)), args
    | Caddf w -> basic_op (Floatop (w, Iaddf)), args
    | Csubf w -> basic_op (Floatop (w, Isubf)), args
    | Cmulf w -> basic_op (Floatop (w, Imulf)), args
    | Cdivf w -> basic_op (Floatop (w, Idivf)), args
    | Creinterpret_cast cast -> basic_op (Reinterpret_cast cast), args
    | Cstatic_cast cast -> basic_op (Static_cast cast), args
    | Catomic { op; size } -> (
      match op with
      | Exchange | Fetch_and_add | Add | Sub | Land | Lor | Lxor ->
        let src, dst = two_args () in
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        basic_op (Intop_atomic { op; size; addr }), [src; eloc]
      | Compare_set | Compare_exchange ->
        let compare_with, set_to, dst = three_args () in
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        basic_op (Intop_atomic { op; size; addr }), [compare_with; set_to; eloc]
      )
    | Cprobe { name; handler_code_sym; enabled_at_init } ->
      ( Terminator
          (Prim
             { op = Probe { name; handler_code_sym; enabled_at_init };
               label_after
             }),
        args )
    | Cprobe_is_enabled { name } -> basic_op (Probe_is_enabled { name }), []
    | Cbeginregion -> basic_op Begin_region, []
    | Cendregion -> basic_op End_region, args
    | Cpackf32 | Copaque | Cbswap _ | Cprefetch _ | Craise _
    | Ctuple_field (_, _) ->
      Misc.fatal_error "Selection.select_oper"

  let rec select_operation (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      basic_or_terminator * Cmm.expression list =
    match Target.select_operation op args ~label_after with
    | Rewritten (basic_or_terminator, args) -> basic_or_terminator, args
    | Select_operation_then_rewrite (op, args, dbg, rewriter) -> (
      let basic_or_terminator, args =
        select_operation op args dbg ~label_after
      in
      match rewriter basic_or_terminator ~args with
      | Rewritten (basic_or_terminator, args) -> basic_or_terminator, args
      | Use_default -> basic_or_terminator, args)
    | Use_default -> select_operation0 op args dbg ~label_after

  (* XXX put this in the env *)
  let tailrec_label = ref Label.none
  (* set in emit_fundecl *)

  let insert_return (env : environment) sub_cfg r (traps : trap_action list) =
    match r with
    | None -> ()
    | Some r ->
      List.iter
        (fun trap ->
          let instr_desc =
            match trap with
            | Cmm.Push _ -> Misc.fatal_error "unexpected push on trap actions"
            | Cmm.Pop _ -> Cfg.Poptrap
          in
          Sub_cfg.add_instruction sub_cfg instr_desc [||] [||] Debuginfo.none)
        traps;
      let loc = Proc.loc_results_return (Reg.typv r) in
      insert_moves env sub_cfg r loc;
      insert' env sub_cfg Cfg.Return loc [||]
  (* The following two functions, [emit_parts] and [emit_parts_list], force
     right-to-left evaluation order as required by the Flambda [Un_anf] pass
     (and to be consistent with the bytecode compiler). *)

  let rec emit_parts env sub_cfg ~effects_after exp :
      (Cmm.expression * Select_utils.environment * Sub_cfg.t) Or_never_returns.t
      =
    let module EC = Effect_and_coeffect in
    let may_defer_evaluation =
      let ec = effects_of exp in
      match EC.effect ec with
      | Arbitrary | Raise ->
        (* Preserve the ordering of effectful expressions by evaluating them
           early (in the correct order) and assigning their results to
           temporaries. We can avoid this in just one case: if we know that
           every [exp'] in the original expression list (cf. [emit_parts_list])
           to be evaluated after [exp] cannot possibly affect the result of
           [exp] or depend on the result of [exp], then [exp] may be deferred.
           (Checking purity here is not enough: we need to check copurity too to
           avoid e.g. moving mutable reads earlier than the raising of an
           exception.) *)
        EC.pure_and_copure effects_after
      | None -> (
        match EC.coeffect ec with
        | None ->
          (* Pure expressions may be moved. *)
          true
        | Read_mutable -> (
          (* Read-mutable expressions may only be deferred if evaluation of
             every [exp'] (for [exp'] as in the comment above) has no effects
             "worse" (in the sense of the ordering in [Effect.t]) than raising
             an exception. *)
          match EC.effect effects_after with
          | None | Raise -> true
          | Arbitrary -> false)
        | Arbitrary -> (
          (* Arbitrary expressions may only be deferred if evaluation of every
             [exp'] (for [exp'] as in the comment above) has no effects. *)
          match EC.effect effects_after with
          | None -> true
          | Arbitrary | Raise -> false))
    in
    (* Even though some expressions may look like they can be deferred from the
       (co)effect analysis, it may be forbidden to move them. *)
    if may_defer_evaluation && is_simple_expr exp
    then Ok (exp, env, sub_cfg)
    else
      match emit_expr env sub_cfg exp ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok (r, sub_cfg) ->
        if Array.length r = 0
        then Ok (Ctuple [], env, sub_cfg)
        else
          (* The normal case *)
          let id = V.create_local "bind" in
          if all_regs_anonymous r
          then
            (* r is an anonymous, unshared register; use it directly *)
            Ok (Cvar id, env_add (VP.create id) r env, sub_cfg)
          else
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            insert_moves env sub_cfg r tmp;
            Ok (Cvar id, env_add (VP.create id) tmp env, sub_cfg)

  and emit_parts_list env sub_cfg exp_list :
      (expression list * environment * Sub_cfg.t) Or_never_returns.t =
    let module EC = Effect_and_coeffect in
    let exp_list_right_to_left, _effect =
      (* Annotate each expression with the (co)effects that happen after it when
         the original expression list is evaluated from right to left. The
         resulting expression list has the rightmost expression first. *)
      List.fold_left
        (fun (exp_list, effects_after) exp ->
          let exp_effect = effects_of exp in
          (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
        ([], EC.none) exp_list
    in
    List.fold_left
      (fun (results_and_env : _ Or_never_returns.t) (exp, effects_after) :
           _ Or_never_returns.t ->
        match results_and_env with
        | Never_returns -> Never_returns
        | Ok (result, env, sub_cfg) -> (
          match emit_parts env sub_cfg exp ~effects_after with
          | Never_returns -> Never_returns
          | Ok (exp_result, env, sub_cfg) ->
            Ok (exp_result :: result, env, sub_cfg)))
      (Or_never_returns.Ok ([], env, sub_cfg))
      exp_list_right_to_left

  and emit_tuple_not_flattened env sub_cfg exp_list =
    let rec emit_list = function
      | [] -> []
      | exp :: rem -> (
        (* Again, force right-to-left evaluation *)
        let loc_rem = emit_list rem in
        match emit_expr env sub_cfg exp ~bound_name:None with
        | Never_returns ->
          assert false (* should have been caught in emit_parts *)
        | Ok loc_exp -> loc_exp :: loc_rem)
    in
    emit_list exp_list

  and emit_tuple env sub_cfg exp_list :
      (Reg.t array * Sub_cfg.t) Or_never_returns.t =
    (* XXX *)
    assert false
  (* Array.concat (emit_tuple_not_flattened env sub_cfg exp_list) *)

  and emit_extcall_args env sub_cfg ty_args args =
    let args = emit_tuple_not_flattened env sub_cfg args in
    let ty_args =
      match ty_args with
      | [] -> List.map (fun _ -> XInt) args
      | _ :: _ -> ty_args
    in
    let locs, stack_ofs = Proc.loc_external_arguments ty_args in
    let ty_args = Array.of_list ty_args in
    if stack_ofs <> 0
    then insert env sub_cfg (make_stack_offset stack_ofs) [||] [||];
    List.iteri
      (fun i arg ->
        insert_move_extcall_arg env sub_cfg ty_args.(i) arg locs.(i))
      args;
    Array.concat (Array.to_list locs), stack_ofs

  and emit_stores env sub_cfg dbg args regs_addr =
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int))
    in
    let byte_offset = ref 0 in
    let base =
      assert (Array.length regs_addr = 1);
      ref regs_addr
    in
    let for_one_arg arg =
      let arg : Cmm.expression =
        match select_store ~is_assign:false !addressing_mode arg with
        | Maybe_out_of_range -> arg
        | Rewritten (_, arg) -> arg
        | Use_default -> Store (Word_val, addr, is_assign), arg
      in
      match emit_expr env sub_cfg arg ~bound_name:None with
      | Ok (regs, sub_cfg) -> (
        let operation_replacing_store =
          match select_store_result with
          | Maybe_out_of_range -> None
          | Rewritten (op, _) -> if is_store op then None else Some op
          | Use_default -> None (* see above *)
        in
        match operation_replacing_store with
        | None ->
          for i = 0 to Array.length regs - 1 do
            let r = regs.(i) in
            let kind =
              match r.Reg.typ with
              | Float -> Double
              | Float32 -> Single { reg = Float32 }
              | Vec128 ->
                (* 128-bit memory operations are default unaligned. Aligned
                   (big)array operations are handled separately via cmm. *)
                Onetwentyeight_unaligned
              | Val | Addr | Int -> Word_val
              | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
            in
            let is_out_of_range : Select_utils.is_store_out_of_range_result =
              match select_store_result with
              | Operation _ | Use_default -> Within_range
              | Maybe_out_of_range ->
                Target.is_store_out_of_range kind ~byte_offset:!byte_offset
            in
            let new_addressing_mode =
              match is_out_of_range with
              | Within_range -> !addressing_mode
              | Out_of_range ->
                (* Use a temporary to store the address [!base + offset]. *)
                let tmp = regs_for Cmm.typ_int in
                insert_debug env sub_cfg
                  (Op (make_const_int (Nativeint.of_int !byte_offset)))
                  dbg [||] tmp;
                insert_debug env sub_cfg (Op (Operation.Intop Iadd)) dbg
                  (Array.append !base tmp) tmp;
                (* Use the temporary as the new base address. *)
                base := tmp;
                Arch.identity_addressing
            in
            insert_debug env sub_cfg
              (Op (Store (kind, new_addressing_mode, false)))
              dbg
              (Array.append [| r |] regs_addr)
              [||];
            let size = Select_utils.size_component r.Reg.typ in
            addressing_mode := Arch.offset_addressing new_addressing_mode size;
            byte_offset := !byte_offset + size
          done
        | Some op ->
          insert_debug env sub_cfg (Op op) dbg (Array.append regs regs_addr) [||];
          let size = size_expr env arg in
          addressing_mode := Arch.offset_addressing !addressing_mode size;
          byte_offset := !byte_offset + size)
      | Never_returns ->
        Misc.fatal_error
          "emit_expr did not return any registers in [emit_stores]"
    in
    List.iter for_one_arg args

  (* Emit an expression.

     [bound_name] is the name that will be bound to the result of evaluating the
     expression, if such exists. This is used for emitting debugging info.

     Returns:

     - [None] if the expression does not finish normally (e.g. raises)

     - [Some rs] if the expression yields a result in registers [rs] *)
  and emit_expr env sub_cfg exp ~bound_name :
      (Reg.t array * Sub_cfg.t) Or_never_returns.t =
    match exp with
    | Cconst_int (n, _dbg) ->
      let r = regs_for typ_int in
      Ok
        ( insert_op env sub_cfg (make_const_int (Nativeint.of_int n)) [||] r,
          sub_cfg )
    | Cconst_natint (n, _dbg) ->
      let r = regs_for typ_int in
      Ok (insert_op env sub_cfg (make_const_int n) [||] r, sub_cfg)
    | Cconst_float32 (n, _dbg) ->
      let r = regs_for typ_float32 in
      Ok
        ( insert_op env sub_cfg
            (make_const_float32 (Int32.bits_of_float n))
            [||] r,
          sub_cfg )
    | Cconst_float (n, _dbg) ->
      let r = regs_for typ_float in
      Ok
        ( insert_op env sub_cfg (make_const_float (Int64.bits_of_float n)) [||] r,
          sub_cfg )
    | Cconst_vec128 (bits, _dbg) ->
      let r = regs_for typ_vec128 in
      Ok (insert_op env sub_cfg (make_const_vec128 bits) [||] r, sub_cfg)
    | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of which
         may point to heap values. However, any such blocks will be registered
         in the compilation unit's global roots structure, so adding this
         register to the frame table would be redundant *)
      let r = regs_for typ_int in
      Ok (insert_op env sub_cfg (make_const_symbol n) [||] r, sub_cfg)
    | Cvar v -> (
      try Ok (env_find v env, sub_cfg)
      with Not_found ->
        Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.unique_name v)
      )
    | Clet (v, e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:(Some v) with
      | Never_returns -> Never_returns
      | Ok (r1, sub_cfg) ->
        emit_expr (bind_let env sub_cfg v r1) sub_cfg e2 ~bound_name)
    | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr env sub_cfg body ~bound_name
    | Ctuple [] -> Ok ([||], sub_cfg)
    | Ctuple exp_list -> (
      match emit_parts_list env sub_cfg exp_list with
      | Never_returns -> Never_returns
      | Ok (simple_list, ext_env, sub_cfg) ->
        Ok (emit_tuple ext_env sub_cfg simple_list))
    | Cop (Craise k, args, dbg) -> emit_expr_raise env sub_cfg k args dbg
    | Cop (Copaque, args, dbg) -> (
      match emit_parts_list env sub_cfg args with
      | Never_returns -> Never_returns
      | Ok (simple_args, env, sub_cfg) ->
        let rs = emit_tuple env sub_cfg simple_args in
        let rd = insert_op_debug env sub_cfg (make_opaque ()) dbg rs rs in
        Ok (rd, sub_cfg))
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) -> (
      match emit_expr env sub_cfg arg ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok (loc_exp, sub_cfg) ->
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        let field_slice =
          Array.sub loc_exp size_before (Array.length fields_layout.(field))
        in
        Ok (field_slice, sub_cfg))
    | Cop (op, args, dbg) -> emit_expr_op env sub_cfg bound_name op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok _ -> emit_expr env sub_cfg e2 ~bound_name)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg, value_kind) ->
      emit_expr_ifthenelse env sub_cfg bound_name econd ifso_dbg eif ifnot_dbg
        eelse dbg value_kind
    | Cswitch (esel, index, ecases, dbg, value_kind) ->
      emit_expr_switch env sub_cfg bound_name esel index ecases dbg value_kind
    | Ccatch (_, [], e1, _) -> emit_expr env sub_cfg e1 ~bound_name
    | Ccatch (rec_flag, handlers, body, value_kind) ->
      emit_expr_catch env sub_cfg bound_name rec_flag handlers body value_kind
    | Cexit (lbl, args, traps) -> emit_expr_exit env sub_cfg lbl args traps
    | Ctrywith (e1, exn_cont, v, extra_args, e2, dbg, value_kind) ->
      emit_expr_trywith env sub_cfg bound_name e1 exn_cont v ~extra_args e2 dbg
        value_kind

  (* Emit an expression in tail position of a function *)
  and emit_tail env sub_cfg exp : Sub_cfg.t =
    match exp with
    | Clet (v, e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> sub_cfg
      | Ok (r1, sub_cfg) -> emit_tail (bind_let env sub_cfg v r1) sub_cfg e2)
    | Cphantom_let (_var, _defining_expr, body) -> emit_tail env sub_cfg body
    | Cop ((Capply (ty, Rc_normal) as op), args, dbg) ->
      emit_tail_apply env sub_cfg ty op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> sub_cfg
      | Ok _ -> emit_tail env sub_cfg e2)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg, value_kind) ->
      emit_tail_ifthenelse env sub_cfg econd ifso_dbg eif ifnot_dbg eelse dbg
        value_kind
    | Cswitch (esel, index, ecases, dbg, value_kind) ->
      emit_tail_switch env sub_cfg esel index ecases dbg value_kind
    | Ccatch (_, [], e1, _) -> emit_tail env sub_cfg e1
    | Ccatch (rec_flag, handlers, e1, value_kind) ->
      emit_tail_catch env sub_cfg rec_flag handlers e1 value_kind
    | Ctrywith (e1, exn_cont, v, extra_args, e2, dbg, value_kind) ->
      emit_tail_trywith env sub_cfg e1 exn_cont v ~extra_args e2 dbg value_kind
    | Cop _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ | Ctuple _ | Cexit _ ->
      emit_return env sub_cfg exp (pop_all_traps env)

  and emit_expr_raise env sub_cfg k (args : expression list) dbg :
      _ Or_never_returns.t =
    let r1 = emit_tuple env sub_cfg args in
    let extra_args_regs =
      match env.trap_stack with
      | Uncaught ->
        (* Function-level or toplevel exception continuations never have extra
           args. *)
        [||]
      | Specific_trap (cont, _trap_stack) ->
        Select_utils.env_find_regs_for_exception_extra_args cont env
    in
    (* Populate the distinguished extra args registers, for the current
       exception handler, with the extra args for this particular raise. *)
    let rd = Array.append [| Proc.loc_exn_bucket |] extra_args_regs in
    Array.iter2
      (fun r1 rd -> insert env sub_cfg (Op Move) [| r1 |] [| rd |])
      r1 rd;
    insert_debug' env sub_cfg (Cfg.Raise k) dbg rd [||];
    set_traps_for_raise env;
    Never_returns

  and emit_expr_op env sub_cfg bound_name op args dbg :
      (Reg.t array * Sub_cfg.t) Or_never_returns.t =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> Never_returns
    | Ok (simple_args, env, sub_cfg) -> (
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let add_naming_op_for_bound_name sub_cfg regs =
        match bound_name with
        | None -> ()
        | Some bound_name ->
          let provenance = VP.provenance bound_name in
          if Option.is_some provenance
          then
            let bound_name = VP.var bound_name in
            let naming_op =
              Operation.Name_for_debugger
                { ident = bound_name;
                  provenance;
                  which_parameter = None;
                  is_assignment = false;
                  regs
                }
            in
            insert_debug env sub_cfg (Op naming_op) Debuginfo.none [||] [||]
      in
      let ty = Select_utils.oper_result_type op in
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match new_op with
      | Terminator (Call { op = Indirect; label_after } as term) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rarg = Array.sub r1 1 (Array.length r1 - 1) in
        let rd = regs_for ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        insert_move_args env sub_cfg rarg loc_arg stack_ofs;
        insert_debug' env sub_cfg term dbg
          (Array.append [| r1.(0) |] loc_arg)
          loc_res;
        let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
        (* The destination registers (as per the procedure calling convention)
           need to be named right now, otherwise the result of the function call
           may be unavailable in the debugger immediately after the call. *)
        add_naming_op_for_bound_name sub_cfg loc_res;
        insert_move_results env sub_cfg loc_res rd stack_ofs;
        Select_utils.set_traps_for_raise env;
        Ok (rd, sub_cfg)
      | Terminator (Call { op = Direct _; label_after } as term) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rd = regs_for ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        insert_move_args env sub_cfg r1 loc_arg stack_ofs;
        insert_debug' env sub_cfg term dbg loc_arg loc_res;
        add_naming_op_for_bound_name sub_cfg loc_res;
        let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
        insert_move_results env sub_cfg loc_res rd stack_ofs;
        Select_utils.set_traps_for_raise env;
        Ok (rd, sub_cfg)
      | Terminator
          (Prim { op = External ({ ty_args; ty_res; _ } as r); label_after }) ->
        let loc_arg, stack_ofs =
          emit_extcall_args env sub_cfg ty_args new_args
        in
        let rd = regs_for ty_res in
        let term =
          Cfg.Prim { op = External { r with stack_ofs }; label_after }
        in
        let loc_res =
          insert_op_debug' env sub_cfg term dbg loc_arg
            (Proc.loc_external_results (Reg.typv rd))
        in
        let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
        add_naming_op_for_bound_name sub_cfg loc_res;
        insert_move_results env sub_cfg loc_res rd stack_ofs;
        Select_utils.set_traps_for_raise env;
        Ok (rd, sub_cfg)
      | Terminator (Prim { op = Probe _; label_after } as term) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rd = regs_for ty in
        let rd = insert_op_debug' env sub_cfg term dbg r1 rd in
        Select_utils.set_traps_for_raise env;
        let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
        Ok (rd, sub_cfg)
      | Terminator (Call_no_return ({ func_symbol; ty_args; _ } as r)) ->
        let loc_arg, stack_ofs = emit_extcall_args t env ty_args new_args in
        let keep_for_checking =
          !Select_utils.current_function_is_check_enabled
          && String.equal func_symbol Cmm.caml_flambda2_invalid
        in
        let returns, ty =
          if keep_for_checking then true, typ_int else false, ty
        in
        let rd = regs_for ty in
        let label = Cmm.new_label () in
        let r = { r with stack_ofs } in
        let term : Cfg.terminator =
          if keep_for_checking
          then Prim { op = External r; label_after = label }
          else Call_no_return r
        in
        let (_ : Reg.t array) =
          insert_op_debug' t env term dbg loc_arg
            (Proc.loc_external_results (Reg.typv rd))
        in
        Select_utils.set_traps_for_raise env;
        if returns
        then Ok (rd, Sub_cfg.add_never_block sub_cfg ~label)
        else Never_returns
      | Basic (Op (Alloc { bytes = _; mode; dbginfo = [placeholder] })) ->
        let rd = regs_for typ_val in
        let bytes = Select_utils.size_expr env (Ctuple new_args) in
        let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
        let op =
          Operation.Alloc
            { bytes = alloc_words * Arch.size_addr;
              dbginfo = [{ placeholder with alloc_words; alloc_dbg = dbg }];
              mode
            }
        in
        insert_debug env sub_cfg (Op op) dbg [||] rd;
        add_naming_op_for_bound_name sub_cfg rd;
        emit_stores env sub_cfg dbg new_args rd;
        Select_utils.set_traps_for_raise env;
        Ok (rd, sub_cfg)
      | Basic (Op (Alloc { bytes = _; mode = _; dbginfo })) ->
        Misc.fatal_errorf
          "Selection Alloc: expected a single placehold in dbginfo, found %d"
          (List.length dbginfo)
      | Basic (Op op) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rd = regs_for ty in
        add_naming_op_for_bound_name sub_cfg rd;
        Ok (insert_op_debug env sub_cfg op dbg r1 rd, sub_cfg)
      | Basic basic ->
        Misc.fatal_errorf "unexpected basic (%a)" Cfg.dump_basic basic
      | Terminator term ->
        Misc.fatal_errorf "unexpected terminator (%a)"
          (Cfg.dump_terminator ~sep:"")
          term)

  and emit_expr_ifthenelse env sub_cfg bound_name econd _ifso_dbg eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t)
      (_value_kind : Cmm.kind_for_unboxing) : _ Or_never_returns.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    let cond, earg = select_condition econd in
    match emit_expr env sub_cfg earg ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok (rarg, sub_cfg) -> (
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let if_ = emit_sequence env eif ~bound_name in
      let else_ = emit_sequence env eelse ~bound_name in
      match join env if_ else_ ~bound_name with
      | Never_returns -> Never_returns
      | Ok (r, sub_if, sub_else) ->
        let term_desc =
          terminator_of_test cond
            ~label_true:(Sub_cfg.start_label sub_if)
            ~label_false:(Sub_cfg.start_label sub_else)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
        Ok (r, Sub_cfg.join ~from:[sub_if; sub_else] ~to_:sub_cfg))

  and emit_expr_switch env sub_cfg bound_name esel index ecases
      (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) :
      _ Or_never_returns.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    match emit_expr env sub_cfg esel ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok (rsel, sub_cfg) -> (
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_cases =
        Array.map
          (fun (case, _dbg) -> emit_sequence env case ~bound_name)
          ecases
      in
      match join_array env sub_cases ~bound_name with
      | Never_returns -> Never_returns
      | Ok (r, sub_cases) ->
        let term_desc : Cfg.terminator =
          Switch
            (Array.map (fun idx -> Sub_cfg.start_label sub_cases.(idx)) index)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
        Ok (r, Sub_cfg.join ~from:(Array.to_list sub_cases) ~to_:sub_cfg))

  and emit_expr_catch env sub_cfg bound_name (_rec_flag : Cmm.rec_flag) handlers
      body (_value_kind : Cmm.kind_for_unboxing) : _ Or_never_returns.t =
    let handlers =
      List.map
        (fun (nfail, ids, e2, dbg, is_cold) ->
          let rs =
            List.map
              (fun (id, typ) ->
                let r = regs_for typ in
                Select_utils.name_regs id r;
                r)
              ids
          in
          nfail, ids, rs, e2, dbg, is_cold)
        handlers
    in
    let env, handlers_map =
      (* Since the handlers may be recursive, and called from the body, the same
         environment is used for translating both the handlers and the body. *)
      List.fold_left
        (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
          let label = Cmm.new_label () in
          let env, r =
            Select_utils.env_add_static_exception nfail rs env label
          in
          env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
        (env, Int.Map.empty) handlers
    in
    let body = emit_sequence env body ~bound_name in
    let translate_one_handler nfail
        (trap_info, (ids, rs, e2, _dbg, is_cold, label)) =
      assert (List.length ids = List.length rs);
      let trap_stack =
        match (!trap_info : Select_utils.trap_stack_info) with
        | Unreachable -> assert false
        | Reachable t -> t
      in
      let ids_and_rs = List.combine ids rs in
      let new_env =
        List.fold_left
          (fun env ((id, _typ), r) -> Select_utils.env_add sub_cfg id r env)
          (Select_utils.env_set_trap_stack env trap_stack)
          ids_and_rs
      in
      let handler =
        emit_sequence new_env e2 ~bound_name:None ~at_start:(fun sub_cfg ->
            List.iter
              (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance
                then
                  let var = VP.var var in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = r
                      }
                  in
                  insert_debug new_env sub_cfg (Cfg.Op naming_op) Debuginfo.none
                    [||] [||])
              ids_and_rs)
      in
      let r, sub_cfg =
        (* XXX this is maybe incorrect *)
        match handler with
        | Never_returns -> [||], Sub_cfg.make_empty ()
        | Ok (r, sub_cfg) -> r, sub_cfg
      in
      (nfail, trap_stack, is_cold, label), (r, sub_cfg)
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        Int.Map.partition
          (fun _n (r, _) ->
            match !r with
            | Select_utils.Unreachable -> true
            | Select_utils.Reachable _ -> false)
          not_built
      in
      if Int.Map.is_empty to_build
      then already_built
      else
        let already_built =
          Int.Map.fold
            (fun nfail handler already_built ->
              translate_one_handler nfail handler :: already_built)
            to_build already_built
        in
        build_all_reachable_handlers ~already_built ~not_built
    in
    let l =
      build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
      (* Note: we're dropping unreachable handlers here *)
    in
    let a = Array.of_list (body :: List.map snd l) in
    match join_array env a ~bound_name with
    | Never_returns -> Never_returns
    | Ok (r, s_handlers) ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let s_handlers =
        List.map
          (fun ((_, _, _, label), (_, sub_handler)) ->
            Sub_cfg.add_empty_block_at_start sub_handler ~label)
          s_handlers
      in
      let term_desc = Cfg.Always (Sub_cfg.start_label s_body) in
      Sub_cfg.update_exit_terminator sub_cfg term_desc;
      let sub_cfg = Sub_cfg.join ~from:(s_body :: s_handlers) ~to_:sub_cfg in
      Ok (r, sub_cfg)

  and emit_expr_exit env sub_cfg lbl args traps : _ Or_never_returns.t =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> Never_returns
    | Ok (simple_list, ext_env, sub_cfg) -> (
      match lbl with
      | Lbl nfail ->
        let src = emit_tuple ext_env sub_cfg simple_list in
        let handler =
          try Select_utils.env_find_static_exception nfail env
          with Not_found ->
            Misc.fatal_error
              ("Selection.emit_expr: unbound label "
             ^ Stdlib.Int.to_string nfail)
        in
        (* Intermediate registers to handle cases where some registers from src
           are present in dest *)
        let tmp_regs = Reg.createv_like src in
        (* Ccatch registers must not contain out of heap pointers *)
        Array.iter
          (fun reg ->
            match reg.Reg.typ with
            | Addr -> assert false
            | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
            | Val | Int | Float | Vec128 | Float32 -> ())
          src;
        insert_moves t env src tmp_regs;
        insert_moves t env tmp_regs (Array.concat handler.regs);
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        List.iter
          (fun trap ->
            let instr_desc =
              match trap with
              | Cmm.Push handler_id ->
                let lbl_handler =
                  (Select_utils.env_find_static_exception handler_id env).extra
                in
                Cfg.Pushtrap { lbl_handler }
              | Cmm.Pop _ -> Cfg.Poptrap
            in
            Sub_cfg.add_instruction sub_cfg instr_desc [||] [||] Debuginfo.none)
          traps;
        Sub_cfg.update_exit_terminator sub_cfg (Always handler.extra);
        Select_utils.set_traps nfail handler.Select_utils.traps_ref
          env.Select_utils.trap_stack traps;
        Never_returns
      | Return_lbl -> (
        match simple_list with
        | [expr] ->
          let (_ : Sub_cfg.t) = emit_return ext_env sub_cfg expr traps in
          (* XXX what should happen here? *)
          Never_returns
        | [] -> Misc.fatal_error "Selection.emit_expr: Return without arguments"
        | _ :: _ :: _ ->
          Misc.fatal_error "Selection.emit_expr: Return with too many arguments"
        ))

  and emit_expr_trywith env sub_cfg bound_name e1 exn_cont v ~extra_args e2
      (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) :
      _ Or_never_returns.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let exn_label = Cmm.new_label () in
    (* For each exception handler having extra arguments, distinguished
       registers corresponding to such arguments are created. They are populated
       at each raise site, which is the only place Cmm allows extra arguments to
       be involved in a raise. Any function call that involves extra arguments
       on its exception continuation has to compiled using a wrapper; see
       [To_cmm_expr.translate_apply]. *)
    let extra_arg_regs_split =
      List.map (fun (_param, machtype) -> regs_for t machtype) extra_args
    in
    let extra_arg_regs = Array.concat extra_arg_regs_split in
    let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
    let env_body =
      env_add_regs_for_exception_extra_args exn_cont extra_arg_regs env_body
    in
    let body = emit_sequence env_body e1 ~bound_name in
    let exn_bucket_in_handler = regs_for typ_val in
    let rv_list = exn_bucket_in_handler :: extra_arg_regs_split in
    let with_handler env_handler e2 : _ Or_never_returns.t =
      let handler =
        emit_sequence env_handler e2 ~bound_name ~at_start:(fun sub_cfg ->
            List.iter2
              (fun v regs ->
                let provenance = VP.provenance v in
                if Option.is_some provenance
                then
                  let var = VP.var v in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs
                      }
                  in
                  insert_debug env sub_cfg (Cfg.Op naming_op) Debuginfo.none
                    [||] [||])
              (v :: List.map fst extra_args)
              rv_list)
      in
      match join env body handler ~bound_name with
      | Never_returns -> Never_returns
      | Ok (r, s1, s2) ->
        Sub_cfg.mark_as_trap_handler s2 ~exn_label;
        Sub_cfg.add_instruction_at_start s2 (Cfg.Op Move)
          [| Proc.loc_exn_bucket |] exn_bucket_in_handler Debuginfo.none;
        Sub_cfg.update_exit_terminator sub_cfg (Always (Sub_cfg.start_label s1));
        Ok (r, Sub_cfg.join ~from:[s1; s2] ~to_:sub_cfg)
    in
    let env =
      List.fold_left2
        (* XXX check which [sub_cfg] this is *)
          (fun env var regs -> Select_utils.env_add sub_cfg var regs env)
        env
        (v :: List.map fst extra_args)
        rv_list
    in
    match Select_utils.env_find_static_exception exn_cont env_body with
    | { traps_ref = { contents = Reachable ts }; _ } ->
      with_handler (Select_utils.env_set_trap_stack env ts) e2
    | { traps_ref = { contents = Unreachable }; _ } ->
      let dummy_constant = Cconst_int (1, Debuginfo.none) in
      let segfault =
        Cmm.(
          Cop
            ( Cload
                { memory_chunk = Word_int;
                  mutability = Mutable;
                  is_atomic = false
                },
              [Cconst_int (0, Debuginfo.none)],
              Debuginfo.none ))
      in
      let dummy_raise =
        Cop (Craise Raise_notrace, [dummy_constant], Debuginfo.none)
      in
      let unreachable =
        (* The use of a raise operation means that this handler is known not to
           return, making it compatible with any layout for the body or
           surrounding code. We also set the trap stack to [Uncaught] to ensure
           that we don't introduce spurious control-flow edges inside the
           function. *)
        Csequence (segfault, dummy_raise)
      in
      let env = Select_utils.env_set_trap_stack env Uncaught in
      with_handler env unreachable
      (* Misc.fatal_errorf "Selection.emit_expr: \ * Unreachable exception
         handler %d" lbl *)
    | exception Not_found ->
      Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont

  and emit_sequence ?at_start (env : environment) exp ~bound_name :
      (Reg.t array * Sub_cfg.t) Or_never_returns.t =
    let sub_cfg = Sub_cfg.make_empty () in
    (match at_start with None -> () | Some f -> f sub_cfg);
    emit_expr env sub_cfg exp ~bound_name

  (* Same, but in tail position. *)

  and emit_return (env : environment) sub_cfg exp traps : Sub_cfg.t =
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    match emit_expr env sub_cfg exp ~bound_name:None with
    | Never_returns -> sub_cfg
    | Ok (rd, sub_cfg) ->
      insert_return env sub_cfg (Some rd) traps;
      sub_cfg

  and emit_tail_apply env (sub_cfg : Sub_cfg.t) ty op args dbg : Sub_cfg.t =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> sub_cfg
    | Ok (simple_args, env, sub_cfg) -> (
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match new_op with
      | Terminator (Call { op = Indirect; label_after } as term) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rd = regs_for ty in
        let rarg = Array.sub r1 1 (Array.length r1 - 1) in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_func Indirect in
          insert_moves env sub_cfg rarg loc_arg;
          insert_debug' env sub_cfg call dbg
            (Array.append [| r1.(0) |] loc_arg)
            [||];
          sub_cfg)
        else (
          insert_move_args env sub_cfg rarg loc_arg stack_ofs;
          insert_debug' env sub_cfg term dbg
            (Array.append [| r1.(0) |] loc_arg)
            loc_res;
          let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
          Select_utils.set_traps_for_raise env;
          insert env sub_cfg (Op (Stackoffset (-stack_ofs))) [||] [||];
          insert_return env sub_cfg (Some loc_res) (pop_all_traps env);
          sub_cfg)
      | Terminator (Call { op = Direct func; label_after } as term) ->
        let r1 = emit_tuple env sub_cfg new_args in
        let rd = regs_for ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if String.equal func.sym_name !Select_utils.current_function_name
           && Select_utils.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_self { destination = !tailrec_label } in
          let loc_arg' =
            assert (stack_ofs >= 0);
            if stack_ofs = 0 then loc_arg else Proc.loc_parameters (Reg.typv r1)
          in
          insert_moves env sub_cfg r1 loc_arg';
          insert_debug' env sub_cfg call dbg loc_arg' [||];
          sub_cfg)
        else if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_func (Direct func) in
          insert_moves env sub_cfg r1 loc_arg;
          insert_debug' env sub_cfg call dbg loc_arg [||];
          sub_cfg)
        else (
          insert_move_args env sub_cfg r1 loc_arg stack_ofs;
          insert_debug' env sub_cfg term dbg loc_arg loc_res;
          let sub_cfg = Sub_cfg.add_never_block sub_cfg ~label:label_after in
          Select_utils.set_traps_for_raise env;
          insert env sub_cfg (Op (Stackoffset (-stack_ofs))) [||] [||];
          insert_return env sub_cfg (Some loc_res) (pop_all_traps env);
          sub_cfg)
      | _ -> Misc.fatal_error "Cfg_selectgen.emit_tail")

  and emit_tail_ifthenelse env sub_cfg econd (_ifso_dbg : Debuginfo.t) eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t)
      (_kind : Cmm.kind_for_unboxing) : Sub_cfg.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    let cond, earg = select_condition econd in
    match emit_expr env sub_cfg earg ~bound_name:None with
    | Never_returns -> sub_cfg
    | Ok (rarg, sub_cfg) ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_if = emit_tail_sequence env eif in
      let sub_else = emit_tail_sequence env eelse in
      let term_desc =
        terminator_of_test cond
          ~label_true:(Sub_cfg.start_label sub_if)
          ~label_false:(Sub_cfg.start_label sub_else)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
      Sub_cfg.join_tail ~from:[sub_if; sub_else] ~to_:sub_cfg

  and emit_tail_switch env sub_cfg esel index ecases (_dbg : Debuginfo.t)
      (_kind : Cmm.kind_for_unboxing) : Sub_cfg.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    match emit_expr env sub_cfg esel ~bound_name:None with
    | Never_returns -> sub_cfg
    | Ok (rsel, sub_cfg) ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_cases =
        Array.map (fun (case, _dbg) -> emit_tail_sequence env case) ecases
      in
      let term_desc : Cfg.terminator =
        Switch
          (Array.map (fun idx -> Sub_cfg.start_label sub_cases.(idx)) index)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
      Sub_cfg.join_tail ~from:(Array.to_list sub_cases) ~to_:sub_cfg

  and emit_tail_catch env sub_cfg (_rec_flag : Cmm.rec_flag) handlers e1
      (_value_kind : Cmm.kind_for_unboxing) : Sub_cfg.t =
    let handlers =
      List.map
        (fun (nfail, ids, e2, dbg, is_cold) ->
          let rs =
            List.map
              (fun (id, typ) ->
                let r = regs_for typ in
                Select_utils.name_regs id r;
                r)
              ids
          in
          nfail, ids, rs, e2, dbg, is_cold)
        handlers
    in
    let env, handlers_map =
      List.fold_left
        (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
          let label = Cmm.new_label () in
          let env, r =
            Select_utils.env_add_static_exception nfail rs env label
          in
          env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
        (env, Int.Map.empty) handlers
    in
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let s_body = emit_tail_sequence env e1 in
    let translate_one_handler nfail
        (trap_info, (ids, rs, e2, _dbg, is_cold, label)) =
      assert (List.length ids = List.length rs);
      let trap_stack =
        match (!trap_info : Select_utils.trap_stack_info) with
        | Unreachable -> assert false
        | Reachable t -> t
      in
      let ids_and_rs = List.combine ids rs in
      let new_env =
        List.fold_left
          (fun env ((id, _typ), r) -> Select_utils.env_add id r env)
          (Select_utils.env_set_trap_stack env trap_stack)
          ids_and_rs
      in
      let seq : Sub_cfg.t =
        emit_tail_sequence new_env e2 ~at_start:(fun sub_cfg ->
            List.iter
              (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance
                then
                  let var = VP.var var in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = r
                      }
                  in
                  insert_debug new_env sub_cfg (Cfg.Op naming_op) Debuginfo.none
                    [||] [||])
              ids_and_rs)
      in
      let seq = Sub_cfg.add_empty_block_at_start seq ~label in
      nfail, trap_stack, seq, is_cold
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        Int.Map.partition
          (fun _n (r, _) ->
            match !r with
            | Select_utils.Unreachable -> true
            | Select_utils.Reachable _ -> false)
          not_built
      in
      if Int.Map.is_empty to_build
      then already_built
      else
        let already_built =
          Int.Map.fold
            (fun nfail handler already_built ->
              translate_one_handler nfail handler :: already_built)
            to_build already_built
        in
        build_all_reachable_handlers ~already_built ~not_built
    in
    let new_handlers :
        (int * Simple_operation.trap_stack * Sub_cfg.t * bool) list =
      build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
      (* Note: we're dropping unreachable handlers here *)
    in
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let term_desc = Cfg.Always (Sub_cfg.start_label s_body) in
    Sub_cfg.update_exit_terminator sub_cfg term_desc;
    let s_handlers = List.map (fun (_, _, s, _) -> s) new_handlers in
    Sub_cfg.join_tail ~from:(s_body :: s_handlers) ~to_:sub_cfg

  and emit_tail_trywith env sub_cfg e1 exn_cont v ~extra_args e2
      (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) : Sub_cfg.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let exn_label = Cmm.new_label () in
    (* See comment in emit_expr_trywith about extra args *)
    let extra_arg_regs_split =
      List.map (fun (_param, machtype) -> regs_for machtype) extra_args
    in
    let extra_arg_regs = Array.concat extra_arg_regs_split in
    let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
    let env_body =
      env_add_regs_for_exception_extra_args exn_cont extra_arg_regs env_body
    in
    let s1 : Sub_cfg.t = emit_tail_sequence env_body e1 in
    let exn_bucket_in_handler = regs_for typ_val in
    let rv_list = exn_bucket_in_handler :: extra_arg_regs_split in
    let with_handler env_handler e2 : Sub_cfg.t =
      let s2 : Sub_cfg.t =
        emit_tail_sequence env_handler e2 ~at_start:(fun sub_cfg ->
            List.iter2
              (fun v regs ->
                let provenance = VP.provenance v in
                if Option.is_some provenance
                then
                  let var = VP.var v in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs
                      }
                  in
                  insert_debug env sub_cfg (Cfg.Op naming_op) Debuginfo.none
                    [||] [||])
              (v :: List.map fst extra_args)
              rv_list)
      in
      Sub_cfg.mark_as_trap_handler s2 ~exn_label;
      Sub_cfg.add_instruction_at_start s2 (Cfg.Op Move)
        [| Proc.loc_exn_bucket |] exn_bucket_in_handler Debuginfo.none;
      Sub_cfg.update_exit_terminator sub_cfg (Always (Sub_cfg.start_label s1));
      Sub_cfg.join_tail ~from:[s1; s2] ~to_:sub_cfg
    in
    let env =
      List.fold_left2
        (fun env var regs -> Select_utils.env_add var regs env)
        env
        (v :: List.map fst extra_args)
        rv_list
    in
    match Select_utils.env_find_static_exception exn_cont env_body with
    | { traps_ref = { contents = Reachable ts }; _ } ->
      with_handler (Select_utils.env_set_trap_stack env ts) e2
    | { traps_ref = { contents = Unreachable }; _ } ->
      (* Note: The following [unreachable] expression has machtype [|Int|], but
         this might not be the correct machtype for this function's return
         value. It doesn't matter at runtime since the expression cannot return,
         but if we start checking (or joining) the machtypes of the different
         tails we will need to implement something like the [emit_expr] version
         above, that hides the machtype. *)
      let unreachable =
        Cmm.(
          Cop
            ( Cload
                { memory_chunk = Word_int;
                  mutability = Mutable;
                  is_atomic = false
                },
              [Cconst_int (0, Debuginfo.none)],
              Debuginfo.none ))
      in
      with_handler env unreachable
    (* Misc.fatal_errorf "Selection.emit_expr: \ Unreachable exception handler
       %d" lbl *)
    | exception Not_found ->
      Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont

  and emit_tail_sequence ?at_start env exp : Sub_cfg.t =
    let sub_cfg = Sub_cfg.make_empty () in
    (match at_start with None -> () | Some f -> f sub_cfg);
    emit_tail env sub_cfg exp

  (* Sequentialization of a function definition *)

  let emit_fundecl ~future_funcnames f =
    Select_utils.current_function_name := f.Cmm.fun_name.sym_name;
    Select_utils.current_function_is_check_enabled
      := Zero_alloc_checker.is_check_enabled f.Cmm.fun_codegen_options
           f.Cmm.fun_name.sym_name f.Cmm.fun_dbg;
    let num_regs_per_arg = Array.make (List.length f.Cmm.fun_args) 0 in
    let rargs =
      List.mapi
        (fun arg_index (var, ty) ->
          let r = regs_for ty in
          Select_utils.name_regs var r;
          num_regs_per_arg.(arg_index) <- Array.length r;
          r)
        f.Cmm.fun_args
    in
    let rarg = Array.concat rargs in
    let loc_arg = Proc.loc_parameters (Reg.typv rarg) in
    let env =
      List.fold_right2
        (fun (id, _ty) r env -> Select_utils.env_add id r env)
        f.Cmm.fun_args rargs Select_utils.env_empty
    in
    tailrec_label := Cmm.new_label ();
    let sub_cfg = Sub_cfg.make_empty () in
    let loc_arg_index = ref 0 in
    List.iteri
      (fun param_index (var, _ty) ->
        let provenance = VP.provenance var in
        let var = VP.var var in
        let num_regs_for_arg = num_regs_per_arg.(param_index) in
        let hard_regs_for_arg =
          Array.init num_regs_for_arg (fun index ->
              loc_arg.(!loc_arg_index + index))
        in
        loc_arg_index := !loc_arg_index + num_regs_for_arg;
        if Option.is_some provenance
        then
          let naming_op =
            Operation.Name_for_debugger
              { ident = var;
                provenance;
                which_parameter = Some param_index;
                is_assignment = false;
                regs = hard_regs_for_arg
              }
          in
          insert_debug env sub_cfg (Cfg.Op naming_op) Debuginfo.none
            hard_regs_for_arg [||])
      f.Cmm.fun_args;
    insert_moves env sub_cfg loc_arg rarg;
    let prologue_poll_instr_id =
      insert_op_debug_returning_id env sub_cfg Operation.Poll Debuginfo.none
        [||] [||]
    in
    let body = emit_tail env sub_cfg f.Cmm.fun_body in
    let cfg =
      (* note: we set `fun_contains_calls` to `true` here, but will compute its
         proper value below, after possibly removing the prologue poll
         instruction. It is not very satisfactory, but as noted in the CR below,
         we should revisit the way we handle polling points. *)
      Cfg.create ~fun_name:f.Cmm.fun_name.sym_name ~fun_args:loc_arg
        ~fun_codegen_options:
          (Cfg.of_cmm_codegen_option f.Cmm.fun_codegen_options)
        ~fun_dbg:f.Cmm.fun_dbg ~fun_contains_calls:true
        ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
        ~fun_poll:f.Cmm.fun_poll
    in
    let layout = DLL.make_empty () in
    let entry_block =
      Cfg.make_empty_block ~label:(Cfg.entry_label cfg)
        (Cfg.make_instr (Cfg.Always !tailrec_label) [||] [||] Debuginfo.none)
    in
    DLL.add_begin entry_block.body
      (Cfg.make_instr Cfg.Prologue [||] [||] Debuginfo.none);
    Cfg.add_block_exn cfg entry_block;
    DLL.add_end layout entry_block.start;
    let tailrec_block =
      Cfg.make_empty_block ~label:!tailrec_label
        (Cfg.make_instr
           (Cfg.Always (Sub_cfg.start_label body))
           [||] [||] Debuginfo.none)
    in
    Cfg.add_block_exn cfg tailrec_block;
    DLL.add_end layout tailrec_block.start;
    let delete_prologue_poll =
      (* CR mshinwell/xclerc: find a neater way of doing this rather than making
         a special case for the [optimistic_prologue_poll_instr_id]. *)
      not
        (Cfg_polling.requires_prologue_poll ~future_funcnames
           ~fun_name:f.Cmm.fun_name.sym_name
           ~optimistic_prologue_poll_instr_id:prologue_poll_instr_id cfg)
    in
    let found_prologue_poll = ref false in
    Sub_cfg.iter_basic_blocks body ~f:(fun (block : Cfg.basic_block) ->
        if delete_prologue_poll && not !found_prologue_poll
        then
          DLL.filter_left block.body
            ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
              let is_prologue_poll =
                InstructionId.equal instr.id prologue_poll_instr_id
              in
              if is_prologue_poll then found_prologue_poll := true;
              not is_prologue_poll);
        if not (Cfg.is_never_terminator block.terminator.desc)
        then (
          block.can_raise <- Cfg.can_raise_terminator block.terminator.desc;
          if Cfg.is_return_terminator block.terminator.desc
          then
            DLL.add_end block.body
              (Cfg.make_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
          Cfg.add_block_exn cfg block;
          DLL.add_end layout block.start)
        else assert (DLL.is_empty block.body));
    if delete_prologue_poll && not !found_prologue_poll
    then Misc.fatal_error "Did not find [Poll] instruction to delete";
    (* note: `Cfgize.Stack_offset_and_exn.update_cfg` may add edges to the
       graph, and should hence be executed before
       `Cfg.register_predecessors_for_all_blocks`. *)
    Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
    let fun_contains_calls =
      Sub_cfg.exists_basic_blocks body ~f:Cfg.basic_block_contains_calls
    in
    let cfg = { cfg with fun_contains_calls } in
    let cfg_with_layout =
      Cfg_with_layout.create cfg ~layout ~preserve_orig_labels:false
        ~new_labels:Label.Set.empty
    in
    (* CR xclerc for xclerc: Regalloc_irc_utils.log_cfg_with_infos ~indent:1
       (Cfg_with_infos.make cfg_with_layout); *)
    Merge_straightline_blocks.run cfg_with_layout;
    Simplify_terminator.run cfg;
    Eliminate_dead_code.run_dead_block cfg_with_layout;
    cfg_with_layout
end
