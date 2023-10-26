# 2 "asmcomp/i386/proc.ml"
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

(* Description of the Intel 386 processor *)

open Misc
open Arch
open Cmm
open Reg
open Mach

(* Which asm conventions to use *)
let masm =
  match Config.ccomp_type with
  | "msvc" -> true
  | _      -> false

(* Registers available for register allocation *)

(* Register map:
    eax         0               eax - edi: function arguments and results
    ebx         1               eax: C function results
    ecx         2               ebx, esi, edi, ebp: preserved by C
    edx         3
    esi         4
    edi         5
    ebp         6

    tos         100             top of floating-point stack. *)

let int_reg_name =
  if masm then
    [| "eax"; "ebx"; "ecx"; "edx"; "esi"; "edi"; "ebp" |]
  else
    [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi"; "%ebp" |]

let float_reg_name =
  if masm then
    [| "tos" |]
  else
    [| "%tos" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 1

let num_available_registers = [| 7; 0 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

(* There is little scheduling, and some operations are more compact
   when their argument is %eax. *)

let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 7 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg = [| Reg.at_location Float (Reg 100) |]

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let eax = phys_reg 0
let ebx = phys_reg 1
let ecx = phys_reg 2
let edx = phys_reg 3

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref (- size_domainstate_args) in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
      Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align (max 0 !ofs) stack_alignment)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 5 100 99 outgoing arg
let loc_parameters arg =
  let (loc, _ofs) = calling_conventions 0 5 100 99 incoming arg in loc

(* CR vlaviron: The old code used to allow a single float register for
   the return registers (even though the case was never used). I've
   chosen to forbid it to match the convention for parameters.
   Unboxed float return values will thus end up either on the reserved
   region of the domain state or on the stack. *)
let loc_results_call res =
  calling_conventions 0 5 100 99 outgoing res
let loc_results_return res =
  let (loc, _ofs) = calling_conventions 0 5 100 99 incoming res in loc

let max_arguments_for_tailcalls =
  6 (* in registers *) + 64 (* in domain state *)

let loc_external_arguments _arg =
  fatal_error "Proc.loc_external_arguments"
let loc_external_results res =
  match res with
  | [| Int; Int |] -> [|eax; edx|]
  | _ ->
      let (loc, _ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = eax

(* See "System V Application Binary Interface Intel386 Architecture
   Processor Supplement Version 1.0"
   (https://www.uclibc.org/docs/psABI-i386.pdf) *)

let int_dwarf_reg_numbers = [| 0; 3; 1; 2; 6; 7; 5 |]

let float_dwarf_reg_numbers = [| |]

let dwarf_register_numbers ~reg_class =
  match reg_class with
  | 0 -> int_dwarf_reg_numbers
  | 1 -> float_dwarf_reg_numbers
  | _ -> Misc.fatal_errorf "Bad register class %d" reg_class

let stack_ptr_dwarf_register_number = 4

(* Volatile registers: the x87 top of FP stack is *)

let reg_is_volatile = function
  | { typ = Float; loc = Reg _ } -> true
  | _ -> false

let regs_are_volatile rs =
  try
    for i = 0 to Array.length rs - 1 do
      if reg_is_volatile rs.(i) then raise Exit
    done;
    false
  with Exit ->
    true

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* ebx, esi, edi, ebp preserved *)
  [|eax; ecx; edx|]

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall { alloc = true; _}) ->
    all_phys_regs
  | Iop(Iextcall { alloc = false; }) -> destroyed_at_c_call
  | Iop(Iintop(Idiv | Imod)) -> [| eax; edx |]
  | Iop(Ialloc _) -> [| eax; ebx |]
  | Iop(Iintop Imulh) -> [| eax |]
  | Iop(Iintop(Icomp _) | Iintop_imm(Icomp _, _)) -> [| eax |]
  | Iop(Iintoffloat) -> [| eax |]
  | Iop(Ibeginregion|Iendregion) -> [| eax; ebx |]
  | Iifthenelse(Ifloattest _, _, _) -> [| eax |]
  | Itrywith _ -> [| edx |]
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

(* Maximal register pressure *)

let safe_register_pressure _op = 4

let max_register_pressure = function
    Iextcall _ -> [| 4; max_int |]
  | Iintop(Idiv | Imod) -> [| 5; max_int |]
  | Ialloc _ | Iintop(Icomp _) | Iintop_imm(Icomp _, _) |
    Iintoffloat -> [| 6; max_int |]
  | _ -> [|7; max_int |]

(* Layout of the stack frame *)

let frame_required fd =
  let frame_size_at_top_of_function =
    (* cf. [frame_size] in emit.mlp. *)
    Misc.align (4*fd.fun_num_stack_slots.(0) + 8*fd.fun_num_stack_slots.(1) + 4)
      stack_alignment
  in
  frame_size_at_top_of_function > 4

let prologue_required fd =
  frame_required fd

(* Calling the assembler *)

let assemble_file infile outfile =
  X86_proc.assemble_file infile outfile

let init () = ()
