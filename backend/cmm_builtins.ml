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

open Cmm
open Cmm_helpers
open Arch

let four_args name args =
  match args with
  | [arg1; arg2; arg3; arg4] -> arg1, arg2, arg3, arg4
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 4 arguments for %s" name

let three_args name args =
  match args with
  | [arg1; arg2; arg3] -> arg1, arg2, arg3
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 3 arguments for %s" name

let two_args name args =
  match args with
  | [arg1; arg2] -> arg1, arg2
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 2 arguments for %s" name

let one_arg name args =
  match args with
  | [arg] -> arg
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 1 argument for %s" name

let if_operation_supported op ~f =
  match Proc.operation_supported op with true -> Some (f ()) | false -> None

let if_operation_supported_bi bi op ~f =
  if bi = Primitive.Pint64 && size_int = 4
  then None
  else if_operation_supported op ~f

let int_of_value arg dbg = Cop (Cintofvalue, [arg], Op_debuginfo.create dbg)

let value_of_int arg dbg = Cop (Cvalueofint, [arg], Op_debuginfo.create dbg)

(* Untagging of a negative value shifts in an extra bit. The following code
   clears the shifted sign bit of an untagged int. This straightline code is
   faster on most targets than conditional code for checking whether the
   argument is negative. *)
let clear_sign_bit arg dbg =
  let mask = Nativeint.lognot (Nativeint.shift_left 1n ((size_int * 8) - 1)) in
  Cop (Cand, [arg; Cconst_natint (mask, dbg)], Op_debuginfo.create dbg)

let clz ~arg_is_non_zero bi arg dbg =
  let op = Cclz { arg_is_non_zero } in
  if_operation_supported_bi bi op ~f:(fun () ->
      let res =
        Cop (op, [make_unsigned_int bi arg dbg], Op_debuginfo.create dbg)
      in
      if bi = Primitive.Pint32 && size_int = 8
      then Cop (Caddi, [res; Cconst_int (-32, dbg)], Op_debuginfo.create dbg)
      else res)

let ctz ~arg_is_non_zero bi arg dbg =
  let arg = make_unsigned_int bi arg dbg in
  if bi = Primitive.Pint32 && size_int = 8
  then
    (* regardless of the value of the argument [arg_is_non_zero], always set the
       corresponding field to [true], because we make it non-zero below by
       setting bit 32. *)
    let op = Cctz { arg_is_non_zero = true } in
    if_operation_supported_bi bi op ~f:(fun () ->
        (* Set bit 32 *)
        let mask = Nativeint.shift_left 1n 32 in
        Cop
          ( op,
            [ Cop
                (Cor, [arg; Cconst_natint (mask, dbg)], Op_debuginfo.create dbg)
            ],
            Op_debuginfo.create dbg ))
  else
    let op = Cctz { arg_is_non_zero } in
    if_operation_supported_bi bi op ~f:(fun () ->
        Cop (op, [arg], Op_debuginfo.create dbg))

let popcnt bi arg dbg =
  if_operation_supported_bi bi Cpopcnt ~f:(fun () ->
      Cop (Cpopcnt, [make_unsigned_int bi arg dbg], Op_debuginfo.create dbg))

let mulhi bi ~signed args dbg =
  let op = Cmulhi { signed } in
  if_operation_supported_bi bi op ~f:(fun () ->
      Cop (op, args, Op_debuginfo.create dbg))

let ext_pointer_load chunk name args dbg =
  let p = int_as_pointer (one_arg name args) dbg in
  Some (Cop (Cload (chunk, Mutable), [p], Op_debuginfo.create dbg))

let ext_pointer_store chunk name args dbg =
  let arg1, arg2 = two_args name args in
  let p = int_as_pointer arg1 dbg in
  Some
    (return_unit dbg
       (Cop (Cstore (chunk, Assignment), [p; arg2], Op_debuginfo.create dbg)))

let bigstring_prefetch ~is_write locality args dbg =
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      let arg1, arg2 = two_args "bigstring_prefetch" args in
      (* [arg2], the index, is already untagged. *)
      bind "index" arg2 (fun idx ->
          bind "ba" arg1 (fun ba ->
              bind "ba_data"
                (Cop
                   ( Cload (Word_int, Mutable),
                     [field_address ba 1 dbg],
                     Op_debuginfo.create dbg ))
                (fun ba_data ->
                  (* pointer to element "idx" of "ba" of type (char,
                     int8_unsigned_elt, c_layout) Bigarray.Array1.t is simply
                     offset "idx" from "ba_data" *)
                  return_unit dbg
                    (Cop (op, [add_int ba_data idx dbg], Op_debuginfo.create dbg))))))

let prefetch ~is_write locality arg dbg =
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      return_unit dbg (Cop (op, [arg], Op_debuginfo.create dbg)))

let prefetch_offset ~is_write locality (arg1, arg2) dbg =
  (* [arg2], the index, is already untagged. *)
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      return_unit dbg
        (Cop (op, [add_int arg1 arg2 dbg], Op_debuginfo.create dbg)))

let ext_pointer_prefetch ~is_write locality arg dbg =
  prefetch ~is_write locality (int_as_pointer arg dbg) dbg

let native_pointer_cas size (arg1, arg2, arg3) dbg =
  let op = Catomic { op = Compare_and_swap; size } in
  if_operation_supported op ~f:(fun () ->
      bind "set_to" arg3 (fun set_to ->
          bind "compare_with" arg2 (fun compare_with ->
              bind "dst" arg1 (fun dst ->
                  tag_int
                    (Cop
                       (op, [compare_with; set_to; dst], Op_debuginfo.create dbg))
                    dbg))))

let ext_pointer_cas size (arg1, arg2, arg3) dbg =
  native_pointer_cas size (int_as_pointer arg1 dbg, arg2, arg3) dbg

let bigstring_cas size (arg1, arg2, arg3, arg4) dbg =
  let op = Catomic { op = Compare_and_swap; size } in
  if_operation_supported op ~f:(fun () ->
      bind "set_to" arg4 (fun set_to ->
          bind "compare_with" arg3 (fun compare_with ->
              bind "idx" arg2 (fun idx ->
                  bind "bs" arg1 (fun bs ->
                      bind "bs_data"
                        (Cop
                           ( Cload (Word_int, Mutable),
                             [field_address bs 1 dbg],
                             Op_debuginfo.create dbg ))
                        (fun bs_data ->
                          bind "dst" (add_int bs_data idx dbg) (fun dst ->
                              tag_int
                                (Cop
                                   ( op,
                                     [compare_with; set_to; dst],
                                     Op_debuginfo.create dbg ))
                                dbg)))))))

let native_pointer_atomic_add size (arg1, arg2) dbg =
  let op = Catomic { op = Fetch_and_add; size } in
  if_operation_supported op ~f:(fun () ->
      bind "src" arg2 (fun src ->
          bind "dst" arg1 (fun dst ->
              Cop (op, [src; dst], Op_debuginfo.create dbg))))

let native_pointer_atomic_sub size (arg1, arg2) dbg =
  native_pointer_atomic_add size (arg1, neg_int arg2 dbg) dbg

let ext_pointer_atomic_add size (arg1, arg2) dbg =
  native_pointer_atomic_add size (int_as_pointer arg1 dbg, arg2) dbg

let ext_pointer_atomic_sub size (arg1, arg2) dbg =
  native_pointer_atomic_add size (int_as_pointer arg1 dbg, neg_int arg2 dbg) dbg

let bigstring_atomic_add size (arg1, arg2, arg3) dbg =
  let op = Catomic { op = Fetch_and_add; size } in
  if_operation_supported op ~f:(fun () ->
      bind "src" arg3 (fun src ->
          bind "idx" arg2 (fun idx ->
              bind "bs" arg1 (fun bs ->
                  bind "bs_data"
                    (Cop
                       ( Cload (Word_int, Mutable),
                         [field_address bs 1 dbg],
                         Op_debuginfo.create dbg ))
                    (fun bs_data ->
                      bind "dst" (add_int bs_data idx dbg) (fun dst ->
                          Cop (op, [src; dst], Op_debuginfo.create dbg)))))))

let bigstring_atomic_sub size (arg1, arg2, arg3) dbg =
  bigstring_atomic_add size (arg1, arg2, neg_int arg3 dbg) dbg

(** [transl_builtin prim args dbg] returns None if the built-in [prim] is not
  supported, otherwise it constructs and returns the corresponding Cmm
  expression.

  The names of builtins below correspond to the native code names associated
  with "external" declarations in the stand-alone library [ocaml_intrinsics].

  For situations such as where the Cmm code below returns e.g. an untagged
  integer, we exploit the generic mechanism on "external" to deal with the
  tagging before the result is returned to the user. *)
let transl_builtin name args dbg typ_res =
  match name with
  | "caml_int_clz_tagged_to_untagged" ->
    (* The tag does not change the number of leading zeros. The advantage of
       keeping the tag is it guarantees that, on x86-64, the input to the BSR
       instruction is nonzero. *)
    let op = Cclz { arg_is_non_zero = true } in
    if_operation_supported op ~f:(fun () ->
        Cop (op, args, Op_debuginfo.create dbg))
  | "caml_int_clz_untagged_to_untagged" ->
    let op = Cclz { arg_is_non_zero = false } in
    if_operation_supported op ~f:(fun () ->
        let arg = clear_sign_bit (one_arg name args) dbg in
        Cop
          ( Caddi,
            [Cop (op, [arg], Op_debuginfo.create dbg); Cconst_int (-1, dbg)],
            Op_debuginfo.create dbg ))
  | "caml_int64_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pint64 (one_arg name args) dbg
  | "caml_int32_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pint32 (one_arg name args) dbg
  | "caml_nativeint_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pnativeint (one_arg name args) dbg
  | "caml_int64_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pint64 (one_arg name args) dbg
  | "caml_int32_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pint32 (one_arg name args) dbg
  | "caml_nativeint_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pnativeint (one_arg name args) dbg
  | "caml_int_popcnt_tagged_to_untagged" ->
    if_operation_supported Cpopcnt ~f:(fun () ->
        (* Having the argument tagged saves a shift, but there is one extra
           "set" bit, which is accounted for by the (-1) below. *)
        Cop
          ( Caddi,
            [Cop (Cpopcnt, args, Op_debuginfo.create dbg); Cconst_int (-1, dbg)],
            Op_debuginfo.create dbg ))
  | "caml_int_popcnt_untagged_to_untagged" ->
    (* This code is expected to be faster than [popcnt(tagged_x) - 1] when the
       untagged argument is already available from a previous computation. *)
    if_operation_supported Cpopcnt ~f:(fun () ->
        let arg = clear_sign_bit (one_arg name args) dbg in
        Cop (Cpopcnt, [arg], Op_debuginfo.create dbg))
  | "caml_int64_popcnt_unboxed_to_untagged" ->
    popcnt Pint64 (one_arg name args) dbg
  | "caml_int32_popcnt_unboxed_to_untagged" ->
    popcnt Pint32 (one_arg name args) dbg
  | "caml_nativeint_popcnt_unboxed_to_untagged" ->
    popcnt Pnativeint (one_arg name args) dbg
  | "caml_int_ctz_untagged_to_untagged" ->
    (* Assuming a 64-bit x86-64 target:

       Setting the top bit of the input for the BSF instruction ensures the
       input is nonzero without affecting the result.

       The expression [x lor (1 lsl 63)] sets the top bit of x. The constant:

       [1 lsl 63]

       can be precomputed statically:

       Cconst_natint ((Nativeint.shift_left 1n 63), dbg)

       However, the encoding of this OR instruction with the large static
       constant is 10 bytes long, on x86-64. Instead, we emit a shift operation,
       whose corresponding instruction is 1 byte shorter. This will not require
       an extra register, unless both the argument and result of the BSF
       instruction are in the same register. *)
    let op = Cctz { arg_is_non_zero = true } in
    if_operation_supported op ~f:(fun () ->
        let c =
          Cop
            ( Clsl,
              [Cconst_int (1, dbg); Cconst_int ((size_int * 8) - 1, dbg)],
              Op_debuginfo.create dbg )
        in
        Cop
          ( op,
            [Cop (Cor, [one_arg name args; c], Op_debuginfo.create dbg)],
            Op_debuginfo.create dbg ))
  | "caml_int32_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pint32 (one_arg name args) dbg
  | "caml_int64_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pint64 (one_arg name args) dbg
  | "caml_nativeint_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pnativeint (one_arg name args) dbg
  | "caml_int32_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pint32 (one_arg name args) dbg
  | "caml_int64_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pint64 (one_arg name args) dbg
  | "caml_nativeint_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pnativeint (one_arg name args) dbg
  | "caml_signed_int64_mulh_unboxed" -> mulhi ~signed:true Pint64 args dbg
  | "caml_unsigned_int64_mulh_unboxed" -> mulhi ~signed:false Pint64 args dbg
  | "caml_int32_unsigned_to_int_trunc_unboxed_to_untagged" ->
    Some (zero_extend_32 dbg (one_arg name args))
  | "caml_csel_value" | "caml_csel_int_untagged" | "caml_csel_int64_unboxed"
  | "caml_csel_int32_unboxed" | "caml_csel_nativeint_unboxed" ->
    (* Unboxed float variant of csel intrinsic is not currently supported. It
       can be emitted on arm64 using FCSEL, but there appears to be no
       corresponding instruction on amd64 for xmm registers. *)
    let op = Ccsel typ_res in
    let cond, ifso, ifnot = three_args name args in
    if_operation_supported op ~f:(fun () ->
        Cop (op, [test_bool dbg cond; ifso; ifnot], Op_debuginfo.create dbg))
  (* Native_pointer: handled as unboxed nativeint *)
  | "caml_ext_pointer_as_native_pointer" ->
    Some (int_as_pointer (one_arg name args) dbg)
  | "caml_native_pointer_of_value" ->
    Some (int_of_value (one_arg name args) dbg)
  | "caml_native_pointer_to_value" ->
    Some (value_of_int (one_arg name args) dbg)
  | "caml_native_pointer_load_immediate"
  | "caml_native_pointer_load_unboxed_nativeint" ->
    Some (Cop (Cload (Word_int, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_immediate"
  | "caml_native_pointer_store_unboxed_nativeint" ->
    Some
      (return_unit dbg
         (Cop (Cstore (Word_int, Assignment), args, Op_debuginfo.create dbg)))
  | "caml_native_pointer_load_unboxed_int64" when size_int = 8 ->
    Some (Cop (Cload (Word_int, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_unboxed_int64" when size_int = 8 ->
    Some
      (return_unit dbg
         (Cop (Cstore (Word_int, Assignment), args, Op_debuginfo.create dbg)))
  | "caml_native_pointer_load_signed_int32"
  | "caml_native_pointer_load_unboxed_int32" ->
    Some
      (Cop (Cload (Thirtytwo_signed, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_signed_int32"
  | "caml_native_pointer_store_unboxed_int32" ->
    Some
      (return_unit dbg
         (Cop
            ( Cstore (Thirtytwo_signed, Assignment),
              args,
              Op_debuginfo.create dbg )))
  | "caml_native_pointer_load_unsigned_int32" ->
    Some
      (Cop (Cload (Thirtytwo_unsigned, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_unsigned_int32" ->
    Some
      (return_unit dbg
         (Cop
            ( Cstore (Thirtytwo_unsigned, Assignment),
              args,
              Op_debuginfo.create dbg )))
  | "caml_native_pointer_load_unboxed_float" ->
    Some (Cop (Cload (Double, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_unboxed_float" ->
    Some
      (return_unit dbg
         (Cop (Cstore (Double, Assignment), args, Op_debuginfo.create dbg)))
  | "caml_native_pointer_load_unsigned_int8" ->
    Some (Cop (Cload (Byte_unsigned, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_load_signed_int8" ->
    Some (Cop (Cload (Byte_signed, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_load_unsigned_int16" ->
    Some
      (Cop (Cload (Sixteen_unsigned, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_load_signed_int16" ->
    Some (Cop (Cload (Sixteen_signed, Mutable), args, Op_debuginfo.create dbg))
  | "caml_native_pointer_store_unsigned_int8" ->
    Some
      (return_unit dbg
         (Cop (Cstore (Byte_unsigned, Assignment), args, Op_debuginfo.create dbg)))
  | "caml_native_pointer_store_signed_int8" ->
    Some
      (return_unit dbg
         (Cop (Cstore (Byte_signed, Assignment), args, Op_debuginfo.create dbg)))
  | "caml_native_pointer_store_unsigned_int16" ->
    Some
      (return_unit dbg
         (Cop
            ( Cstore (Sixteen_unsigned, Assignment),
              args,
              Op_debuginfo.create dbg )))
  | "caml_native_pointer_store_signed_int16" ->
    Some
      (return_unit dbg
         (Cop
            (Cstore (Sixteen_signed, Assignment), args, Op_debuginfo.create dbg)))
  (* Ext_pointer: handled as tagged int *)
  | "caml_ext_pointer_load_immediate"
  | "caml_ext_pointer_load_unboxed_nativeint" ->
    ext_pointer_load Word_int name args dbg
  | "caml_ext_pointer_store_immediate"
  | "caml_ext_pointer_store_unboxed_nativeint" ->
    ext_pointer_store Word_int name args dbg
  | "caml_ext_pointer_load_unboxed_int64" when size_int = 8 ->
    ext_pointer_load Word_int name args dbg
  | "caml_ext_pointer_store_unboxed_int64" when size_int = 8 ->
    ext_pointer_store Word_int name args dbg
  | "caml_ext_pointer_load_signed_int32" | "caml_ext_pointer_load_unboxed_int32"
    ->
    ext_pointer_load Thirtytwo_signed name args dbg
  | "caml_ext_pointer_store_signed_int32"
  | "caml_ext_pointer_store_unboxed_int32" ->
    ext_pointer_store Thirtytwo_signed name args dbg
  | "caml_ext_pointer_load_unsigned_int32" ->
    ext_pointer_load Thirtytwo_unsigned name args dbg
  | "caml_ext_pointer_store_unsigned_int32" ->
    ext_pointer_store Thirtytwo_unsigned name args dbg
  | "caml_ext_pointer_load_unboxed_float" ->
    ext_pointer_load Double name args dbg
  | "caml_ext_pointer_store_unboxed_float" ->
    ext_pointer_store Double name args dbg
  | "caml_ext_pointer_load_unsigned_int8" ->
    ext_pointer_load Byte_unsigned name args dbg
  | "caml_ext_pointer_load_signed_int8" ->
    ext_pointer_load Byte_signed name args dbg
  | "caml_ext_pointer_load_unsigned_int16" ->
    ext_pointer_load Sixteen_unsigned name args dbg
  | "caml_ext_pointer_load_signed_int16" ->
    ext_pointer_load Sixteen_signed name args dbg
  | "caml_ext_pointer_store_unsigned_int8" ->
    ext_pointer_store Byte_unsigned name args dbg
  | "caml_ext_pointer_store_signed_int8" ->
    ext_pointer_store Byte_signed name args dbg
  | "caml_ext_pointer_store_unsigned_int16" ->
    ext_pointer_store Sixteen_unsigned name args dbg
  | "caml_ext_pointer_store_signed_int16" ->
    ext_pointer_store Sixteen_signed name args dbg
  (* Bigstring prefetch *)
  | "caml_prefetch_write_high_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true High args dbg
  | "caml_prefetch_write_moderate_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Moderate args dbg
  | "caml_prefetch_write_low_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Low args dbg
  | "caml_prefetch_write_none_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Nonlocal args dbg
  | "caml_prefetch_read_none_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Nonlocal args dbg
  | "caml_prefetch_read_high_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false High args dbg
  | "caml_prefetch_read_moderate_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Moderate args dbg
  | "caml_prefetch_read_low_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Low args dbg
  (* Ext_pointer prefetch *)
  | "caml_prefetch_write_high_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true High (one_arg name args) dbg
  | "caml_prefetch_write_moderate_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Moderate (one_arg name args) dbg
  | "caml_prefetch_write_low_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Low (one_arg name args) dbg
  | "caml_prefetch_write_none_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_none_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_high_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false High (one_arg name args) dbg
  | "caml_prefetch_read_moderate_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Moderate (one_arg name args) dbg
  | "caml_prefetch_read_low_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Low (one_arg name args) dbg
  (* Value and unboxed Native_pointer prefetch *)
  | "caml_prefetch_write_high" ->
    prefetch ~is_write:true High (one_arg name args) dbg
  | "caml_prefetch_write_moderate" ->
    prefetch ~is_write:true Moderate (one_arg name args) dbg
  | "caml_prefetch_write_low" ->
    prefetch ~is_write:true Low (one_arg name args) dbg
  | "caml_prefetch_write_none" ->
    prefetch ~is_write:true Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_none" ->
    prefetch ~is_write:false Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_high" ->
    prefetch ~is_write:false High (one_arg name args) dbg
  | "caml_prefetch_read_moderate" ->
    prefetch ~is_write:false Moderate (one_arg name args) dbg
  | "caml_prefetch_read_low" ->
    prefetch ~is_write:false Low (one_arg name args) dbg
  (* Prefetch value with offset *)
  | "caml_prefetch_write_high_val_offset_untagged" ->
    prefetch_offset ~is_write:true High (two_args name args) dbg
  | "caml_prefetch_write_moderate_val_offset_untagged" ->
    prefetch_offset ~is_write:true Moderate (two_args name args) dbg
  | "caml_prefetch_write_low_val_offset_untagged" ->
    prefetch_offset ~is_write:true Low (two_args name args) dbg
  | "caml_prefetch_write_none_val_offset_untagged" ->
    prefetch_offset ~is_write:true Nonlocal (two_args name args) dbg
  | "caml_prefetch_read_none_val_offset_untagged" ->
    prefetch_offset ~is_write:false Nonlocal (two_args name args) dbg
  | "caml_prefetch_read_high_val_offset_untagged" ->
    prefetch_offset ~is_write:false High (two_args name args) dbg
  | "caml_prefetch_read_moderate_val_offset_untagged" ->
    prefetch_offset ~is_write:false Moderate (two_args name args) dbg
  | "caml_prefetch_read_low_val_offset_untagged" ->
    prefetch_offset ~is_write:false Low (two_args name args) dbg
  (* Atomics *)
  | "caml_native_pointer_fetch_and_add_nativeint_unboxed"
  | "caml_native_pointer_fetch_and_add_int_untagged" ->
    native_pointer_atomic_add Word (two_args name args) dbg
  | "caml_native_pointer_fetch_and_add_int64_unboxed" when size_int = 8 ->
    native_pointer_atomic_add Sixtyfour (two_args name args) dbg
  | "caml_native_pointer_fetch_and_add_int32_unboxed" ->
    native_pointer_atomic_add Thirtytwo (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_nativeint_unboxed"
  | "caml_ext_pointer_fetch_and_add_int_untagged" ->
    ext_pointer_atomic_add Word (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_int64_unboxed" when size_int = 8 ->
    ext_pointer_atomic_add Sixtyfour (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_int32_unboxed" ->
    ext_pointer_atomic_add Thirtytwo (two_args name args) dbg
  | "caml_bigstring_fetch_and_add_nativeint_unboxed"
  | "caml_bigstring_fetch_and_add_int_untagged" ->
    bigstring_atomic_add Word (three_args name args) dbg
  | "caml_bigstring_fetch_and_add_int64_unboxed" when size_int = 8 ->
    bigstring_atomic_add Sixtyfour (three_args name args) dbg
  | "caml_bigstring_fetch_and_add_int32_unboxed" ->
    bigstring_atomic_add Thirtytwo (three_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_nativeint_unboxed"
  | "caml_native_pointer_fetch_and_sub_int_untagged" ->
    native_pointer_atomic_sub Word (two_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    native_pointer_atomic_sub Sixtyfour (two_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_int32_unboxed" ->
    native_pointer_atomic_sub Thirtytwo (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_nativeint_unboxed"
  | "caml_ext_pointer_fetch_and_sub_int_untagged" ->
    ext_pointer_atomic_sub Word (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    ext_pointer_atomic_sub Sixtyfour (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_int32_unboxed" ->
    ext_pointer_atomic_sub Thirtytwo (two_args name args) dbg
  | "caml_bigstring_fetch_and_sub_nativeint_unboxed"
  | "caml_bigstring_fetch_and_sub_int_untagged" ->
    bigstring_atomic_sub Word (three_args name args) dbg
  | "caml_bigstring_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    bigstring_atomic_sub Sixtyfour (three_args name args) dbg
  | "caml_bigstring_fetch_and_sub_int32_unboxed" ->
    bigstring_atomic_sub Thirtytwo (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int_untagged"
  | "caml_native_pointer_compare_and_swap_nativeint_unboxed" ->
    native_pointer_cas Word (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int64_unboxed" when size_int = 8 ->
    native_pointer_cas Sixtyfour (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int32_unboxed" ->
    native_pointer_cas Thirtytwo (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int_untagged"
  | "caml_ext_pointer_compare_and_swap_nativeint_unboxed" ->
    ext_pointer_cas Word (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int64_unboxed" when size_int = 8 ->
    ext_pointer_cas Sixtyfour (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int32_unboxed" ->
    ext_pointer_cas Thirtytwo (three_args name args) dbg
  | "caml_bigstring_compare_and_swap_int_untagged"
  | "caml_bigstring_compare_and_swap_nativeint_unboxed" ->
    bigstring_cas Word (four_args name args) dbg
  | "caml_bigstring_compare_and_swap_int64_unboxed" when size_int = 8 ->
    bigstring_cas Sixtyfour (four_args name args) dbg
  | "caml_bigstring_compare_and_swap_int32_unboxed" ->
    bigstring_cas Thirtytwo (four_args name args) dbg
  | _ -> None

let transl_effects (e : Primitive.effects) : Cmm.effects =
  match e with
  | No_effects -> No_effects
  | Only_generative_effects | Arbitrary_effects -> Arbitrary_effects

let transl_coeffects (ce : Primitive.coeffects) : Cmm.coeffects =
  match ce with No_coeffects -> No_coeffects | Has_coeffects -> Has_coeffects

(* [cextcall] is called from [Cmmgen.transl_ccall] *)
let cextcall (prim : Primitive.description) args dbg ret ty_args returns =
  let name = Primitive.native_name prim in
  let default =
    Cop
      ( Cextcall
          { func = name;
            ty = ret;
            builtin = prim.prim_c_builtin;
            effects = transl_effects prim.prim_effects;
            coeffects = transl_coeffects prim.prim_coeffects;
            alloc = prim.prim_alloc;
            returns;
            ty_args
          },
        args,
        Op_debuginfo.create dbg )
  in
  if prim.prim_c_builtin
  then
    match transl_builtin name args dbg ret with
    | Some op -> op
    | None -> default
  else default

let extcall ~dbg ~returns ~alloc ~is_c_builtin ~ty_args name typ_res args =
  if not returns then assert (typ_res = typ_void);
  let default =
    Cop
      ( Cextcall
          { func = name;
            ty = typ_res;
            alloc;
            ty_args;
            returns;
            builtin = is_c_builtin;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects
          },
        args,
        Op_debuginfo.create dbg )
  in
  if is_c_builtin
  then
    match transl_builtin name args dbg typ_res with
    | Some op -> op
    | None -> default
  else default
