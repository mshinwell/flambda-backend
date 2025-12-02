open Arm64_ast

type reloc_type =
  | ADR
  | ADRP
  | GOT_PAGE_OFF
  | PAGE_OFF

type relocation =
  { offset_bytes : int;
    symbol_name : string;
    reloc_type : reloc_type
  }
[@@warning "-69"]

let current_offset_bytes : int ref = ref 0

let symbol_definitions : (string, int) Hashtbl.t = Hashtbl.create 64

let pending_relocations : relocation list ref = ref []

let add_relocation ~offset_bytes ~symbol_name ~reloc_type =
  pending_relocations
    := { offset_bytes; symbol_name; reloc_type } :: !pending_relocations

let _get_relocations () = List.rev !pending_relocations

let _clear_relocations () = pending_relocations := []

let _encode_shift_type (type op) (kind : op Operand.Shift.Kind.t) =
  match kind with LSL -> 0b00 | LSR -> 0b01 | ASR -> 0b10

let _encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12 ~rn ~rd =
  let open Int32 in
  if imm12 < 0 || imm12 > 4095
  then Misc.fatal_errorf "ADD/SUB immediate out of range: %d" imm12 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b100010) 23) in
  let result = logor result (shift_left (of_int sh) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

let _encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let max_shift = if sf = 1 then 63 else 31 in
  if imm6 < 0 || imm6 > max_shift
  then Misc.fatal_errorf "ADD/SUB shift amount out of range: %d" imm6 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b01011) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Logical (immediate) encoding - C4.1.92.6 Used for AND, ORR, EOR, ANDS with
   bitmask immediates *)
let _encode_logical_immediate ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100100) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Move wide (immediate) encoding - C4.1.92.7 Used for MOVN, MOVZ, MOVK *)
let encode_move_wide ~sf ~opc ~hw ~imm16 ~rd =
  let open Int32 in
  if imm16 < 0 || imm16 > 0xFFFF
  then Misc.fatal_errorf "MOVZ/MOVN/MOVK immediate out of range: %d" imm16 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100101) 23) in
  let result = logor result (shift_left (of_int hw) 21) in
  let result = logor result (shift_left (of_int imm16) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Bitfield encoding - C4.1.92.8 Used for SBFM, BFM, UBFM *)
let encode_bitfield ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100110) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (2 source) - C4.1.94.1 *)
let encode_data_proc_2_source ~sf ~s ~opcode ~rm ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (3 source) - C4.1.94.13 *)
let encode_data_proc_3_source ~sf ~op54 ~op31 ~o0 ~rm ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op54) 29) in
  let result = logor result (shift_left (of_int 0b11011) 24) in
  let result = logor result (shift_left (of_int op31) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding ra)) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Data-processing (1 source) - C4.1.94.2 *)
let encode_data_proc_1_source ~sf ~s ~opcode2 ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b1) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b11010110) 21) in
  let result = logor result (shift_left (of_int opcode2) 16) in
  let result = logor result (shift_left (of_int opcode) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Advanced SIMD three same - C4.1.95.24 *)
let _encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 31) in
  let result = logor result (shift_left (of_int u) 30) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point immediate - C4.1.95.36 *)
let _encode_fp_immediate ~ftype ~imm8 ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int imm8) 13) in
  let result = logor result (shift_left (of_int 0b100) 10) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (2 source) - C4.1.95.38 *)
let _encode_fp_2_source ~ftype ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point conditional select - C4.1.95.39 *)
let _encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (3 source) - C4.1.95.40 *)
let _encode_fp_3_source ~ftype ~o1 ~rm ~o0 ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11111) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int o1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int ra) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

let _encode_condition (cond : Cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | MI -> 0b0100
  | PL -> 0b0101
  | VS -> 0b0110
  | VC -> 0b0111
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101

(* Logical (shifted register) - C4.1.94.3 *)
let _encode_logical_shifted_register ~sf ~opc ~shift ~n ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b01010) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int n) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Load register (literal) - C4.1.96.19 *)
let encode_load_literal ~opc ~v ~imm19 ~rt =
  assert (imm19 >= 0 && imm19 <= 0x7ffff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b011) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int imm19) 5) in
  let result = logor result (of_int rt) in
  result

(* PC-relative addressing - C4.1.92.2 *)
let encode_adr ~op ~immlo ~immhi ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int immlo) 29) in
  let result = logor result (shift_left (of_int 0b10000) 24) in
  let result = logor result (shift_left (of_int immhi) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

let split_21bit_immediate (imm21 : int) : int * int =
  if imm21 < -0x100000 || imm21 > 0xfffff
  then
    Misc.fatal_errorf
      "Immediate value %d (0x%x) out of range for 21-bit signed immediate (max \
       ±0x100000)"
      imm21 imm21;
  let immlo = imm21 land 0b11 in
  let immhi = (imm21 lsr 2) land 0x7ffff in
  immlo, immhi

(* Logical (immediate) - C4.1.92.6 *)
let encode_logical_immediate ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100100) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Add/subtract (immediate) - C4.1.92.3 *)
let encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b100010) 23) in
  let result = logor result (shift_left (of_int sh) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

let encode_six_bit_shift (shift_opt : [`Shift of _ * [`Six]] Operand.t option) =
  match shift_opt with
  | Some (Shift shift) -> (match shift.amount with Six n -> n) / 16
  | None -> 0

(* Load/store register (unscaled immediate) - C4.1.96.25 *)
let encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate post-indexed) - C4.1.96.26 *)
let encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b01) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate pre-indexed) - C4.1.96.28 *)
let encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (unsigned immediate) - C4.1.96.27 *)
let encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt =
  assert (imm12 >= 0 && imm12 <= 0xfff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int 0b01) 24) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (post-indexed) - C4.1.96.15 Encoding: opc[1:0] | 101 | V |
   001 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_post_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b001) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (pre-indexed) - C4.1.96.17 Encoding: opc[1:0] | 101 | V | 011
   | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_pre_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b011) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (signed offset) - C4.1.96.16 Encoding: opc[1:0] | 101 | V |
   010 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_signed_offset ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b010) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

let encode_load_store_gp :
    type a.
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    Operand.Addressing_mode.t ->
    int32 =
 fun ~instr_name ~opc ~rd addressing ->
  let size =
    match rd.reg_name with
    | GP W | GP WZR | GP WSP -> 0b10
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b11
  in
  let vr = 0 in
  let rt = Reg.gp_encoding rd in
  match addressing with
  | Reg rn ->
    let rn = Reg.gp_encoding rn in
    encode_load_store_unscaled ~size ~vr ~opc ~imm9:0 ~rn ~rt
  | Literal (_rn, sym) -> (
    (* This is encoded as "LDR (literal)" (ARMARM C6.2.192) *)
    match Hashtbl.find_opt symbol_definitions sym.name with
    | None ->
      Misc.fatal_errorf "%s (literal) references undefined symbol '%s' (rd=%s)"
        instr_name sym.name (Reg.name rd)
    | Some target_offset ->
      (* XXX what do we do about forward references? *)
      let pc_relative_offset = target_offset - !current_offset_bytes in
      assert (pc_relative_offset > 0);
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' must be 4-byte aligned"
          instr_name pc_relative_offset sym.name;
      let imm19_unmasked = pc_relative_offset / 4 in
      if imm19_unmasked < -0x40000 || imm19_unmasked > 0x3ffff
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%s' out of range (max ±1MB)"
          instr_name pc_relative_offset sym.name;
      let imm19 = imm19_unmasked land 0x7FFFF in
      let v = 0 in
      encode_load_literal ~opc ~v ~imm19 ~rt)
  | Offset (rn, Imm (Twelve_unsigned_scaled imm12)) ->
    let max_imm12 = 0xfff in
    let reg_size_bytes = if size = 0b11 then 8 else 4 in
    if imm12 mod reg_size_bytes <> 0
    then
      Misc.fatal_errorf
        "%s offset %d must be aligned to %d-byte register size (rd=%s, rn=%s)"
        instr_name imm12 reg_size_bytes (Reg.name rd) (Reg.name rn);
    let rn = Reg.gp_encoding rn in
    let imm12_scaled = imm12 / reg_size_bytes in
    if imm12_scaled < 0 || imm12_scaled > max_imm12
    then
      Misc.fatal_errorf
        "%s offset %d (scaled: %d) out of range (max 0x%x * %d = 0x%x bytes)"
        instr_name imm12 imm12_scaled max_imm12 reg_size_bytes
        (max_imm12 * reg_size_bytes);
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:imm12_scaled ~rn ~rt
  | Offset (rn, Symbol_with_reloc sym) -> (
    match sym.reloc with
    | Different_section reloc ->
      let max_imm12 = 0xfff in
      let reloc_type =
        match reloc with
        | GOT_PAGE_OFF -> GOT_PAGE_OFF
        | PAGE_OFF -> PAGE_OFF
        | LOWER_TWELVE -> PAGE_OFF
        | GOT_LOWER_TWELVE -> GOT_PAGE_OFF
      in
      add_relocation ~offset_bytes:!current_offset_bytes ~symbol_name:sym.name
        ~reloc_type;
      let rn = Reg.gp_encoding rn in
      let shift = if size = 0b11 then 3 else 2 in
      let imm12_unmasked = sym.offset lsr shift in
      if imm12_unmasked < 0 || imm12_unmasked > max_imm12
      then
        Misc.fatal_errorf
          "%s symbol offset %d (shifted by %d) out of range (max 0x%x)"
          instr_name sym.offset shift max_imm12;
      let imm12 = imm12_unmasked land 0xFFF in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt)
  | Pre (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Post (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Offset_pair _ | Pre_pair _ | Post_pair _ ->
    Misc.fatal_errorf
      "%s: pair addressing modes not supported for single-register load/store"
      instr_name

(* Encode LDP/STP instructions for GP registers. l=1 for load (LDP), l=0 for
   store (STP). opc: 00 for 32-bit (W), 10 for 64-bit (X) *)
let encode_load_store_pair_gp :
    type a b.
    instr_name:string ->
    l:int ->
    rt1:[`GP of a] Reg.t ->
    rt2:[`GP of b] Reg.t ->
    Operand.Addressing_mode.t ->
    int32 =
 fun ~instr_name ~l ~rt1 ~rt2 addressing ->
  let opc =
    match rt1.reg_name with
    | GP W | GP WZR | GP WSP -> 0b00
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b10
  in
  let scale = if opc = 0b10 then 8 else 4 in
  let v = 0 in
  let rt1_enc = Reg.gp_encoding rt1 in
  let rt2_enc = Reg.gp_encoding rt2 in
  let encode_with_alignment_check encode_fn rn imm =
    if imm mod scale <> 0
    then
      Misc.fatal_errorf "%s offset %d must be aligned to %d bytes" instr_name
        imm scale;
    let imm7 = imm / scale in
    let rn = Reg.gp_encoding rn in
    encode_fn ~opc ~v ~l ~imm7 ~rt2:rt2_enc ~rn ~rt:rt1_enc
  in
  match addressing with
  | Offset_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_signed_offset rn imm
  | Pre_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_pre_indexed rn imm
  | Post_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_post_indexed rn imm
  | Reg _ | Literal _ | Offset _ | Pre _ | Post _ ->
    Misc.fatal_errorf
      "%s: single-register addressing modes not supported for pair load/store"
      instr_name

let encode_instruction :
    type num operands.
    (num, operands) Instruction_name.t -> (num, operands) many -> int32 =
 fun instr operands ->
  match operands, instr with
  | Pair (Reg _rd, Reg _rn), ABS_vector -> assert false
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADD_immediate ->
    let sh = match shift with Some _ -> 1 | None -> 0 in
    encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh ~imm12 ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), ADD_shifted_register ->
    assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), ADDP_vector -> assert false
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADDS ->
    let sh = match shift with Some _ -> 1 | None -> 0 in
    encode_add_sub_immediate ~sf:1 ~op:0 ~s:1 ~sh ~imm12 ~rn ~rd
  | Triple (Reg _rd, Reg _rn, Reg _rm), ADD_vector -> assert false
  | Pair (Reg _rd, Reg _rn), ADDV -> assert false
  | Pair (Reg rd, Imm (Sym sym)), ADR ->
    add_relocation ~offset_bytes:!current_offset_bytes ~symbol_name:sym.name
      ~reloc_type:ADR;
    let immlo, immhi = split_21bit_immediate sym.offset in
    encode_adr ~op:0 ~immlo ~immhi ~rd
  | Pair (Reg rd, Imm (Sym sym)), ADRP ->
    add_relocation ~offset_bytes:!current_offset_bytes ~symbol_name:sym.name
      ~reloc_type:ADRP;
    let immlo, immhi = split_21bit_immediate sym.offset in
    encode_adr ~op:1 ~immlo ~immhi ~rd
  | Triple (Reg rd, Reg rn, Bitmask bitmask), AND_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b00 ~n ~immr ~imms ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), AND_shifted_register ->
    assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), AND_vector -> assert false
  | Triple (Reg rd, Reg rn, Reg rm), ASRV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001010 ~rm ~rn ~rd
  | Singleton (Imm _), B -> assert false
  | Singleton _, B_cond _ -> assert false
  | Singleton _, B_cond_float _ -> assert false
  | Singleton _, BL -> assert false
  | Singleton (Reg _), BLR -> assert false
  | Singleton (Reg _), BR -> assert false
  | Pair (Reg _rd, Imm _), CBNZ -> assert false
  | Pair (Reg _rd, Imm _), CBZ -> assert false
  | Pair (Reg rd, Reg rn), CLZ ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000100 ~rn ~rd
  | Triple (Reg _rd, Reg _rn, Reg _rm), CM_register _ -> assert false
  | Pair (Reg _rd, Reg _rn), CM_zero _ -> assert false
  | Pair (Reg rd, Reg rn), CNT ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000111 ~rn ~rd
  | Pair (Reg _rd, Reg _rn), CNT_vector -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), CSEL -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), CSINC -> assert false
  | Pair (Reg rd, Reg rn), CTZ ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000110 ~rn ~rd
  | Pair (Reg _rd, Reg _rn), CVT_vector -> assert false
  | _, DMB _ -> assert false
  | _, DSB _ -> assert false
  | Pair (Reg _rd, Reg _rn), DUP _ -> assert false
  | Triple (Reg rd, Reg rn, Bitmask bitmask), EOR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b10 ~n ~immr ~imms ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), EOR_shifted_register ->
    assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), EOR_vector -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Imm _), EXT -> assert false
  | Pair (Reg _rd, Reg _rn), FABS -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FADD -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FADDP_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FADD_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FCM_register _ -> assert false
  | Pair (Reg _rd, Reg _rn), FCM_zero _ -> assert false
  | Pair (Reg _rd, Reg _rn), FCMP -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), FCSEL -> assert false
  | Pair (Reg _rd, Reg _rn), FCVT -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTL_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTN_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTNS -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTNS_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTZS -> assert false
  | Pair (Reg _rd, Reg _rn), FCVTZS_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FDIV -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FDIV_vector -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FMADD -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMAX -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMAX_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMIN -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMIN_vector -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FMSUB -> assert false
  | Pair (Reg _rd, Reg _rn), FMOV_general_or_register -> assert false
  | Pair (Reg _rd, _), FMOV_scalar_immediate -> assert false
  | Pair (Reg _rd, _), FMOV_vector_immediate -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMUL -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FMUL_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FNEG -> assert false
  | Pair (Reg _rd, Reg _rn), FNEG_vector -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FNMADD -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FNMUL -> assert false
  | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FNMSUB -> assert false
  | Pair (Reg _rd, Reg _rn), FRECPE_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FRINT _ -> assert false
  | Pair (Reg _rd, Reg _rn), FRINT_vector _ -> assert false
  | Pair (Reg _rd, Reg _rn), FRSQRTE_vector -> assert false
  | Pair (Reg _rd, Reg _rn), FSQRT -> assert false
  | Pair (Reg _rd, Reg _rn), FSQRT_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FSUB -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), FSUB_vector -> assert false
  | Pair (Reg _rd, Reg _rn), INS _ -> assert false
  | Pair (Reg _rd, Reg _rn), INS_V _ -> assert false
  | Pair (Reg _rd, Mem _addressing), LDAR -> assert false
  | Triple (Reg rt1, Reg rt2, Mem addressing), LDP ->
    encode_load_store_pair_gp ~instr_name:"LDP" ~l:1 ~rt1 ~rt2 addressing
  | Pair (Reg rd, Mem addressing), LDR ->
    encode_load_store_gp ~instr_name:"LDR" ~opc:0b01 ~rd addressing
  | Pair (Reg _rd, Mem _addressing), LDR_simd_and_fp -> assert false
  | Pair (Reg _rd, Mem _addressing), LDRB -> assert false
  | Pair (Reg _rd, Mem _addressing), LDRH -> assert false
  | Pair (Reg _rd, Mem _addressing), LDRSB -> assert false
  | Pair (Reg _rd, Mem _addressing), LDRSH -> assert false
  | Pair (Reg _rd, Mem _addressing), LDRSW -> assert false
  | Triple (Reg rd, Reg rn, Reg rm), LSLV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001000 ~rm ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), LSRV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001001 ~rm ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MADD ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:0 ~rm ~ra ~rn ~rd
  | Pair (Reg _rd, Imm _), MOV -> assert false
  | Pair (Reg _rd, Reg _rn), MOV -> assert false
  | Pair (Reg _rd, Reg _rn), MOV_vector -> assert false
  | Pair (Reg _rd, Imm _), MOVI -> assert false
  | Triple (Reg rd, Imm imm, Shift shift), MOVK ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = (match shift.amount with Six n -> n) / 16 in
    encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVN ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = encode_six_bit_shift shift_opt in
    encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVZ ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = encode_six_bit_shift shift_opt in
    encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MSUB ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:1 ~rm ~ra ~rn ~rd
  | Triple (Reg _rd, Reg _rn, Reg _rm), MULL_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), MUL_vector -> assert false
  | Pair (Reg _rd, Reg _rn), MVN_vector -> assert false
  | Pair (Reg _rd, Reg _rn), NEG_vector -> assert false
  | _, NOP -> assert false
  | Triple (Reg rd, Reg rn, Bitmask bitmask), ORR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    encode_logical_immediate ~sf:1 ~opc:0b01 ~n ~immr ~imms ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), ORR_shifted_register ->
    assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), ORR_vector -> assert false
  | Pair (Reg rd, Reg rn), RBIT ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000000 ~rn ~rd
  | _, RET -> assert false
  | Pair (Reg rd, Reg rn), REV ->
    let sf = Reg.gp_sf rd in
    let opcode = if sf = 1 then 0b000011 else 0b000010 in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode ~rn ~rd
  | Pair (Reg rd, Reg rn), REV16 ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000 ~opcode:0b000001 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), SBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    encode_bitfield ~sf ~opc:0b00 ~n ~immr ~imms ~rn ~rd
  | Pair (Reg _rd, Reg _rn), SCVTF -> assert false
  | Pair (Reg _rd, Reg _rn), SCVTF_vector -> assert false
  | Triple (Reg rd, Reg rn, Reg rm), SDIV ->
    let sf = Reg.gp_sf rd in
    encode_data_proc_2_source ~sf ~s:0 ~opcode:0b000011 ~rm ~rn ~rd
  | Triple (Reg _rd, Reg _rn, Imm _), SHL -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SMAX_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SMIN_vector -> assert false
  | Pair (Reg _rd, Reg _rn), SMOV _ -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SMULH -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SMULL2_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SMULL_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SQADD_vector -> assert false
  | Pair (Reg _rd, Reg _rn), SQXTN -> assert false
  | Pair (Reg _rd, Reg _rn), SQXTN2 -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SQSUB_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SSHL_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Imm _), SSHR -> assert false
  | Triple (Reg rt1, Reg rt2, Mem addressing), STP ->
    encode_load_store_pair_gp ~instr_name:"STP" ~l:0 ~rt1 ~rt2 addressing
  | Pair (Reg rd, Mem addressing), STR ->
    encode_load_store_gp ~instr_name:"STR" ~opc:0b00 ~rd addressing
  | Pair (Reg _rd, Mem _addressing), STR_simd_and_fp -> assert false
  | Pair (Reg _rd, Mem _addressing), STRB -> assert false
  | Pair (Reg _rd, Mem _addressing), STRH -> assert false
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUB_immediate ->
    let sh = match shift with Some _ -> 1 | None -> 0 in
    encode_add_sub_immediate ~sf:1 ~op:1 ~s:0 ~sh ~imm12 ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), SUB_shifted_register ->
    assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), SUB_vector -> assert false
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUBS_immediate ->
    let sh = match shift with Some _ -> 1 | None -> 0 in
    encode_add_sub_immediate ~sf:1 ~op:1 ~s:1 ~sh ~imm12 ~rn ~rd
  | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), SUBS_shifted_register ->
    assert false
  | Pair (Reg _rd, Reg _rn), SXTL -> assert false
  | Triple (Reg _rd, Imm _, Imm _), TBNZ -> assert false
  | Triple (Reg _rd, Imm _, Imm _), TBZ -> assert false
  | Pair (Reg _rd, Bitmask _), TST -> assert false
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), UBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    encode_bitfield ~sf ~opc:0b10 ~n ~immr ~imms ~rn ~rd
  | Pair (Reg _rd, Reg _rn), UADDLP_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UMAX_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UMIN_vector -> assert false
  | Pair (Reg _rd, Reg _rn), UMOV _ -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UMULH -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UMULL2_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UMULL_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UQADD_vector -> assert false
  | Pair (Reg _rd, Reg _rn), UQXTN -> assert false
  | Pair (Reg _rd, Reg _rn), UQXTN2 -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), UQSUB_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), USHL_vector -> assert false
  | Triple (Reg _rd, Reg _rn, Imm _), USHR -> assert false
  | Pair (Reg _rd, Reg _rn), UXTL -> assert false
  | Pair (Reg _rd, Reg _rn), XTN -> assert false
  | Pair (Reg _rd, Reg _rn), XTN2 -> assert false
  | _, YIELD -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), ZIP1 -> assert false
  | Triple (Reg _rd, Reg _rn, Reg _rm), ZIP2 -> assert false
