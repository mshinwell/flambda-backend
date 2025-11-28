(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 Jane Street Group LLC                              *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* We disable warning 37 ("constructor X is never used to build values") in a
   few places to allow various types below to be a full description of what is
   permitted by the architecture, even though we don't yet use all of them. *)

open! Int_replace_polymorphic_compare

let check_index first last index =
  if index < first || index > last
  then Misc.fatal_errorf "Illegal register index %d" index ()

type any_vector =
  [ `V8B
  | `V16B
  | `V4H
  | `V8H
  | `V2S
  | `V4S
  | `V1D
  | `V2D ]

type any_width =
  [ `B
  | `H
  | `S
  | `D ]

(* Float/SIMD register description *)
module Neon_reg_name = struct
  module Vector = struct
    type (_, _) t =
      | V8B : ([`V8B], [`B]) t
      | V16B : ([`V16B], [`B]) t
      | V4H : ([`V4H], [`H]) t
      | V8H : ([`V8H], [`H]) t
      | V2S : ([`V2S], [`S]) t
      | V4S : ([`V4S], [`S]) t
      | V1D : ([`V1D], [`D]) t
      | V2D : ([`V2D], [`D]) t
    [@@ocaml.warning "-37"]

    let to_string (type v s) (t : (v, s) t) =
      match t with
      | V8B -> "8B"
      | V16B -> "16B"
      | V4H -> "4H"
      | V8H -> "8H"
      | V2S -> "2S"
      | V4S -> "4S"
      | V1D -> "1D"
      | V2D -> "2D"

    let num_lanes (type v s) (t : (v, s) t) =
      match t with
      | V8B -> 8
      | V16B -> 16
      | V4H -> 4
      | V8H -> 8
      | V2S -> 2
      | V4S -> 4
      | V1D -> 1
      | V2D -> 2

    let name t index = Printf.sprintf "V%d.%s" index (to_string t)

    type _ testi = I : [< `V8B | `V16B] testi

    type _ testo =
      | O_V8B : [`V8B] testo
      | O_V16B : [`V16B] testo
      | O_V4H : [`V4H] testo

    let foo : type v. v testi -> v testo -> int =
     fun i o -> match i, o with I, O_V8B -> 42 | I, O_V16B -> 42
  end

  (* module Scalar_vector_equality = struct type ('scalar, 'vector) t = |
     Equal_B : ([`B], [`V8B | `V16B]) t | Equal_H : ([`H], [`V4H | `V8H]) t |
     Equal_S : ([`S], [`V2S | `V4S]) t | Equal_D : ([`D], [`V1D | `V2D]) t

     (* let create (type a b) (s : a Scalar.t) (v : b Vector.t) : (a Scalar.t *
     b Vector.t) t = match[@warning "-4"] s, v with | B, (V8B | V16B) -> Equal_B
     | H, (V4H | V8H) -> Equal_H | S, (V2S | V4S) -> Equal_S | D, (V1D | V2D) ->
     Equal_D | _, _ -> assert false *) end *)

  module Scalar = struct
    type _ t =
      | B : [`B] t
      | H : [`H] t
      | S : [`S] t
      | D : [`D] t
      | Q : [`Q] t

    let num_lanes : type a. a t -> int = function
      | B -> 16
      | H -> 8
      | S -> 4
      | D -> 2
      | Q -> 1

    let to_string : type a. a t -> string = function
      | B -> "b"
      | H -> "h"
      | S -> "s"
      | D -> "d"
      | Q -> "q"

    let of_vector : type v s. (v, s) Vector.t -> s t =
     fun vec ->
      match vec with
      | V8B -> B
      | V16B -> B
      | V4H -> H
      | V8H -> H
      | V2S -> S
      | V4S -> S
      | V1D -> D
      | V2D -> D

    let name t index = Printf.sprintf "%s%d" (to_string t) index
  end

  module Lane = struct
    (** Support representation with and without the optional number of lanes, for
        example Vn.4S[1] and Vn.S[1]. *)
    type 'a r =
      | V : ('v, 's) Vector.t -> [`Vector of 'v * 's] r
      | S : 's Scalar.t -> [`Scalar of 's] r

    type 'a t =
      { r : 'a r;
        lane : int
      }

    let num_lanes (type a) (r : a r) =
      match r with V v -> Vector.num_lanes v | S s -> Scalar.num_lanes s

    let check_index t =
      let last = num_lanes t.r - 1 in
      check_index 0 last t.lane

    let name (type a) (t : a t) index =
      let suffix =
        match t.r with V v -> Vector.to_string v | S s -> Scalar.to_string s
      in
      Printf.sprintf "V%d.%s[%d]" index suffix t.lane
  end

  module Lane_index = struct
    type t = int

    let create i = i
  end

  type _ t =
    | Vector : ('v, 's) Vector.t -> [`Vector of 'v] t
    | Scalar : 's Scalar.t -> [`Scalar of 's] t
    | Lane : 'l Lane.t -> [`Lane of 'l] t

  let last = 31

  let check_index (type a) (t : a t) index =
    check_index 0 last index;
    match t with Vector _ | Scalar _ -> () | Lane l -> Lane.check_index l

  let name (type a) (t : a t) index =
    match t with
    | Vector v -> Vector.name v index
    | Scalar s -> Scalar.name s index
    | Lane l -> Lane.name l index

  let lane_of_vector vector ~lane =
    let scalar_type = Scalar.of_vector vector in
    Lane { r = S scalar_type; lane }
end

(* General-purpose register description *)
module GP_reg_name = struct
  type _ t =
    | W : [`W] t
    | X : [`X] t
    | WZR : [`WZR] t
    | XZR : [`XZR] t
    | WSP : [`WSP] t
    | SP : [`SP] t
    | LR : [`LR] t
    | FP : [`FP] t
  [@@ocaml.warning "-37"]

  let last_numbered = 30

  let last = 31

  let check_index (type a) (t : a t) index =
    match t with
    | W | X -> check_index 0 last_numbered index
    | WZR | XZR | WSP | SP | LR | FP -> check_index last last index

  let name (type a) (t : a t) index =
    match t with
    | W -> Printf.sprintf "w%d" index
    | X -> Printf.sprintf "x%d" index
    | WZR -> "wzr"
    | XZR -> "xzr"
    | WSP -> "wsp"
    | SP -> "sp"
    | LR -> "lr"
    | FP -> "fp"

  let encoding (type a) (t : a t) index =
    match t with
    | W | X -> index
    | WZR | XZR | WSP | SP -> 31
    | LR -> 30
    | FP -> 29
end

(* Register representation *)
module Reg_name = struct
  type _ t =
    | GP : 'a GP_reg_name.t -> [`GP of 'a] t
    | Neon : 'a Neon_reg_name.t -> [`Neon of 'a] t

  let check_index (type a) (t : a t) index =
    match t with
    | GP rn -> GP_reg_name.check_index rn index
    | Neon rn -> Neon_reg_name.check_index rn index

  let name (type a) (t : a t) index =
    match t with
    | GP rn -> GP_reg_name.name rn index
    | Neon rn -> Neon_reg_name.name rn index

  let encoding (type a) (t : a t) index =
    match t with GP rn -> GP_reg_name.encoding rn index | Neon _ -> index
end

module Reg = struct
  type 'a t =
    { reg_name : 'a Reg_name.t;
      index : int
    }

  let create (type a) (reg_name : a Reg_name.t) index : a t =
    Reg_name.check_index reg_name index;
    { reg_name; index }

  let name (type a) (t : a t) = Reg_name.name t.reg_name t.index

  let encoding (type a) (t : a t) = Reg_name.encoding t.reg_name t.index

  (* for special GP registers we use the last index *)
  (* CR mshinwell: why is this? *)
  let sp () = create (GP SP) GP_reg_name.last

  let lr () = create (GP LR) GP_reg_name.last

  let fp () = create (GP FP) GP_reg_name.last

  let xzr () = create (GP XZR) GP_reg_name.last

  let wzr () = create (GP WZR) GP_reg_name.last

  let reg_x i = create (GP GP_reg_name.X) i

  let reg_w i = create (GP GP_reg_name.W) i

  let reg_s i = create (Neon (Scalar S)) i

  let reg_d i = create (Neon (Scalar D)) i

  let reg_q i = create (Neon (Scalar Q)) i

  let reg_v2d i = create (Neon (Vector V2D)) i

  let reg_v2s i = create (Neon (Vector V2S)) i

  let reg_v4h i = create (Neon (Vector V4H)) i

  let reg_v4s i = create (Neon (Vector V4S)) i

  let reg_v8b i = create (Neon (Vector V8B)) i

  let reg_v8h i = create (Neon (Vector V8H)) i

  let reg_v16b i = create (Neon (Vector V16B)) i

  let reg_b i = create (Neon (Scalar B)) i
end

module Float_cond = struct
  type t =
    | EQ
    | GT
    | LE
    | GE
    | LT
    | NE
    | CC
    | CS
    | LS
    | HI

  let to_string t =
    match t with
    | EQ -> "eq"
    | GT -> "gt"
    | LE -> "le"
    | GE -> "ge"
    | LT -> "lt"
    | NE -> "ne"
    | CC -> "cc"
    | CS -> "cs"
    | LS -> "ls"
    | HI -> "hi"
end

module Cond = struct
  type t =
    | EQ
    | NE
    | CS (* alias HS *)
    | CC (* alias LO *)
    | MI
    | PL
    | VS
    | VC
    | HI
    | LS
    | GE
    | LT
    | GT
    | LE
  (* | AL *)
  (* | NV *)

  let to_string t =
    match t with
    | EQ -> "eq"
    | NE -> "ne"
    | CS -> "cs"
    | CC -> "cc"
    | MI -> "mi"
    | PL -> "pl"
    | VS -> "vs"
    | VC -> "vc"
    | HI -> "hi"
    | LS -> "ls"
    | GE -> "ge"
    | LT -> "lt"
    | GT -> "gt"
    | LE -> "le"

  let invert t =
    match t with
    | EQ -> NE
    | NE -> EQ
    | CS -> CC
    | CC -> CS
    | MI -> PL
    | PL -> MI
    | VS -> VC
    | VC -> VS
    | HI -> LS
    | LS -> HI
    | GE -> LT
    | LT -> GE
    | GT -> LE
    | LE -> GT

  let of_float_cond (cond : Float_cond.t) : t =
    match cond with
    | EQ -> EQ
    | GT -> GT
    | GE -> GE
    | LT -> LT
    | LE -> LE
    | NE -> NE
    | CC -> CC
    | CS -> CS
    | HI -> HI
    | LS -> LS
end

module Rounding_mode = struct
  type t =
    | M
    | P
    | Z
    | X
    | N

  let to_string t =
    match t with M -> "m" | P -> "p" | Z -> "z" | X -> "x" | N -> "n"
end

module Memory_barrier = struct
  type t =
    | SY
    | LD
    | ST
    | ISH
    | ISHLD
    | ISHST
    | NSH
    | NSHLD
    | NSHST
    | OSH
    | OSHLD
    | OSHST

  let to_string b =
    match b with
    | SY -> "sy"
    | LD -> "ld"
    | ST -> "st"
    | ISH -> "ish"
    | ISHLD -> "ishld"
    | ISHST -> "ishst"
    | NSH -> "nsh"
    | NSHLD -> "nshld"
    | NSHST -> "nshst"
    | OSH -> "osh"
    | OSHLD -> "oshld"
    | OSHST -> "oshst"
end

module Symbol = struct
  type _ reloc_directive =
    | LOWER_TWELVE : [`Twelve] reloc_directive
    | GOT_PAGE : [`Twenty_one] reloc_directive
    | GOT_PAGE_OFF : [`Twelve] reloc_directive
    | GOT : [`Sixty_four] reloc_directive
    (* XXX is Sixty_four correct? *)
    | GOT_LOWER_TWELVE : [`Twelve] reloc_directive
    | PAGE : [`Twenty_one] reloc_directive
    | PAGE_OFF : [`Twelve] reloc_directive

  type 'width t =
    { name : string;
      offset : int;
      reloc : 'width reloc_directive option
    }

  let create (type w) ?(reloc : w reloc_directive option) ?(offset = 0) name :
      w t =
    { name; offset; reloc }

  let print_with_reloc_directive :
      type w. Format.formatter -> string * w reloc_directive option -> unit =
   fun ppf (s, reloc) ->
    let macosx = Target_system.is_macos () in
    match reloc with
    | None -> Format.pp_print_string ppf s
    | Some LOWER_TWELVE -> Format.fprintf ppf ":lo12:%s" s
    | Some GOT -> Format.fprintf ppf ":got:%s" s
    | Some GOT_LOWER_TWELVE -> Format.fprintf ppf ":got_lo12:%s" s
    | Some GOT_PAGE ->
      if macosx
      then Format.fprintf ppf "%s@GOTPAGE" s
      else Format.fprintf ppf ":got:%s" s
    | Some GOT_PAGE_OFF ->
      if macosx
      then Format.fprintf ppf "%s@GOTPAGEOFF" s
      else Format.fprintf ppf ":got_lo12:%s" s
    | Some PAGE ->
      if macosx
      then Format.fprintf ppf "%s@PAGE" s
      else Format.fprintf ppf "%s" s
    | Some PAGE_OFF ->
      if macosx
      then Format.fprintf ppf "%s@PAGEOFF" s
      else Format.fprintf ppf ":lo12:%s" s

  let print_int_offset ppf ofs =
    if ofs > 0
    then Format.fprintf ppf "+%d" ofs
    else if ofs < 0
    then Format.fprintf ppf "-%d" (-ofs)
    else ()

  let print : type w. Format.formatter -> w t -> unit =
   fun ppf { name; offset; reloc } ->
    Format.fprintf ppf "%a%a" print_with_reloc_directive (name, reloc)
      print_int_offset offset
end

module Operand = struct
  module Imm = struct
    (* int is big enough for all instruction encodings *)
    type 'width t =
      | Six : int -> [`Six] t
      | Twelve : int -> [`Twelve] t

    let print : type w. Format.formatter -> w t -> unit =
     fun ppf t ->
      match t with
      | Six n -> Format.fprintf ppf "#%d" n
      | Twelve n -> Format.fprintf ppf "#%d" n
  end

  module Bitmask = struct
    type t = nativeint

    let print ppf n = Format.fprintf ppf "#%nd" n
  end

  module Shift = struct
    module Kind = struct
      type 'op t =
        | LSL : [`Lsl] t
        | ASR : [`Asr] t
        | LSR : [`Lsr] t

      let to_string (type op) (t : op t) =
        match t with LSL -> "lsl" | ASR -> "asr" | LSR -> "lsr"
    end

    type ('op, 'amount) t =
      { kind : 'op Kind.t;
        amount : 'amount Imm.t
      }

    let print ppf t =
      let { kind; amount } = t in
      Format.fprintf ppf "%s %a" (Kind.to_string kind) Imm.print amount
  end

  let print_separator ppf () = Format.fprintf ppf ", "

  module Addressing_mode = struct
    module Offset = struct
      type _ t =
        | Imm : 'w Imm.t -> [`Imm of 'w] t
        | Symbol : 'w Symbol.t -> [`Symbol of 'w] t

      let print : type a. Format.formatter -> a t -> unit =
       fun ppf t ->
        match t with Imm i -> Imm.print ppf i | Symbol s -> Symbol.print ppf s
    end

    type t =
      | Reg : [< `GP of [< `X | `SP]] Reg.t -> t
      (* CR mshinwell: Offset -> Unsigned_offset? *)
      | Offset : [< `GP of [< `X | `SP]] Reg.t * _ Offset.t -> t
      | Pre : [< `GP of [< `X | `SP]] Reg.t * _ Offset.t -> t
      | Post : [< `GP of [< `X | `SP]] Reg.t * _ Offset.t -> t

    let print ppf (t : t) =
      let open Format in
      match t with
      | Reg r -> fprintf ppf "[%s]" (Reg.name r)
      | Offset (r, off) -> fprintf ppf "[%s, %a]" (Reg.name r) Offset.print off
      | Pre (r, off) -> fprintf ppf "[%s, %a]!" (Reg.name r) Offset.print off
      | Post (r, off) -> fprintf ppf "[%s], %a" (Reg.name r) Offset.print off
  end

  type _ t =
    | Sym : 'w Symbol.t -> [`Imm of 'w] t
    | Imm : 'w Imm.t -> [`Imm of 'w] t
    | Imm_float : float -> [`Imm of [`Sixty_four]] t
    | Imm_nativeint : nativeint -> [`Imm of [`Sixty_four]] t
    | Reg : 'a Reg.t -> [`Reg of 'a] t
    | Lsl_by_twelve : [`Fixed_shift of [`Lsl_by_twelve]] t
    | Shift : ('op, 'amount) Shift.t -> [`Shift of 'op * 'amount] t
    | Cond : Cond.t -> [`Cond] t
    | Float_cond : Float_cond.t -> [`Float_cond] t
    | Mem : Addressing_mode.t -> [`Mem] t
    | Bitmask : Bitmask.t -> [`Bitmask] t
  [@@ocaml.warning "-37"]

  type 'a operand = 'a t

  let print ppf (type a) (t : a t) =
    match t with
    | Sym s -> Symbol.print ppf s
    | Imm imm -> Imm.print ppf imm
    | Imm_float f -> Format.fprintf ppf "#%.7f" f
    | Imm_nativeint n -> Format.fprintf ppf "#%s" (Nativeint.to_string n)
    | Reg r -> Format.fprintf ppf "%s" (Reg.name r)
    | Lsl_by_twelve -> Format.fprintf ppf "lsr #12"
    | Shift s -> Shift.print ppf s
    | Cond c -> Format.fprintf ppf "%s" (Cond.to_string c)
    | Float_cond c -> Format.fprintf ppf "%s" (Float_cond.to_string c)
    | Mem m -> Format.fprintf ppf "%a" Addressing_mode.print m
    | Bitmask b -> Bitmask.print ppf b

  module Wrapped = struct
    type t = O : _ operand -> t

    let create (type a) (o : a operand) : t = O o

    let print ppf (O t) = print ppf t
  end
end

module Instruction_name = struct
  type _ t =
    (* | ABS_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | ADDP_vector : (([<
       `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as
       'r) Operand.t * 'r Operand.t * 'r Operand.t) t | ADDS : ([< `Reg of [<
       `GP of [< `X | `XZR]]] Operand.t * [< `Reg of [< `GP of [< `X | `SP]]]
       Operand.t * [< `Imm of [< `Twelve]] Operand.t * [< `Fixed_shift of [<
       `Lsl_by_twelve]] Operand.t option) t | ADDV : ([< `Reg of [< `Neon of [<
       `Scalar of [< `B]]]] Operand.t * [< `Reg of [< `Neon of [< `Vector of
       _]]] Operand.t) t | ADD_immediate : ([< `Reg of [< `GP of [< `X | `SP |
       `FP]]] Operand.t * [< `Reg of [< `GP of [< `X | `SP | `FP]]] Operand.t *
       [< `Imm of [< `Twelve]] Operand.t * [< `Fixed_shift of [<
       `Lsl_by_twelve]] Operand.t option) t | ADD_shifted_register : ([< `Reg of
       [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t *
       [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Shift of [< `Lsl | `Lsr |
       `Asr] * [`Six]] Operand.t option) t | ADD_vector : (([< `Reg of [< `Neon
       of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | ADR : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Imm of [< `Twenty_one]] Operand.t) t | ADRP : ([< `Reg of
       [< `GP of [< `X]]] Operand.t * _ Operand.t) t | AND_immediate : ([< `Reg
       of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t
       * [< `Bitmask] Operand.t) t | AND_shifted_register : ([< `Reg of [< `GP
       of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg
       of [< `GP of [< `X]]] Operand.t * [< `Shift of [< `Lsl | `Lsr | `Asr] *
       [`Six]] Operand.t option) t | AND_vector : (([< `Reg of [< `Neon of [<
       `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | ASRV : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP
       of [< `X]]] Operand.t) t | B : [< `Imm of _] Operand.t t | BL : [< `Imm
       of _] Operand.t t | BLR : [< `Reg of [< `GP of [< `X]]] Operand.t t | BR
       : [< `Reg of [< `GP of [< `X]]] Operand.t t | B_cond : Cond.t -> [< `Imm
       of _] Operand.t t | B_cond_float : Float_cond.t -> [< `Imm of _]
       Operand.t t | CBNZ : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Imm
       of _] Operand.t) t | CBZ : ([< `Reg of [< `GP of [< `X]]] Operand.t * [<
       `Imm of _] Operand.t) t | CLZ : ([< `Reg of [< `GP of [< `X]]] Operand.t
       * [< `Reg of [< `GP of [< `X]]] Operand.t) t | CM_register : Cond.t ->
       ([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] Operand.t) t | CM_zero : Cond.t -> ([<
       `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] Operand.t) t | CNT : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t) t | CNT_vector :
       (([< `Reg of [< `Neon of [< `Vector of _]]] as 'r) Operand.t * 'r
       Operand.t) t | CSEL : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg
       of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t
       * [< `Cond] Operand.t) t | CSINC : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t * [< `Reg of
       [< `GP of [< `X | `XZR]]] Operand.t * [< `Cond] Operand.t) t | CTZ : ([<
       `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]]
       Operand.t) t | CVT_vector : (([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | DMB
       : Memory_barrier.t -> unit t | DSB : Memory_barrier.t -> unit t | DUP :
       (Neon_reg_name.Lane_index.t * ([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t |
       EOR_immediate : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [<
       `GP of [< `X]]] Operand.t * [< `Bitmask] Operand.t) t |
       EOR_shifted_register : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg
       of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t
       * [< `Shift of [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option) t |
       EOR_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | EXT :
       ([< `Reg of [< `Neon of [< `Vector of [`V16B]]]] Operand.t * [< `Reg of
       [< `Neon of [< `Vector of [`V16B]]]] Operand.t * [< `Reg of [< `Neon of
       [< `Vector of [`V16B]]]] Operand.t * [< `Imm of [< `Six]] Operand.t) t |
       FABS : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r)
       Operand.t * 'r Operand.t) t | FADD : (([< `Reg of [< `Neon of [< `Scalar
       of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t |
       FADDP_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t |
       FADD_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | FCMP :
       (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t *
       'r Operand.t) t | FCM_register : Float_cond.t -> ([< `Reg of [< `Neon of
       [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t * [< `Reg of
       [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t *
       [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       Operand.t) t | FCM_zero : Float_cond.t -> ([< `Reg of [< `Neon of [<
       `Vector of [< any_vector] * [< any_width]]] ] Operand.t * [< `Reg of [<
       `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t) t |
       FCSEL : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r)
       Operand.t * 'r Operand.t * 'r Operand.t * [< `Cond] Operand.t) t | FCVT :
       ([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t * [< `Reg
       of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t) t | FCVTL_vector :
       ([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]]
       Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] Operand.t) t | FCVTNS : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
       Operand.t) t | FCVTNS_vector : (([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t |
       FCVTN_vector : ([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]]] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] Operand.t) t (* Binary vector operations
       with min/max *) | FCVTZS : ([< `Reg of [< `GP of [< `X]]] Operand.t * [<
       `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] Operand.t) t |
       FCVTZS_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] *
       [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | FDIV : (([< `Reg
       of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | FDIV_vector : (([< `Reg of [< `Neon of [<
       `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | FMADD : (([< `Reg of [< `Neon of [< `Scalar
       of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t * 'r Operand.t * 'r
       Operand.t) t | FMAX : (([< `Reg of [< `Neon of [< `Scalar of [< `S |
       `D]]]] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | FMAX_vector :
       (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | FMIN : (([< `Reg of
       [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t
       * 'r Operand.t) t | FMIN_vector : (([< `Reg of [< `Neon of [< `Vector of
       [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r
       Operand.t) t | FMOV_general_or_register : ([< `Reg of [< `Neon of [<
       `Scalar of [< `S | `D]] | `GP of [< `X | `W]] ] Operand.t * [< `Reg of [<
       `Neon of [< `Scalar of [< `S | `D]] | `GP of [< `X | `XZR | `W | `WZR] ]
       ] Operand.t) t | FMOV_scalar_immediate : ([< `Reg of [< `Neon of [<
       `Scalar of [< `S | `D]]]] Operand.t * [< `Imm of [< `Sixty_four]]
       Operand.t) t | FMOV_vector_immediate : ([< `Reg of [< `Neon of [< `Vector
       of _]]] Operand.t * [< `Imm of [< `Sixty_four]] Operand.t) t | FMSUB :
       (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t *
       'r Operand.t * 'r Operand.t * 'r Operand.t) t | FMUL : (([< `Reg of [<
       `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t *
       'r Operand.t) t | FMUL_vector : (([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r
       Operand.t) t | FNEG : (([< `Reg of [< `Neon of [< `Scalar of [< `S |
       `D]]]] as 'r) Operand.t * 'r Operand.t) t | FNEG_vector : (([< `Reg of [<
       `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r)
       Operand.t * 'r Operand.t) t | FNMADD : (([< `Reg of [< `Neon of [<
       `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t * 'r Operand.t
       * 'r Operand.t) t | FNMSUB : (([< `Reg of [< `Neon of [< `Scalar of [< `S
       | `D]]]] as 'r) Operand.t * 'r Operand.t * 'r Operand.t * 'r Operand.t) t
       | FNMUL : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r)
       Operand.t * 'r Operand.t * 'r Operand.t) t | FRECPE_vector : (([< `Reg of
       [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r)
       Operand.t * 'r Operand.t) t | FRINT : Rounding_mode.t -> (([< `Reg of [<
       `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r Operand.t) t
       | FRINT_vector : Rounding_mode.t -> (([< `Reg of [< `Neon of [< `Vector
       of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t
       | FRSQRTE_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector]
       * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | FSQRT : (([<
       `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t * 'r
       Operand.t) t | FSQRT_vector : (([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | FSUB
       : (([< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] as 'r) Operand.t
       * 'r Operand.t * 'r Operand.t) t | FSUB_vector : (([< `Reg of [< `Neon of
       [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | INS : (Neon_reg_name.Lane_index.t * [< `Reg
       of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t
       * ([< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]] as
       'rs) Operand.t) t | INS_V : (Neon_reg_name.Lane_index.t *
       Neon_reg_name.Lane_index.t * ([< `Reg of [< `Neon of [< `Vector of [<
       any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | LDAR
       : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Mem] Operand.t) t |
       LDP : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t * [< `Reg of [<
       `GP of [< `X | `W | `LR]]] Operand.t * [< `Mem] Operand.t) t | LDR : ([<
       `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t * [< `Mem] Operand.t) t |
       LDRB : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t |
       LDRH : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t |
       LDRSB : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem] Operand.t) t
       | LDRSH : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem] Operand.t)
       t | LDRSW : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Mem]
       Operand.t) t | LDR_simd_and_fp : ([< `Reg of [< `Neon of [< `Scalar of [<
       `D | `S | `Q]]]] Operand.t * [< `Mem] Operand.t) t | LSLV : ([< `Reg of
       [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t *
       [< `Reg of [< `GP of [< `X]]] Operand.t) t | LSRV : ([< `Reg of [< `GP of
       [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of
       [< `GP of [< `X]]] Operand.t) t | MADD : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP
       of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t) t
       | MOV : ([< `Reg of [< `GP of [< `X | `W] (* | `Neon of [< `Scalar of _ |
       `Vector of Neon_reg_name.Vector.t] *) ] ] Operand.t * [< `Reg of [< `GP
       of [< `X | `W | `XZR | `WZR] (* | `Neon of [< `Scalar of _ | `Vector of
       Neon_reg_name.Vector.t] *) ] | `Imm of _ ] Operand.t) t | MOVI : ([< `Reg
       of [< `Neon of _]] Operand.t * [< `Imm of [< `Twelve]] Operand.t) t |
       MOVK : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Imm of [<
       `Sixty_four]] Operand.t * [< `Shift of [< `Lsl] * [`Six]] Operand.t) t (*
       Typed vector SIMD instructions *) (* Binary vector operations - same
       format for all operands *) | MOVN : ([< `Reg of [< `GP of [< `X | `W]]]
       Operand.t * [< `Imm of [< `Twelve | `Sixty_four]] Operand.t * [< `Shift
       of [< `Lsl] * [`Six]] Operand.t option) t | MOVZ : ([< `Reg of [< `GP of
       [< `X | `W]]] Operand.t * [< `Imm of [< `Sixty_four]] Operand.t * [<
       `Shift of [< `Lsl] * [`Six]] Operand.t option) t | MOV_vector : (([< `Reg
       of [< `Neon of [< `Vector of _]]] as 'r) Operand.t * 'r Operand.t) t |
       MSUB : ([< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [<
       `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [<
       `GP of [< `X]]] Operand.t) t | MULL_vector : (([< `Reg of [< `Neon of [<
       `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t (* Unary vector operations *) | MUL_vector :
       (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | MVN_vector : (([<
       `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as
       'r) Operand.t * 'r Operand.t) t | NEG_vector : (([< `Reg of [< `Neon of
       [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t) t | NOP : unit t | ORR_immediate : ([< `Reg of [< `GP of [<
       `X]]] Operand.t * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t * [<
       `Bitmask] Operand.t) t | ORR_shifted_register : ([< `Reg of [< `GP of [<
       `X]]] Operand.t * [< `Reg of [< `GP of [< `X | `XZR]]] Operand.t * [<
       `Reg of [< `GP of [< `X]]] Operand.t * [< `Shift of [< `Lsl | `Lsr |
       `Asr] * [`Six]] Operand.t option) t | ORR_vector : (([< `Reg of [< `Neon
       of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
       Operand.t * 'r Operand.t) t | RBIT : ([< `Reg of [< `GP of [< `X]]]
       Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t) t | RET : unit t |
       REV : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Reg of [< `GP
       of [< `X | `W]]] Operand.t) t | REV16 : ([< `Reg of [< `GP of [< `X |
       `W]]] Operand.t * [< `Reg of [< `GP of [< `X | `W]]] Operand.t) t | SBFM
       : ([< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Reg of [< `GP of [<
       `X | `W]]] Operand.t * [< `Imm of [< `Six]] Operand.t * [< `Imm of [<
       `Six]] Operand.t) t | SCVTF : ([< `Reg of [< `Neon of [< `Scalar of [< `S
       | `D]]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t) t |
       SCVTF_vector : (([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
       any_width]]] ] as 'r) Operand.t * 'r Operand.t) t | SDIV : ([< `Reg of [<
       `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [<
       `Reg of V8B | [< `GP of [< `X]]] Operand.t) t | SHL : ([< `Reg of [<
       `Neon of [< `Vector of [< any_vector] * [< any_width]]]] Operand.t * [<
       `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
       Operand.t * [< `Imm of [< `Six]] Operand.t) t | SMAX_vector : (([< `Reg
       of [< `Neon of [< `Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
       * [< any_width] ] ] ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t)
       t *)
    | SMIN_vector
        : ([ `Reg of
             [ `Neon of
               [< `Vector of
                  [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
               ] ] ]
          * [ `Reg of
              [ `Neon of
                [< `Vector of
                   [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                ] ] ]
          * [ `Reg of
              [ `Neon of
                [< `Vector of
                   [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                ] ] ])
          t
  (* | SMOV : (Neon_reg_name.Lane_index.t * [< `Reg of [< `GP of [< `X]]]
     Operand.t * [< `Reg of [< `Neon of [< `Vector of [< `V8B | `V16B | `V4H |
     `V8H | `V2S | `V4S] * [< any_width] ] ] ] Operand.t) t | SMULH : ([< `Reg
     of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t *
     [< `Reg of [< `GP of [< `X]]] Operand.t) t | SMULL2_vector : ([< `Reg of [<
     `Neon of [< `Vector of [< any_vector] * [< any_width]]]] Operand.t * [<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
     Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
     any_width]]] ] Operand.t) t | SMULL_vector : ([< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]]] Operand.t * [< `Reg of [<
     `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t * [<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
     Operand.t) t | SQADD_vector : (([< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r
     Operand.t) t | SQSUB_vector : (([< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r
     Operand.t) t | SQXTN : ([< `Reg of [< `Neon of [< `Vector of [< any_vector]
     * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] Operand.t) t | SQXTN2 : ([< `Reg of [<
     `Neon of [< `Vector of [< any_vector] * [< any_width]]]] Operand.t * [<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
     Operand.t) t | SSHL_vector : (([< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r Operand.t * 'r
     Operand.t) t | SSHR : ([< `Reg of [< `Neon of [< `Vector of [< any_vector]
     * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] Operand.t * [< `Imm of [< `Six]] Operand.t)
     t | STP : ([< `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t * [< `Reg of
     [< `GP of [< `X | `W | `LR]]] Operand.t * [< `Mem] Operand.t) t | STR : ([<
     `Reg of [< `GP of [< `X | `W | `LR]]] Operand.t * [< `Mem] Operand.t) t |
     STRB : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t |
     STRH : ([< `Reg of [< `GP of [< `W]]] Operand.t * [< `Mem] Operand.t) t |
     STR_simd_and_fp : ([< `Reg of [< `Neon of [< `Scalar of [< `D | `S | `Q]]]]
     Operand.t * [< `Mem] Operand.t) t | SUBS_immediate : ([< `Reg of [< `GP of
     [< `W | `WZR | `X | `XZR]]] Operand.t * [< `Reg of [< `GP of [< `W | `X |
     `SP]]] Operand.t * [< `Imm of [< `Twelve]] Operand.t * [< `Fixed_shift of
     [< `Lsl_by_twelve]] Operand.t option) t | SUBS_shifted_register : ([< `Reg
     of [< `GP of [< `X | `XZR]]] Operand.t * [< `Reg of [< `GP of [< `X |
     `SP]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Shift of
     [< `Lsl | `Lsr | `Asr] * [`Six]] Operand.t option) t | SUB_immediate : ([<
     `Reg of [< `GP of [< `X | `SP]]] Operand.t * [< `Reg of [< `GP of [< `X |
     `SP]]] Operand.t * [< `Imm of [< `Twelve]] Operand.t * [< `Fixed_shift of
     [< `Lsl_by_twelve]] Operand.t option) t | SUB_shifted_register : ([< `Reg
     of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t *
     [< `Reg of [< `GP of [< `X]]] Operand.t * [< `Shift of [< `Lsl | `Lsr |
     `Asr] * [`Six]] Operand.t option) t | SUB_vector : (([< `Reg of [< `Neon of
     [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
     Operand.t * 'r Operand.t) t | SXTL : ([< `Reg of [< `Neon of [< `Vector of
     [< any_vector] * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] Operand.t) t | TBNZ : ([<
     `Reg of [< `GP of [< `X]]] Operand.t * [< `Imm of [< `Six]] Operand.t * [<
     `Imm of _] Operand.t) t | TBZ : ([< `Reg of [< `GP of [< `X]]] Operand.t *
     [< `Imm of [< `Six]] Operand.t * [< `Imm of _] Operand.t) t | TST : ([<
     `Reg of [< `GP of [< `X]]] Operand.t * [< `Bitmask] Operand.t) t |
     UADDLP_vector : ([< `Reg of [< `Neon of [< `Vector of [< `V4H | `V8H | `V2S
     | `V4S | `V1D | `V2D] * [< any_width] ] ] ] Operand.t * [< `Reg of [< `Neon
     of [< `Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [<
     any_width] ] ] ] Operand.t) t | UBFM : ([< `Reg of [< `GP of [< `X | `W]]]
     Operand.t * [< `Reg of [< `GP of [< `X | `W]]] Operand.t * [< `Imm of [<
     `Six]] Operand.t * [< `Imm of [< `Six]] Operand.t) t | UMAX_vector : (([<
     `Reg of [< `Neon of [< `Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S |
     `V4S]] ] ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t (* Other
     binary vector operations *) | UMIN_vector : (([< `Reg of [< `Neon of [<
     `Vector of [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width] ] ]
     ] as 'r) Operand.t * 'r Operand.t * 'r Operand.t) t | UMOV :
     (Neon_reg_name.Lane_index.t * ([< `Reg of [< `GP of [< `W | `X] | `Neon of
     [< `Scalar of [< `D]]]] as 'rd) Operand.t * [< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] Operand.t) t | UMULH : ([<
     `Reg of [< `GP of [< `X]]] Operand.t * [< `Reg of [< `GP of [< `X]]]
     Operand.t * [< `Reg of [< `GP of [< `X]]] Operand.t) t | UMULL2_vector :
     ([< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]]
     Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
     any_width]]] ] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] Operand.t) t | UMULL_vector : ([< `Reg of
     [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]] Operand.t * [<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
     Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
     any_width]]] ] Operand.t) t | UQADD_vector : (([< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
     Operand.t * 'r Operand.t) t | UQSUB_vector : (([< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
     Operand.t * 'r Operand.t) t (* Lane-indexed operations *) | UQXTN : ([<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]]
     Operand.t * [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [<
     any_width]]] ] Operand.t) t | UQXTN2 : ([< `Reg of [< `Neon of [< `Vector
     of [< any_vector] * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] Operand.t) t (* Binary vector
     operations with saturating arithmetic *) | USHL_vector : (([< `Reg of [<
     `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t
     * 'r Operand.t * 'r Operand.t) t | USHR : ([< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]]] Operand.t * [< `Reg of [<
     `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] Operand.t * [<
     `Imm of [< `Six]] Operand.t) t | UXTL : ([< `Reg of [< `Neon of [< `Vector
     of [< any_vector] * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [<
     `Vector of [< any_vector] * [< any_width]]] ] Operand.t) t | XTN : ([< `Reg
     of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]] Operand.t *
     [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
     Operand.t) t | XTN2 : ([< `Reg of [< `Neon of [< `Vector of [< any_vector]
     * [< any_width]]]] Operand.t * [< `Reg of [< `Neon of [< `Vector of [<
     any_vector] * [< any_width]]] ] Operand.t) t | YIELD : unit t | ZIP1 : (([<
     `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r)
     Operand.t * 'r Operand.t * 'r Operand.t) t | ZIP2 : (([< `Reg of [< `Neon
     of [< `Vector of [< any_vector] * [< any_width]]] ] as 'r) Operand.t * 'r
     Operand.t * 'r Operand.t) t *)

  type 'a instr = 'a t

  module Wrapped = struct
    type t = I : 'a instr -> t

    let to_string t =
      match t with
      | I instr -> (
        match instr with
        (* | ABS_vector -> "abs" | ADD_immediate | ADD_shifted_register |
           ADD_vector -> "add" | ADDP_vector -> "addp" | ADDS -> "adds" | ADDV
           -> "addv" | ADR -> "adr" | ADRP -> "adrp" | AND_immediate |
           AND_shifted_register | AND_vector -> "and" | ASRV -> "asrv" | B ->
           "b" | B_cond c -> "b." ^ Cond.to_string c | B_cond_float c -> "b." ^
           Float_cond.to_string c | BL -> "bl" | BLR -> "blr" | BR -> "br" |
           CBNZ -> "cbnz" | CBZ -> "cbz" | CLZ -> "clz" | CM_register cond ->
           "cm" ^ Cond.to_string cond | CM_zero cond -> "cm" ^ Cond.to_string
           cond | CNT -> "cnt" | CNT_vector -> "cnt" | CSEL -> "csel" | CSINC ->
           "csinc" | CTZ -> "ctz" | CVT_vector -> "cvt" | DMB b -> "dmb\t" ^
           Memory_barrier.to_string b | DSB b -> "dsb\t" ^
           Memory_barrier.to_string b | DUP -> "dup" | EOR_immediate |
           EOR_shifted_register | EOR_vector -> "eor" | EXT -> "ext" | FABS ->
           "fabs" | FADD -> "fadd" | FADD_vector -> "fadd" | FADDP_vector ->
           "faddp" | FCM_register cond -> "fcm" ^ Float_cond.to_string cond |
           FCM_zero cond -> "fcm" ^ Float_cond.to_string cond | FCMP -> "fcmp" |
           FCSEL -> "fcsel" | FCVT -> "fcvt" | FCVTL_vector -> "fcvtl" |
           FCVTN_vector -> "fcvtn" | FCVTNS -> "fcvtns" | FCVTNS_vector ->
           "fcvtns" | FCVTZS -> "fcvtzs" | FCVTZS_vector -> "fcvtzs" | FDIV ->
           "fdiv" | FDIV_vector -> "fdiv" | FMADD -> "fmadd" | FMAX -> "fmax" |
           FMAX_vector -> "fmax" | FMIN -> "fmin" | FMIN_vector -> "fmin" |
           FMOV_general_or_register | FMOV_scalar_immediate |
           FMOV_vector_immediate -> "fmov" | FMSUB -> "fmsub" | FMUL -> "fmul" |
           FMUL_vector -> "fmul" | FNEG -> "fneg" | FNEG_vector -> "fneg" |
           FNMADD -> "fnmadd" | FNMSUB -> "fnmsub" | FNMUL -> "fnmul" |
           FRECPE_vector -> "frecpe" | FRINT rm -> "frint" ^
           Rounding_mode.to_string rm | FRINT_vector rm -> "frint" ^
           Rounding_mode.to_string rm | FRSQRTE_vector -> "frsqrte" | FSQRT ->
           "fsqrt" | FSQRT_vector -> "fsqrt" | FSUB -> "fsub" | FSUB_vector ->
           "fsub" | INS -> "ins" | INS_V -> "ins" | LDAR -> "ldar" | LDP ->
           "ldp" | LDR -> "ldr" | LDR_simd_and_fp -> "ldr" | LDRB -> "ldrb" |
           LDRH -> "ldrh" | LDRSB -> "ldrsb" | LDRSH -> "ldrsh" | LDRSW ->
           "ldrsw" | LSLV -> "lslv" | LSRV -> "lsrv" | MADD -> "madd" | MOV ->
           "mov" | MOV_vector -> "mov" | MOVI -> "movi" | MOVK -> "movk" | MOVN
           -> "movn" | MOVZ -> "movz" | MSUB -> "msub" | MUL_vector -> "mul" |
           MULL_vector -> "mull" | MVN_vector -> "mvn" | NEG_vector -> "neg" |
           NOP -> "nop" | ORR_immediate | ORR_shifted_register | ORR_vector ->
           "orr" | RBIT -> "rbit" | RET -> "ret" | REV -> "rev" | REV16 ->
           "rev16" | SBFM -> "sbfm" | SCVTF -> "scvtf" | SCVTF_vector -> "scvtf"
           | SDIV -> "sdiv" | SHL -> "shl" | SMAX_vector -> "smax" *)
        | SMIN_vector ->
          "smin"
          (* | SMOV -> "smov" | SMULH -> "smulh" | SMULL2_vector -> "smull2" |
             SMULL_vector -> "smull" | SQADD_vector -> "sqadd" | SQSUB_vector ->
             "sqsub" | SQXTN -> "sqxtn" | SQXTN2 -> "sqxtn2" | SSHL_vector ->
             "sshl" (* XXX is this right? *) | SSHR -> "sshr" | STP -> "stp" |
             STR -> "str" | STR_simd_and_fp -> "str" | STRB -> "strb" | STRH ->
             "strh" | SUB_immediate | SUB_shifted_register | SUB_vector -> "sub"
             | SUBS_immediate | SUBS_shifted_register -> "subs" | SXTL -> "sxtl"
             | TBNZ -> "tbnz" | TBZ -> "tbz" | TST -> "tst" | UADDLP_vector ->
             "uaddlp" | UBFM -> "ubfm" | UMAX_vector -> "umax" | UMIN_vector ->
             "umin" | UMOV -> "umov" | UMULH -> "umulh" | UMULL2_vector ->
             "umull2" | UMULL_vector -> "umull" | UQADD_vector -> "uqadd" |
             UQSUB_vector -> "uqsub" | UQXTN -> "uqxtn" | UQXTN2 -> "uqxtn2" |
             USHL_vector -> "ushl" | USHR -> "ushr" | UXTL -> "uxtl" | XTN ->
             "xtn" | XTN2 -> "xtn2" | YIELD -> "yield" | ZIP1 -> "zip1" | ZIP2
             -> "zip2" *))
  end

  module Untyped = struct
    let operands_as_array (type operands) (instr : operands t)
        (ops : operands Operand.t) =
      let o = Operand.Wrapped.create in
      let _imm n = Operand.Imm (Operand.Imm.Twelve n) (* XXX duplicate *) in
      (* Helper to convert a vector register operand to a lane-indexed scalar *)
      let _vector_to_lane_operand (type a) (reg_op : a Operand.t) lane =
        match reg_op with
        | Reg reg -> (
          let index = reg.index in
          match reg.reg_name with
          | Neon (Vector vector) ->
            let scalar_type = Neon_reg_name.Scalar.of_vector vector in
            let lane_reg_name =
              Reg_name.Neon (Lane { r = S scalar_type; lane })
            in
            Operand.Wrapped.create
              (Operand.Reg (Reg.create lane_reg_name index))
          | GP _ | Neon (Scalar _) | Neon (Lane _) ->
            failwith "vector_to_lane_operand: not a vector register")
        | Sym _ | Imm _ | Imm_float _ | Imm_nativeint _ | Lsl_by_twelve
        | Shift _ | Cond _ | Float_cond _ | Mem _ | Bitmask _ ->
          failwith "vector_to_lane_operand: not a register operand"
      in
      match instr with
      (* | ABS_vector -> let rd, rs = ops in [| o rd; o rs |] | ADDP_vector ->
         let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] | ADDS -> ( let rd,
         rn, imm, shift_opt = ops in match shift_opt with | None -> [| o rd; o
         rn; o imm |] | Some shift -> [| o rd; o rn; o imm; o shift |]) | ADDV
         -> let rd, src = ops in [| o rd; o src |] | ADD_immediate -> ( let rd,
         rs, imm, shift_opt = ops in match shift_opt with | None -> [| o rd; o
         rs; o imm |] | Some shift -> [| o rd; o rs; o imm; o shift |]) |
         ADD_shifted_register -> ( let rd, rs, reg, shift_opt = ops in match
         shift_opt with | None -> [| o rd; o rs; o reg |] | Some shift -> [| o
         rd; o rs; o reg; o shift |]) | ADD_vector -> let rd, rs1, rs2 = ops in
         [| o rd; o rs1; o rs2 |] | ADR -> let rd, label = ops in [| o rd; o
         label |] | ADRP -> let rd, symbol = ops in [| o rd; o symbol |] |
         AND_immediate -> let rd, rs, bitmask = ops in [| o rd; o rs; o bitmask
         |] | AND_shifted_register -> ( let rd, rs, reg, shift_opt = ops in
         match shift_opt with | None -> [| o rd; o rs; o reg |] | Some shift ->
         [| o rd; o rs; o reg; o shift |]) | AND_vector -> let rd, rs1, rs2 =
         ops in [| o rd; o rs1; o rs2 |] | ASRV -> let rd, rn, rm = ops in [| o
         rd; o rn; o rm |] | B -> let target = ops in [| o target |] | BL -> let
         target = ops in [| o target |] | BLR -> let rn = ops in [| o rn |] | BR
         -> let rn = ops in [| o rn |] | B_cond _ -> let target = ops in [| o
         target |] | B_cond_float _ -> let target = ops in [| o target |] | CBNZ
         -> let reg, target = ops in [| o reg; o target |] | CBZ -> let reg,
         target = ops in [| o reg; o target |] | CLZ -> let rd, rn = ops in [| o
         rd; o rn |] | CM_register _cond -> let rd, rn, rm = ops in [| o rd; o
         rn; o rm |] | CM_zero _cond -> let rd, rn = ops in [| o rd; o rn; o
         (imm 0) |] | CNT -> let rd, rn = ops in [| o rd; o rn |] | CNT_vector
         -> let rd, src = ops in [| o rd; o src |] | CSEL -> let rd, rn, rm,
         cond = ops in [| o rd; o rn; o rm; o cond |] | CSINC -> let rd, rn, rm,
         cond = ops in [| o rd; o rn; o rm; o cond |] | CTZ -> let rd, rn = ops
         in [| o rd; o rn |] | CVT_vector -> let rd, rs = ops in [| o rd; o rs
         |] | DMB _ -> [||] | DSB _ -> [||] | DUP -> let lane, rd, rs = ops in
         [| o rd; vector_to_lane_operand rs lane |] | EOR_immediate -> let rd,
         rs, bitmask = ops in [| o rd; o rs; o bitmask |] | EOR_shifted_register
         -> ( let rd, rs, reg, shift_opt = ops in match shift_opt with | None ->
         [| o rd; o rs; o reg |] | Some shift -> [| o rd; o rs; o reg; o shift
         |]) | EOR_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |]
         | EXT -> let rd, rs1, rs2, idx = ops in [| o rd; o rs1; o rs2; o idx |]
         | FABS -> let rd, rn = ops in [| o rd; o rn |] | FADD -> let rd, rn, rm
         = ops in [| o rd; o rn; o rm |] | FADDP_vector -> let rd, rs1, rs2 =
         ops in [| o rd; o rs1; o rs2 |] | FADD_vector -> let rd, rs1, rs2 = ops
         in [| o rd; o rs1; o rs2 |] | FCMP -> let rn, rm = ops in [| o rn; o rm
         |] | FCM_register _cond -> let rd, rn, rm = ops in [| o rd; o rn; o rm
         |] | FCM_zero _cond -> let rd, rn = ops in [| o rd; o rn; o
         (Operand.Imm_float 0.) |] | FCSEL -> let rd, rn, rm, cond = ops in [| o
         rd; o rn; o rm; o cond |] | FCVT -> let rd, rn = ops in [| o rd; o rn
         |] | FCVTL_vector -> let rd, rs = ops in [| o rd; o rs |] | FCVTNS ->
         let rd, rn = ops in [| o rd; o rn |] | FCVTNS_vector -> let rd, rs =
         ops in [| o rd; o rs |] | FCVTN_vector -> let rd, rs = ops in [| o rd;
         o rs |] | FCVTZS -> let rd, rn = ops in [| o rd; o rn |] |
         FCVTZS_vector -> let rd, rs = ops in [| o rd; o rs |] | FDIV -> let rd,
         rn, rm = ops in [| o rd; o rn; o rm |] | FDIV_vector -> let rd, rs1,
         rs2 = ops in [| o rd; o rs1; o rs2 |] | FMADD -> let rd, rn, rm, ra =
         ops in [| o rd; o rn; o rm; o ra |] | FMAX -> let rd, rn, rm = ops in
         [| o rd; o rn; o rm |] | FMAX_vector -> let rd, rs1, rs2 = ops in [| o
         rd; o rs1; o rs2 |] | FMIN -> let rd, rn, rm = ops in [| o rd; o rn; o
         rm |] | FMIN_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2
         |] | FMOV_general_or_register -> let rd, rs = ops in [| o rd; o rs |] |
         FMOV_scalar_immediate -> let rd, imm = ops in [| o rd; o imm |] |
         FMOV_vector_immediate -> let rd, imm = ops in [| o rd; o imm |] | FMSUB
         -> let rd, rn, rm, ra = ops in [| o rd; o rn; o rm; o ra |] | FMUL ->
         let rd, rn, rm = ops in [| o rd; o rn; o rm |] | FMUL_vector -> let rd,
         rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] | FNEG -> let rd, rn = ops
         in [| o rd; o rn |] | FNEG_vector -> let rd, rs = ops in [| o rd; o rs
         |] | FNMADD -> let rd, rn, rm, ra = ops in [| o rd; o rn; o rm; o ra |]
         | FNMSUB -> let rd, rn, rm, ra = ops in [| o rd; o rn; o rm; o ra |] |
         FNMUL -> let rd, rn, rm = ops in [| o rd; o rn; o rm |] | FRECPE_vector
         -> let rd, rs = ops in [| o rd; o rs |] | FRINT _ -> let rd, rn = ops
         in [| o rd; o rn |] | FRINT_vector _ -> let rd, rn = ops in [| o rd; o
         rn |] | FRSQRTE_vector -> let rd, rs = ops in [| o rd; o rs |] | FSQRT
         -> let rd, rn = ops in [| o rd; o rn |] | FSQRT_vector -> let rd, rs =
         ops in [| o rd; o rs |] | FSUB -> let rd, rn, rm = ops in [| o rd; o
         rn; o rm |] | FSUB_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1;
         o rs2 |] | INS -> let lane, rd, rs = ops in [| vector_to_lane_operand
         rd lane; o rs |] | INS_V -> let dst_lane, src_lane, rd, rs = ops in [|
         vector_to_lane_operand rd dst_lane; vector_to_lane_operand rs src_lane
         |] | LDAR -> let rt, addr = ops in [| o rt; o addr |] | LDP -> let rt1,
         rt2, addr = ops in [| o rt1; o rt2; o addr |] | LDR -> let rt, addr =
         ops in [| o rt; o addr |] | LDRB -> let rt, addr = ops in [| o rt; o
         addr |] | LDRH -> let rt, addr = ops in [| o rt; o addr |] | LDRSB ->
         let rt, addr = ops in [| o rt; o addr |] | LDRSH -> let rt, addr = ops
         in [| o rt; o addr |] | LDRSW -> let rt, addr = ops in [| o rt; o addr
         |] | LDR_simd_and_fp -> let rt, addr = ops in [| o rt; o addr |] | LSLV
         -> let rd, rn, rm = ops in [| o rd; o rn; o rm |] | LSRV -> let rd, rn,
         rm = ops in [| o rd; o rn; o rm |] | MADD -> let rd, rn, rm, ra = ops
         in [| o rd; o rn; o rm; o ra |] | MOV -> let rd, src = ops in [| o rd;
         o src |] | MOVI -> let rd, imm = ops in [| o rd; o imm |] | MOVK -> let
         rd, imm, shift = ops in [| o rd; o imm; o shift |] | MOVN -> ( let rd,
         imm, shift_opt = ops in match shift_opt with | None -> [| o rd; o imm
         |] | Some shift -> [| o rd; o imm; o shift |]) | MOVZ -> ( let rd, imm,
         shift_opt = ops in match shift_opt with | None -> [| o rd; o imm |] |
         Some shift -> [| o rd; o imm; o shift |]) | MOV_vector -> let rd, src =
         ops in [| o rd; o src |] | MSUB -> let rd, rn, rm, ra = ops in [| o rd;
         o rn; o rm; o ra |] | MULL_vector -> let rd, rs1, rs2 = ops in [| o rd;
         o rs1; o rs2 |] | MUL_vector -> let rd, rs1, rs2 = ops in [| o rd; o
         rs1; o rs2 |] | MVN_vector -> let rd, rs = ops in [| o rd; o rs |] |
         NEG_vector -> let rd, rs = ops in [| o rd; o rs |] | NOP -> [||] |
         ORR_immediate -> let rd, rs, bitmask = ops in [| o rd; o rs; o bitmask
         |] | ORR_shifted_register -> ( let rd, rs, reg, shift_opt = ops in
         match shift_opt with | None -> [| o rd; o rs; o reg |] | Some shift ->
         [| o rd; o rs; o reg; o shift |]) | ORR_vector -> let rd, rs1, rs2 =
         ops in [| o rd; o rs1; o rs2 |] | RBIT -> let rd, rn = ops in [| o rd;
         o rn |] | RET -> [||] | REV -> let rd, rn = ops in [| o rd; o rn |] |
         REV16 -> let rd, rn = ops in [| o rd; o rn |] | SBFM -> let rd, rn,
         immr, imms = ops in [| o rd; o rn; o immr; o imms |] | SCVTF -> let rd,
         rn = ops in [| o rd; o rn |] | SCVTF_vector -> let rd, rs = ops in [| o
         rd; o rs |] | SDIV -> let rd, rn, rm = ops in [| o rd; o rn; o rm |] |
         SHL -> let rd, rs, imm = ops in [| o rd; o rs; o imm |] | SMAX_vector
         -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] *)
      | SMIN_vector ->
        let rd (*, rs1, rs2 *) = ops in
        [| o rd (* o rs1; o rs2 *) |]
    (* | SMOV -> let lane, rd, rs = ops in [| o rd; vector_to_lane_operand rs
       lane |] | SMULH -> let rd, rn, rm = ops in [| o rd; o rn; o rm |] |
       SMULL2_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] |
       SMULL_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] |
       SQADD_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] |
       SQSUB_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] |
       SQXTN -> let rd, rs = ops in [| o rd; o rs |] | SQXTN2 -> let rd, rs =
       ops in [| o rd; o rs |] | SSHL_vector -> let rd, rs1, rs2 = ops in [| o
       rd; o rs1; o rs2 |] | SSHR -> let rd, rs, imm = ops in [| o rd; o rs; o
       imm |] | STP -> let rt1, rt2, addr = ops in [| o rt1; o rt2; o addr |] |
       STR -> let rt, addr = ops in [| o rt; o addr |] | STRB -> let rt, addr =
       ops in [| o rt; o addr |] | STRH -> let rt, addr = ops in [| o rt; o addr
       |] | STR_simd_and_fp -> let rt, addr = ops in [| o rt; o addr |] |
       SUBS_immediate -> ( let rd, rn, imm, shift_opt = ops in match shift_opt
       with | None -> [| o rd; o rn; o imm |] | Some shift -> [| o rd; o rn; o
       imm; o shift |]) | SUBS_shifted_register -> ( let rd, rn, reg, shift_opt
       = ops in match shift_opt with | None -> [| o rd; o rn; o reg |] | Some
       shift -> [| o rd; o rn; o reg; o shift |]) | SUB_immediate -> ( let rd,
       rs, imm, shift_opt = ops in match shift_opt with | None -> [| o rd; o rs;
       o imm |] | Some shift -> [| o rd; o rs; o imm; o shift |]) |
       SUB_shifted_register -> ( let rd, rs, reg, shift_opt = ops in match
       shift_opt with | None -> [| o rd; o rs; o reg |] | Some shift -> [| o rd;
       o rs; o reg; o shift |]) | SUB_vector -> let rd, rs1, rs2 = ops in [| o
       rd; o rs1; o rs2 |] | SXTL -> let rd, rs = ops in [| o rd; o rs |] | TBNZ
       -> let reg, bit, target = ops in [| o reg; o bit; o target |] | TBZ ->
       let reg, bit, target = ops in [| o reg; o bit; o target |] | TST -> let
       rn, op2 = ops in [| o rn; o op2 |] | UADDLP_vector -> let rd, rs = ops in
       [| o rd; o rs |] | UBFM -> let rd, rn, immr, imms = ops in [| o rd; o rn;
       o immr; o imms |] | UMAX_vector -> let rd, rs1, rs2 = ops in [| o rd; o
       rs1; o rs2 |] | UMIN_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1;
       o rs2 |] | UMOV -> let lane, rd, rs = ops in [| o rd;
       vector_to_lane_operand rs lane |] | UMULH -> let rd, rn, rm = ops in [| o
       rd; o rn; o rm |] | UMULL2_vector -> let rd, rs1, rs2 = ops in [| o rd; o
       rs1; o rs2 |] | UMULL_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1;
       o rs2 |] | UQADD_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o
       rs2 |] | UQSUB_vector -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2
       |] | UQXTN -> let rd, rs = ops in [| o rd; o rs |] | UQXTN2 -> let rd, rs
       = ops in [| o rd; o rs |] | USHL_vector -> let rd, rs1, rs2 = ops in [| o
       rd; o rs1; o rs2 |] | USHR -> let rd, rs, imm = ops in [| o rd; o rs; o
       imm |] | UXTL -> let rd, rs = ops in [| o rd; o rs |] | XTN -> let rd, rs
       = ops in [| o rd; o rs |] | XTN2 -> let rd, rs = ops in [| o rd; o rs |]
       | YIELD -> [||] | ZIP1 -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2
       |] | ZIP2 -> let rd, rs1, rs2 = ops in [| o rd; o rs1; o rs2 |] *)
  end
end
(* module Instruction = struct type t = { name : Instruction_name.Wrapped.t;
   operands : Operand.Wrapped.t array }

   let create (type a) (name : a Instruction_name.t) ~(operands : a) = { name =
   Instruction_name.Wrapped.I name; operands =
   Instruction_name.Untyped.operands_as_array name operands }

   let print ppf t = let { name; operands } = t in let pp_sep =
   Operand.print_separator in if Array.length operands = 0 then Format.fprintf
   ppf "%s" (Instruction_name.Wrapped.to_string name) else Format.fprintf ppf
   "%s\t%a" (Instruction_name.Wrapped.to_string name) (Format.pp_print_seq
   ~pp_sep Operand.Wrapped.print) (Array.to_seq operands) end *)

module DSL = struct
  let symbol (type w) (s : w Symbol.t) = Operand.Sym s

  let mem ~(base : [< `GP of [< `X | `SP]] Reg.t) = Operand.Mem (Reg base)

  let mem_offset ~(base : [< `GP of [< `X | `SP]] Reg.t) ~offset =
    Operand.Mem (Offset (base, Imm (Operand.Imm.Twelve offset)))

  let mem_symbol ~(base : [< `GP of [< `X | `SP]] Reg.t) ~symbol =
    Operand.Mem (Offset (base, Symbol symbol))

  let mem_pre ~(base : [< `GP of [< `X | `SP]] Reg.t) ~offset =
    Operand.Mem (Pre (base, Imm (Operand.Imm.Twelve offset)))

  let mem_post ~(base : [< `GP of [< `X | `SP]] Reg.t) ~offset =
    Operand.Mem (Post (base, Imm (Operand.Imm.Twelve offset)))

  let shift ~kind ~amount =
    Operand.Shift { kind; amount = Operand.Imm.Six amount }

  let reglane index ~lane r =
    let reg_name = Reg_name.(Neon Neon_reg_name.(Lane { r; lane })) in
    Operand.Reg (Reg.create reg_name index)

  let reglane_v4s index ~lane =
    let r = Neon_reg_name.(Lane.V Vector.V4S) in
    reglane index ~lane r

  let reglane_v2d index ~lane =
    let r = Neon_reg_name.(Lane.V Vector.V2D) in
    reglane index ~lane r

  (* [reglane_*]: Clang 17 assembler does not accept optional number of lanes
     notation of the form Vn.4S[lane], even though it is required to do so in
     ARMARM. Emit Vn.S[lane]. *)
  let reglane_b index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.B) in
    reglane index ~lane r

  let reglane_h index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.H) in
    reglane index ~lane r

  let reglane_s index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.S) in
    reglane index ~lane r

  let reglane_d index ~lane =
    let r = Neon_reg_name.(Lane.S Scalar.D) in
    reglane index ~lane r

  let reg_v2s index : _ Operand.t = Reg (Reg.reg_v2s index)

  let reg_v4s index : _ Operand.t = Reg (Reg.reg_v4s index)

  let reg_v2d index : _ Operand.t = Reg (Reg.reg_v2d index)

  let reg_v8b index : _ Operand.t = Reg (Reg.reg_v8b index)

  let reg_v16b index : _ Operand.t = Reg (Reg.reg_v16b index)

  let reg_v8h index : _ Operand.t = Reg (Reg.reg_v8h index)

  let reg_v4h index : _ Operand.t = Reg (Reg.reg_v4h index)

  let reg_b index : _ Operand.t = Reg (Reg.reg_b index)

  let reg_s index : _ Operand.t = Reg (Reg.reg_s index)

  let reg_d index : _ Operand.t = Reg (Reg.reg_d index)

  let reg_q index : _ Operand.t = Reg (Reg.reg_q index)

  let reg_w index : _ Operand.t = Reg (Reg.reg_w index)

  let reg_x index : _ Operand.t = Reg (Reg.reg_x index)

  let sp () : _ Operand.t = Reg (Reg.sp ())

  let lr () : _ Operand.t = Reg (Reg.lr ())

  let fp () : _ Operand.t = Reg (Reg.fp ())

  let xzr () : _ Operand.t = Reg (Reg.xzr ())

  let wzr () : _ Operand.t = Reg (Reg.wzr ())

  let reg_op reg = Operand.Reg reg

  let imm n = Operand.Imm (Operand.Imm.Twelve n)

  let imm_six n = Operand.Imm (Operand.Imm.Six n)

  let imm_float f = Operand.Imm_float f

  let imm_nativeint n = Operand.Imm_nativeint n

  let bitmask n =
    if not (Arm64_logical_immediates.is_logical_immediate n)
    then
      Misc.fatal_errorf
        "Cannot encode logical immediate %nd as a bitmask immediate" n;
    Operand.Bitmask n

  let cond c = Operand.Cond c

  (* CR sspies: probably these should be part of the instruction name instead *)
  let float_cond c = Operand.Float_cond c
  (* let print_ins name operands = Format.asprintf "%a" Instruction.print
     (Instruction.create name ~operands)

     (* CR mshinwell: Acc should not be in this file *) module Acc = struct let
     emit_string = ref None

     let set_emit_string ~emit_string:emit = emit_string := Some emit

     let ins (type a) (name : a Instruction_name.t) (operands : a) = let instr =
     Instruction.create name ~operands in let str = Format.asprintf "\t%a\n"
     Instruction.print instr in match !emit_string with None -> () | Some
     emit_string -> emit_string str *)

  (* Instructions that are expanded into others *)
  (* let ins_mul rd rn rm = ins MADD (rd, rn, rm, reg_op (Reg.xzr ()))

     (* LSL <Xd>, <Xn>, #<shift> -> UBFM <Xd>, <Xn>, #(-<shift> MOD 64),
     #(63-<shift>) *) let ins_lsl_immediate rd rn ~shift_in_bits = (* CR
     mshinwell: range checks on shift? *) let n = -shift_in_bits in let n' = if
     n < 0 then n + 64 else n in let immr = Operand.Imm (Six n') in let imms =
     Operand.Imm (Six (63 - shift_in_bits)) in ins UBFM (rd, rn, immr, imms)

     (* LSR <Xd>, <Xn>, #<shift> -> UBFM <Xd>, <Xn>, #<shift>, #63 *) let
     ins_lsr_immediate rd rn ~shift_in_bits = (* CR mshinwell: range checks on
     shift? *) let immr = Operand.Imm (Six shift_in_bits) in let imms =
     Operand.Imm (Six 63) in ins UBFM (rd, rn, immr, imms)

     (* ASR <Xd>, <Xn>, #<shift> -> SBFM <Xd>, <Xn>, #<shift>, #63 *) let
     ins_asr_immediate rd rn ~shift_in_bits = (* CR mshinwell: range checks on
     shift? *) let immr = Operand.Imm (Six shift_in_bits) in let imms =
     Operand.Imm (Six 63) in ins SBFM (rd, rn, immr, imms)

     (* UXTB <Wd>, <Wn> -> UBFM <Wd>, <Wn>, #0, #7 *) let ins_uxtb wd wn = let
     immr = Operand.Imm (Six 0) in let imms = Operand.Imm (Six 7) in ins UBFM
     (wd, wn, immr, imms)

     (* UXTH <Wd>, <Wn> -> UBFM <Wd>, <Wn>, #0, #15 *) let ins_uxth wd wn = let
     immr = Operand.Imm (Six 0) in let imms = Operand.Imm (Six 15) in ins UBFM
     (wd, wn, immr, imms)

     [@@@ocaml.warning "-18"] (* CR mshinwell: deal with non-principal warnings
     *)

     (* CMP <Xn|SP>, #<imm>{, <shift>} -> SUBS XZR, <Xn|SP>, #<imm>{, <shift>}
     *) let ins_cmp rn imm shift_opt = ins SUBS_immediate (reg_op (Reg.xzr ()),
     rn, imm, shift_opt)

     (* CMP <Xn>, <Xm>{, <shift>} -> SUBS XZR, <Xn>, <Xm>{, <shift>} *) let
     ins_cmp_reg rn rm shift_opt = ins SUBS_shifted_register (reg_op (Reg.xzr
     ()), rn, rm, shift_opt)

     (* CMN <Xn|SP>, #<imm>{, <shift>} -> ADDS XZR, <Xn|SP>, #<imm>{, <shift>}
     *) let ins_cmn rn imm shift_opt = ins ADDS (reg_op (Reg.xzr ()), rn, imm,
     shift_opt)

     (* CSET <Xd>, <invcond> -> CSINC <Xd>, XZR, XZR, <cond> *) let ins_cset rd
     invcond = ins CSINC ( rd, reg_op (Reg.xzr ()), reg_op (Reg.xzr ()),
     Operand.Cond (Cond.invert invcond) )

     (* MOV from SP -> ADD <Xd>, SP, #0 *) let ins_mov_from_sp ~dst:rd = ins
     ADD_immediate (rd, reg_op (Reg.sp ()), imm 0, None)

     (* MOV to SP -> ADD SP, <Xn>, #0 *) let ins_mov_to_sp ~src:rn = ins
     ADD_immediate (reg_op (Reg.sp ()), rn, imm 0, None) *)
end

module Binary_encoder = struct
  let encode_shift_type (type op) (kind : op Operand.Shift.Kind.t) =
    match kind with LSL -> 0b00 | LSR -> 0b01 | ASR -> 0b10

  let encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12 ~rn ~rd =
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

  let encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm ~imm6 ~rn ~rd =
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
    let result = logor result (of_int rd) in
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
    let result = logor result (shift_left (of_int rn) 5) in
    let result = logor result (of_int rd) in
    result

  (* Data-processing (2 source) - C4.1.94.1 *)
  let encode_data_proc_2_source ~sf ~s ~opcode ~rm ~rn ~rd =
    let open Int32 in
    let result = zero in
    let result = logor result (shift_left (of_int sf) 31) in
    let result = logor result (shift_left (of_int s) 29) in
    let result = logor result (shift_left (of_int 0b11010110) 21) in
    let result = logor result (shift_left (of_int rm) 16) in
    let result = logor result (shift_left (of_int opcode) 10) in
    let result = logor result (shift_left (of_int rn) 5) in
    let result = logor result (of_int rd) in
    result

  (* Data-processing (3 source) - C4.1.94.13 *)
  let encode_data_proc_3_source ~sf ~op54 ~op31 ~o0 ~rm ~ra ~rn ~rd =
    let open Int32 in
    let result = zero in
    let result = logor result (shift_left (of_int sf) 31) in
    let result = logor result (shift_left (of_int op54) 29) in
    let result = logor result (shift_left (of_int 0b11011) 24) in
    let result = logor result (shift_left (of_int op31) 21) in
    let result = logor result (shift_left (of_int rm) 16) in
    let result = logor result (shift_left (of_int o0) 15) in
    let result = logor result (shift_left (of_int ra) 10) in
    let result = logor result (shift_left (of_int rn) 5) in
    let result = logor result (of_int rd) in
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
    let result = logor result (shift_left (of_int rn) 5) in
    let result = logor result (of_int rd) in
    result

  (* Advanced SIMD three same - C4.1.95.24 *)
  let encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd =
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
  let encode_fp_immediate ~ftype ~imm8 ~rd =
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
  let encode_fp_2_source ~ftype ~rm ~opcode ~rn ~rd =
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
  let encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd =
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
  let encode_fp_3_source ~ftype ~o1 ~rm ~o0 ~ra ~rn ~rd =
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

  let encode_condition (cond : Cond.t) : int =
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
  let encode_logical_shifted_register ~sf ~opc ~shift ~n ~rm ~imm6 ~rn ~rd =
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
    let result = logor result (of_int rd) in
    result

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
    let result = logor result (shift_left (of_int rn) 5) in
    let result = logor result (of_int rd) in
    result

  type _ many =
    | Singleton : 'a Operand.t -> 'a many
    | Pair : 'a Operand.t * 'b Operand.t -> ('a * 'b) many
    | Triple : 'a Operand.t * 'b Operand.t * 'c Operand.t -> ('a * 'b * 'c) many

  let encode_instruction :
      type operands. operands many -> operands Instruction_name.t -> int32 =
   fun operands instr ->
    match operands, instr with
    (* | ( Pair ( (*Reg { reg_name = Neon (Vector (V8B|V16B | V4H | V8H | V2S |
       V4S)); _ }, *) Reg { reg_name = Neon (Vector (V8B|V16B | V4H | V8H | V2S
       | V4S)); _ }, Reg { reg_name = Neon (Vector (V8B|V16B | V4H | V8H | V2S |
       V4S)); _ } ), SMIN_vector ) -> failwith "foo" *)
    | ( Triple
          ( Reg { reg_name = Neon (Vector (V16B | V4H | V8H | V2S | V4S)); _ },
            Reg { reg_name = Neon (Vector (V16B | V4H | V8H | V2S | V4S)); _ },
            Reg { reg_name = Neon (Vector (V16B | V4H | V8H | V2S | V4S)); _ }
          ),
        SMIN_vector ) ->
      failwith "foo"
  (* | Reg { reg_name = Neon (Vector (V2D|V1D)); _ } -> . *)

  let encode_instruction :
      type operands. operands Instruction_name.t -> operands Operand.t -> int32
      =
   fun instr operands ->
    match instr, operands with
    (* PC-relative addressing - C4.1.92.2 *)
    (* | ADR, (Reg rd, Sym _) -> let rd_bits = Reg.encoding rd in let immlo = 0
       in let immhi = 0 in encode_adr ~op:0 ~immlo ~immhi ~rd:rd_bits | ADR,
       (Reg rd, Imm _) -> let rd_bits = Reg.encoding rd in let immlo = 0 in let
       immhi = 0 in encode_adr ~op:0 ~immlo ~immhi ~rd:rd_bits | ADRP, (Reg rd,
       _) -> let rd_bits = Reg.encoding rd in let immlo = 0 in let immhi = 0 in
       encode_adr ~op:1 ~immlo ~immhi ~rd:rd_bits (* Logical (immediate) -
       C4.1.92.6 *) | AND_immediate, (Reg rd, Reg rn, Bitmask _) -> let rd_bits
       = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_logical_immediate ~sf:1 ~opc:0b00 ~n:0 ~immr:0 ~imms:0 ~rn:rn_bits
       ~rd:rd_bits | ORR_immediate, (Reg rd, Reg rn, Bitmask _) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_logical_immediate ~sf:1 ~opc:0b01 ~n:0 ~immr:0 ~imms:0 ~rn:rn_bits
       ~rd:rd_bits | EOR_immediate, (Reg rd, Reg rn, Bitmask _) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_logical_immediate ~sf:1 ~opc:0b10 ~n:0 ~immr:0 ~imms:0 ~rn:rn_bits
       ~rd:rd_bits | ADD_immediate, (Reg rd, Reg rn, Imm (Twelve imm12),
       shift_opt) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
       rn in let sh_bit = match shift_opt with None -> 0 | Some Lsl_by_twelve ->
       1 in encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh:sh_bit ~imm12
       ~rn:rn_bits ~rd:rd_bits | ADD_immediate, (Reg rd, Reg rn, Imm (Six imm6),
       shift_opt) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
       rn in let sh_bit = match shift_opt with None -> 0 | Some Lsl_by_twelve ->
       1 in encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh:sh_bit ~imm12:imm6
       ~rn:rn_bits ~rd:rd_bits | ADD_immediate, (Reg rd, Reg rn, Sym _,
       shift_opt) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
       rn in let sh_bit = match shift_opt with None -> 0 | Some Lsl_by_twelve ->
       1 in encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh:sh_bit ~imm12:0
       ~rn:rn_bits ~rd:rd_bits | SUB_immediate, (Reg rd, Reg rn, Imm (Twelve
       imm12), shift_opt) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let sh_bit = match shift_opt with None -> 0 | Some
       Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:0
       ~sh:sh_bit ~imm12 ~rn:rn_bits ~rd:rd_bits | SUB_immediate, (Reg rd, Reg
       rn, Imm (Six imm6), shift_opt) -> let rd_bits = Reg.encoding rd in let
       rn_bits = Reg.encoding rn in let sh_bit = match shift_opt with None -> 0
       | Some Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:0
       ~sh:sh_bit ~imm12:imm6 ~rn:rn_bits ~rd:rd_bits | SUB_immediate, (Reg rd,
       Reg rn, Sym _, shift_opt) -> let rd_bits = Reg.encoding rd in let rn_bits
       = Reg.encoding rn in let sh_bit = match shift_opt with None -> 0 | Some
       Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:0
       ~sh:sh_bit ~imm12:0 ~rn:rn_bits ~rd:rd_bits | SUBS_immediate, (Reg rd,
       Reg rn, Imm (Twelve imm12), shift_opt) -> let rd_bits = Reg.encoding rd
       in let rn_bits = Reg.encoding rn in let sh_bit = match shift_opt with
       None -> 0 | Some Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1
       ~op:1 ~s:1 ~sh:sh_bit ~imm12 ~rn:rn_bits ~rd:rd_bits | SUBS_immediate,
       (Reg rd, Reg rn, Imm (Six imm6), shift_opt) -> let rd_bits = Reg.encoding
       rd in let rn_bits = Reg.encoding rn in let sh_bit = match shift_opt with
       None -> 0 | Some Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1
       ~op:1 ~s:1 ~sh:sh_bit ~imm12:imm6 ~rn:rn_bits ~rd:rd_bits |
       SUBS_immediate, (Reg rd, Reg rn, Sym _, shift_opt) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let sh_bit = match
       shift_opt with None -> 0 | Some Lsl_by_twelve -> 1 in
       encode_add_sub_immediate ~sf:1 ~op:1 ~s:1 ~sh:sh_bit ~imm12:0 ~rn:rn_bits
       ~rd:rd_bits | ADD_shifted_register, (Reg rd, Reg rn, Reg rm, None) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_add_sub_shifted_register ~sf:1 ~op:0 ~s:0
       ~shift:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | (
       ADD_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind; amount
       = Six imm6 })) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
       encode_shift_type kind in encode_add_sub_shifted_register ~sf:1 ~op:0
       ~s:0 ~shift:shift_type ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | (
       ADD_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind; amount
       = Twelve imm12 })) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
       encode_shift_type kind in let imm6 = imm12 land 0x3F in
       encode_add_sub_shifted_register ~sf:1 ~op:0 ~s:0 ~shift:shift_type
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | SUB_shifted_register, (Reg
       rd, Reg rn, Reg rm, None) -> let rd_bits = Reg.encoding rd in let rn_bits
       = Reg.encoding rn in let rm_bits = Reg.encoding rm in
       encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:0 ~shift:0 ~rm:rm_bits
       ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( SUB_shifted_register, (Reg rd, Reg
       rn, Reg rm, Some (Shift { kind; amount = Six imm6 })) ) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in let shift_type = encode_shift_type kind in
       encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:0 ~shift:shift_type
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | ( SUB_shifted_register, (Reg
       rd, Reg rn, Reg rm, Some (Shift { kind; amount = Twelve imm12 })) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in let shift_type = encode_shift_type kind in
       let imm6 = imm12 land 0x3F in encode_add_sub_shifted_register ~sf:1 ~op:1
       ~s:0 ~shift:shift_type ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits |
       SUBS_shifted_register, (Reg rd, Reg rn, Reg rm, None) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:1
       ~shift:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | (
       SUBS_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind;
       amount = Six imm6 })) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
       encode_shift_type kind in encode_add_sub_shifted_register ~sf:1 ~op:1
       ~s:1 ~shift:shift_type ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | (
       SUBS_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind;
       amount = Twelve imm12 })) ) -> let rd_bits = Reg.encoding rd in let
       rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let
       shift_type = encode_shift_type kind in let imm6 = imm12 land 0x3F in
       encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:1 ~shift:shift_type
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | MOVZ, (Reg rd, Imm_nativeint
       imm, shift_opt) -> let rd_bits = Reg.encoding rd in let imm16 =
       Nativeint.to_int imm land 0xFFFF in let hw = match shift_opt with | None
       -> 0 | Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod 16 <> 0
       || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVZ: shift must be 0, 16,
       32, or 48, got %d" sh (); sh / 16 | Some (Shift { kind = LSL; amount =
       Twelve _ }) | Some (Shift { kind = ASR; _ }) | Some (Shift { kind = LSR;
       _ }) -> Misc.fatal_error "MOVZ: invalid shift amount" in encode_move_wide
       ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd:rd_bits | MOVZ, (Reg rd, Sym _, shift_opt)
       -> let rd_bits = Reg.encoding rd in let imm16 = 0 in let hw = match
       shift_opt with | None -> 0 | Some (Shift { kind = LSL; amount = Six sh })
       -> if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVZ:
       shift must be 0, 16, 32, or 48, got %d" sh (); sh / 16 | Some (Shift {
       kind = LSL; amount = Twelve _ }) | Some (Shift { kind = ASR; _ }) | Some
       (Shift { kind = LSR; _ }) -> Misc.fatal_error "MOVZ: invalid shift
       amount" in encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd:rd_bits |
       MOVZ, (Reg rd, Imm (Six imm), shift_opt) -> let rd_bits = Reg.encoding rd
       in let imm16 = imm land 0xFFFF in let hw = match shift_opt with | None ->
       0 | Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod 16 <> 0 ||
       sh < 0 || sh > 48 then Misc.fatal_errorf "MOVZ: shift must be 0, 16, 32,
       or 48, got %d" sh (); sh / 16 | Some (Shift { kind = LSL; amount = Twelve
       _ }) | Some (Shift { kind = ASR; _ }) | Some (Shift { kind = LSR; _ }) ->
       Misc.fatal_error "MOVZ: invalid shift amount" in encode_move_wide ~sf:1
       ~opc:0b10 ~hw ~imm16 ~rd:rd_bits | MOVZ, (Reg rd, Imm (Twelve imm),
       shift_opt) -> let rd_bits = Reg.encoding rd in let imm16 = imm land
       0xFFFF in let hw = match shift_opt with | None -> 0 | Some (Shift { kind
       = LSL; amount = Six sh }) -> if sh mod 16 <> 0 || sh < 0 || sh > 48 then
       Misc.fatal_errorf "MOVZ: shift must be 0, 16, 32, or 48, got %d" sh ();
       sh / 16 | Some (Shift { kind = LSL; amount = Twelve _ }) | Some (Shift {
       kind = ASR; _ }) | Some (Shift { kind = LSR; _ }) -> Misc.fatal_error
       "MOVZ: invalid shift amount" in encode_move_wide ~sf:1 ~opc:0b10 ~hw
       ~imm16 ~rd:rd_bits | MOVZ, (Reg rd, Imm_float f, shift_opt) -> let
       rd_bits = Reg.encoding rd in let imm16 = Int64.to_int
       (Int64.bits_of_float f) land 0xFFFF in let hw = match shift_opt with |
       None -> 0 | Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod 16
       <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVZ: shift must be 0,
       16, 32, or 48, got %d" sh (); sh / 16 | Some (Shift { kind = LSL; amount
       = Twelve _ }) | Some (Shift { kind = ASR; _ }) | Some (Shift { kind =
       LSR; _ }) -> Misc.fatal_error "MOVZ: invalid shift amount" in
       encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd:rd_bits | MOVN, (Reg rd,
       Imm (Twelve imm), None) -> let rd_bits = Reg.encoding rd in
       encode_move_wide ~sf:1 ~opc:0b00 ~hw:0 ~imm16:imm ~rd:rd_bits | ( MOVN, (
       Reg rd, Imm (Twelve imm), Some (Shift { kind = LSL; amount = Six sh }) )
       ) -> let rd_bits = Reg.encoding rd in let imm16 = imm land 0xFFFF in if
       sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVN: shift
       must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd:rd_bits | ( MOVN, ( Reg
       rd, Imm (Twelve imm), Some (Shift { kind = LSL; amount = Twelve sh }) ) )
       -> let rd_bits = Reg.encoding rd in let imm16 = imm land 0xFFFF in if sh
       mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVN: shift must
       be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in encode_move_wide
       ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd:rd_bits | MOVN, (Reg _, Imm (Twelve _),
       Some (Shift { kind = _; _ })) -> Misc.fatal_error "MOVN: only LSL shift
       is supported" | MOVN, (Reg rd, Imm (Six imm), None) -> let rd_bits =
       Reg.encoding rd in let imm16 = imm land 0xFFFF in encode_move_wide ~sf:1
       ~opc:0b00 ~hw:0 ~imm16 ~rd:rd_bits | ( MOVN, (Reg rd, Imm (Six imm), Some
       (Shift { kind = LSL; amount = Six sh })) ) -> let rd_bits = Reg.encoding
       rd in let imm16 = imm land 0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh >
       48 then Misc.fatal_errorf "MOVN: shift must be 0, 16, 32, or 48, got %d"
       sh (); let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16
       ~rd:rd_bits | ( MOVN, ( Reg rd, Imm (Six imm), Some (Shift { kind = LSL;
       amount = Twelve sh }) ) ) -> let rd_bits = Reg.encoding rd in let imm16 =
       imm land 0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh > 48 then
       Misc.fatal_errorf "MOVN: shift must be 0, 16, 32, or 48, got %d" sh ();
       let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16
       ~rd:rd_bits | MOVN, (Reg _, Imm (Six _), Some (Shift { kind = ASR; _ }))
       | MOVN, (Reg _, Imm (Six _), Some (Shift { kind = LSR; _ })) ->
       Misc.fatal_error "MOVN: only LSL shift is supported" | MOVN, (Reg rd, Sym
       _, shift_opt) -> let rd_bits = Reg.encoding rd in let imm16 = 0 in let hw
       = match shift_opt with | None -> 0 | Some (Shift { kind = LSL; amount =
       Six sh }) -> if sh mod 16 <> 0 || sh < 0 || sh > 48 then
       Misc.fatal_errorf "MOVN: shift must be 0, 16, 32, or 48, got %d" sh ();
       sh / 16 | Some (Shift { kind = LSL; amount = Twelve _ }) | Some (Shift {
       kind = ASR; _ }) | Some (Shift { kind = LSR; _ }) -> Misc.fatal_error
       "MOVN: invalid shift amount" in encode_move_wide ~sf:1 ~opc:0b00 ~hw
       ~imm16 ~rd:rd_bits | MOVN, (Reg rd, Imm_float f, shift_opt) -> let
       rd_bits = Reg.encoding rd in let imm16 = Int64.to_int
       (Int64.bits_of_float f) land 0xFFFF in let hw = match shift_opt with |
       None -> 0 | Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod 16
       <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVN: shift must be 0,
       16, 32, or 48, got %d" sh (); sh / 16 | Some (Shift { kind = LSL; amount
       = Twelve _ }) | Some (Shift { kind = ASR; _ }) | Some (Shift { kind =
       LSR; _ }) -> Misc.fatal_error "MOVN: invalid shift amount" in
       encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd:rd_bits | MOVN, (Reg rd,
       Imm_nativeint imm, shift_opt) -> let rd_bits = Reg.encoding rd in let
       imm16 = Nativeint.to_int imm land 0xFFFF in let hw = match shift_opt with
       | None -> 0 | Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod
       16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVN: shift must be
       0, 16, 32, or 48, got %d" sh (); sh / 16 | Some (Shift { kind = LSL;
       amount = Twelve _ }) | Some (Shift { kind = ASR; _ }) | Some (Shift {
       kind = LSR; _ }) -> Misc.fatal_error "MOVN: invalid shift amount" in
       encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg rd,
       Imm_nativeint imm, Shift { kind = LSL; amount = Six sh }) -> let rd_bits
       = Reg.encoding rd in let imm16 = Nativeint.to_int imm land 0xFFFF in if
       sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVK: shift
       must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | ( MOVK, (Reg
       rd, Imm_nativeint imm, Shift { kind = LSL; amount = Twelve sh }) ) -> let
       rd_bits = Reg.encoding rd in let imm16 = Nativeint.to_int imm land 0xFFFF
       in if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVK:
       shift must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg _,
       Imm_nativeint _, Shift { kind = _; _ }) -> Misc.fatal_error "MOVK: only
       LSL shift is supported" | MOVK, (Reg rd, Sym _, Shift { kind = LSL;
       amount = Six sh }) -> let rd_bits = Reg.encoding rd in let imm16 = 0 in
       if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVK:
       shift must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg rd,
       Sym _, Shift { kind = LSL; amount = Twelve sh }) -> let rd_bits =
       Reg.encoding rd in let imm16 = 0 in if sh mod 16 <> 0 || sh < 0 || sh >
       48 then Misc.fatal_errorf "MOVK: shift must be 0, 16, 32, or 48, got %d"
       sh (); let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16
       ~rd:rd_bits | MOVK, (Reg _, Sym _, Shift { kind = _; _ }) ->
       Misc.fatal_error "MOVK: only LSL shift is supported" | MOVK, (Reg rd, Imm
       (Six imm), Shift { kind = LSL; amount = Six sh }) -> let rd_bits =
       Reg.encoding rd in let imm16 = imm land 0xFFFF in if sh mod 16 <> 0 || sh
       < 0 || sh > 48 then Misc.fatal_errorf "MOVK: shift must be 0, 16, 32, or
       48, got %d" sh (); let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b11
       ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg rd, Imm (Six imm), Shift { kind =
       LSL; amount = Twelve sh }) -> let rd_bits = Reg.encoding rd in let imm16
       = imm land 0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh > 48 then
       Misc.fatal_errorf "MOVK: shift must be 0, 16, 32, or 48, got %d" sh ();
       let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16
       ~rd:rd_bits | MOVK, (Reg rd, Imm (Twelve imm), Shift { kind = LSL; amount
       = Six sh }) -> let rd_bits = Reg.encoding rd in let imm16 = imm land
       0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf
       "MOVK: shift must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | ( MOVK, (Reg
       rd, Imm (Twelve imm), Shift { kind = LSL; amount = Twelve sh }) ) -> let
       rd_bits = Reg.encoding rd in let imm16 = imm land 0xFFFF in if sh mod 16
       <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf "MOVK: shift must be 0,
       16, 32, or 48, got %d" sh (); let hw = sh / 16 in encode_move_wide ~sf:1
       ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg _, Imm _, Shift { kind = _;
       _ }) -> Misc.fatal_error "MOVK: only LSL shift is supported" | MOVK, (Reg
       rd, Imm_float f, Shift { kind = LSL; amount = Six sh }) -> let rd_bits =
       Reg.encoding rd in let imm16 = Int64.to_int (Int64.bits_of_float f) land
       0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf
       "MOVK: shift must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg rd,
       Imm_float f, Shift { kind = LSL; amount = Twelve sh }) -> let rd_bits =
       Reg.encoding rd in let imm16 = Int64.to_int (Int64.bits_of_float f) land
       0xFFFF in if sh mod 16 <> 0 || sh < 0 || sh > 48 then Misc.fatal_errorf
       "MOVK: shift must be 0, 16, 32, or 48, got %d" sh (); let hw = sh / 16 in
       encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd:rd_bits | MOVK, (Reg _,
       Imm_float _, Shift { kind = _; _ }) -> Misc.fatal_error "MOVK: only LSL
       shift is supported" | UBFM, (Reg rd, Reg rn, Imm (Six immr), Imm (Six
       imms)) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
       in encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr ~imms ~rn:rn_bits
       ~rd:rd_bits | UBFM, (Reg rd, Reg rn, Imm (Six immr), Imm (Twelve imms))
       -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       imms = imms land 0x3F in encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr ~imms
       ~rn:rn_bits ~rd:rd_bits | UBFM, (Reg rd, Reg rn, Imm (Twelve immr), Imm
       (Six imms)) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let immr = immr land 0x3F in encode_bitfield ~sf:1
       ~opc:0b10 ~n:1 ~immr ~imms ~rn:rn_bits ~rd:rd_bits | UBFM, (Reg rd, Reg
       rn, Imm (Twelve immr), Imm (Twelve imms)) -> let rd_bits = Reg.encoding
       rd in let rn_bits = Reg.encoding rn in let immr = immr land 0x3F in let
       imms = imms land 0x3F in encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr ~imms
       ~rn:rn_bits ~rd:rd_bits | SBFM, (Reg rd, Reg rn, Imm (Six immr), Imm (Six
       imms)) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
       in encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr ~imms ~rn:rn_bits
       ~rd:rd_bits | SBFM, (Reg rd, Reg rn, Imm (Six immr), Imm (Twelve imms))
       -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       imms = imms land 0x3F in encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr ~imms
       ~rn:rn_bits ~rd:rd_bits | SBFM, (Reg rd, Reg rn, Imm (Twelve immr), Imm
       (Six imms)) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let immr = immr land 0x3F in encode_bitfield ~sf:1
       ~opc:0b00 ~n:1 ~immr ~imms ~rn:rn_bits ~rd:rd_bits | SBFM, (Reg rd, Reg
       rn, Imm (Twelve immr), Imm (Twelve imms)) -> let rd_bits = Reg.encoding
       rd in let rn_bits = Reg.encoding rn in let immr = immr land 0x3F in let
       imms = imms land 0x3F in encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr ~imms
       ~rn:rn_bits ~rd:rd_bits | UBFM, (Reg rd, Reg rn, Imm _, Sym _) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits
       ~rd:rd_bits | UBFM, (Reg rd, Reg rn, Sym _, Imm _) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in encode_bitfield ~sf:1
       ~opc:0b10 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | UBFM, (Reg rd,
       Reg rn, Sym _, Sym _) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr:0 ~imms:0
       ~rn:rn_bits ~rd:rd_bits | SBFM, (Reg rd, Reg rn, Imm _, Sym _) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits
       ~rd:rd_bits | SBFM, (Reg rd, Reg rn, Sym _, Imm _) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in encode_bitfield ~sf:1
       ~opc:0b00 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | SBFM, (Reg rd,
       Reg rn, Sym _, Sym _) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr:0 ~imms:0
       ~rn:rn_bits ~rd:rd_bits | SDIV, (Reg rd, Reg rn, Reg rm) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in encode_data_proc_2_source ~sf:1 ~s:0 ~opcode:0b000011
       ~rm:rm_bits ~rn:rn_bits ~rd:rd_bits | LSLV, (Reg rd, Reg rn, Reg rm) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_data_proc_2_source ~sf:1 ~s:0
       ~opcode:0b001000 ~rm:rm_bits ~rn:rn_bits ~rd:rd_bits | LSRV, (Reg rd, Reg
       rn, Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in
       encode_data_proc_2_source ~sf:1 ~s:0 ~opcode:0b001001 ~rm:rm_bits
       ~rn:rn_bits ~rd:rd_bits | ASRV, (Reg rd, Reg rn, Reg rm) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in encode_data_proc_2_source ~sf:1 ~s:0 ~opcode:0b001010
       ~rm:rm_bits ~rn:rn_bits ~rd:rd_bits | MADD, (Reg rd, Reg rn, Reg rm, Reg
       ra) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
       encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b000 ~o0:0 ~rm:rm_bits
       ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | MSUB, (Reg rd, Reg rn, Reg rm, Reg
       ra) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
       encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b000 ~o0:1 ~rm:rm_bits
       ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | AND_shifted_register, (Reg rd, Reg
       rn, Reg rm, None) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in
       encode_logical_shifted_register ~sf:1 ~opc:0b00 ~shift:0 ~n:0 ~rm:rm_bits
       ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( AND_shifted_register, (Reg rd, Reg
       rn, Reg rm, Some (Shift { kind; amount = Six imm6 })) ) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in let shift_type = encode_shift_type kind in
       encode_logical_shifted_register ~sf:1 ~opc:0b00 ~shift:shift_type ~n:0
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | ( AND_shifted_register, (Reg
       rd, Reg rn, Reg rm, Some (Shift { kind; amount = Twelve imm12 })) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in let shift_type = encode_shift_type kind in
       let imm6 = imm12 land 0x3F in encode_logical_shifted_register ~sf:1
       ~opc:0b00 ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits
       ~rd:rd_bits | ORR_shifted_register, (Reg rd, Reg rn, Reg rm, None) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_logical_shifted_register ~sf:1 ~opc:0b01
       ~shift:0 ~n:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | (
       ORR_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind; amount
       = Six imm6 })) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
       encode_shift_type kind in encode_logical_shifted_register ~sf:1 ~opc:0b01
       ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | (
       ORR_shifted_register, (Reg rd, Reg rn, Reg rm, Some (Shift { kind; amount
       = Twelve imm12 })) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
       encode_shift_type kind in let imm6 = imm12 land 0x3F in
       encode_logical_shifted_register ~sf:1 ~opc:0b01 ~shift:shift_type ~n:0
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | EOR_shifted_register, (Reg
       rd, Reg rn, Reg rm, None) -> let rd_bits = Reg.encoding rd in let rn_bits
       = Reg.encoding rn in let rm_bits = Reg.encoding rm in
       encode_logical_shifted_register ~sf:1 ~opc:0b10 ~shift:0 ~n:0 ~rm:rm_bits
       ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( EOR_shifted_register, (Reg rd, Reg
       rn, Reg rm, Some (Shift { kind; amount = Six imm6 })) ) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in let shift_type = encode_shift_type kind in
       encode_logical_shifted_register ~sf:1 ~opc:0b10 ~shift:shift_type ~n:0
       ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | ( EOR_shifted_register, (Reg
       rd, Reg rn, Reg rm, Some (Shift { kind; amount = Twelve imm12 })) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in let shift_type = encode_shift_type kind in
       let imm6 = imm12 land 0x3F in encode_logical_shifted_register ~sf:1
       ~opc:0b10 ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits
       ~rd:rd_bits | RBIT, (Reg rd, Reg rn) -> let rd_bits = Reg.encoding rd in
       let rn_bits = Reg.encoding rn in encode_data_proc_1_source ~sf:1 ~s:0
       ~opcode2:0b00000 ~opcode:0b000000 ~rn:rn_bits ~rd:rd_bits | REV16, (Reg
       rd, Reg rn) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000
       ~opcode:0b000001 ~rn:rn_bits ~rd:rd_bits | REV, (Reg rd, Reg rn) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000011
       ~rn:rn_bits ~rd:rd_bits | CLZ, (Reg rd, Reg rn) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000100
       ~rn:rn_bits ~rd:rd_bits | CNT, (Reg rd, Reg rn) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in
       encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000111
       ~rn:rn_bits ~rd:rd_bits | SMULH, (Reg rd, Reg rn, Reg rm) -> let rd_bits
       = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b010
       ~o0:0 ~rm:rm_bits ~ra:0b11111 ~rn:rn_bits ~rd:rd_bits | UMULH, (Reg rd,
       Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits =
       Reg.encoding rn in let rm_bits = Reg.encoding rm in
       encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b110 ~o0:0 ~rm:rm_bits
       ~ra:0b11111 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector, (Reg ({ reg_name =
       Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) -> let rd_bits =
       Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
       Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V1D); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b11
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( SQADD_vector,
       (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b11
       ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( ADD_vector, (Reg
       ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b11
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ADD_vector, (Reg {
       reg_name = _; _ }, _, _) -> assert false (* TODO XXX *) | ( SUB_vector,
       (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b00
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b00
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b01
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b01
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b10
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b10
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( SUB_vector, (Reg
       ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b11
       ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | SUB_vector, (Reg {
       reg_name = _; _ }, _, _) -> assert false (* TODO XXX *) | ( AND_vector,
       (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | ( AND_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | AND_vector, (Reg {
       reg_name = _; _ }, _, _) -> assert false (* TODO XXX *) | ( ORR_vector,
       (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | ( ORR_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | ORR_vector, (Reg {
       reg_name = _; _ }, _, _) -> assert false (* TODO XXX *) | ( EOR_vector,
       (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b00
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | ( EOR_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b00
       ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | EOR_vector, (Reg {
       reg_name = _; _ }, _, _) -> assert false (* TODO XXX *) | ( SMAX_vector,
       (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm) ) ->
       let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
       rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | ( SMAX_vector, (Reg
       ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | ( SMAX_vector, (Reg
       ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | ( SMAX_vector, (Reg
       ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b01
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | ( SMAX_vector, (Reg
       ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | ( SMAX_vector, (Reg
       ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm) ) -> let
       rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits
       = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10
       ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits *)
    | SMIN_vector, op -> (
      match op with
      | Reg { reg_name = Neon (Vector (V8B | V16B | V4H | V8H | V2S | V4S)); _ }
        ->
        failwith "foo"
        (* | Reg { reg_name = Neon (Vector (V2D|V1D)); _ } -> . *))

  (* (Reg ({ reg_name = Neon (Vector V8B); _ } as rd)) ) -> failwith "foo" *)
  (* | ( SMIN_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn,
     Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     SMIN_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     SMIN_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     SMIN_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     SMIN_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     SMIN_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits *)
  (* | SQADD_vector, (Reg { reg_name = Neon _; _ }, _, _) -> assert false (*
     TODO XXX *) | ( UMAX_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as
     rd), Reg rn, Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( UMAX_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn,
     Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | (
     UMAX_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | (
     UMAX_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | (
     UMAX_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | (
     UMAX_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     UMIN_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     MUL_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn,
     Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SQSUB_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b11 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn,
     Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQADD_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b11 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn,
     Reg rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     UQSUB_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b11 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     SSHL_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b11 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     USHL_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b11 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits | (
     FADD_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FADD_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FADD_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FSUB_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FSUB_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FSUB_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b11 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits | (
     FMUL_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits | (
     FMUL_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits | (
     FMUL_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits | (
     FDIV_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits | (
     FDIV_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b00 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits | (
     FDIV_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1
     ~size:0b01 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits | (
     FMAX_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMAX_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMAX_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMIN_vector, (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMIN_vector, (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMIN_vector, (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg
     rm) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b11 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits | (
     FMOV_scalar_immediate, (Reg ({ reg_name = Neon (Scalar S); _ } as rd),
     Imm_float _f) ) -> let rd_bits = Reg.encoding rd in let imm8 = 0 in
     encode_fp_immediate ~ftype:0b00 ~imm8 ~rd:rd_bits | (
     FMOV_scalar_immediate, (Reg ({ reg_name = Neon (Scalar D); _ } as rd),
     Imm_float _f) ) -> let rd_bits = Reg.encoding rd in let imm8 = 0 in
     encode_fp_immediate ~ftype:0b01 ~imm8 ~rd:rd_bits | (
     FMOV_scalar_immediate, (Reg ({ reg_name = Neon (Scalar H); _ } as rd),
     Imm_float _f) ) -> let rd_bits = Reg.encoding rd in let imm8 = 0 in
     encode_fp_immediate ~ftype:0b11 ~imm8 ~rd:rd_bits | FADD, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0010 ~rn:rn_bits ~rd:rd_bits | FADD, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0010 ~rn:rn_bits
     ~rd:rd_bits | FADD, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0010 ~rn:rn_bits ~rd:rd_bits | FSUB, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0011 ~rn:rn_bits ~rd:rd_bits | FSUB, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0011 ~rn:rn_bits
     ~rd:rd_bits | FSUB, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0011 ~rn:rn_bits ~rd:rd_bits | FMUL, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0000 ~rn:rn_bits ~rd:rd_bits | FMUL, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0000 ~rn:rn_bits
     ~rd:rd_bits | FMUL, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0000 ~rn:rn_bits ~rd:rd_bits | FDIV, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0001 ~rn:rn_bits ~rd:rd_bits | FDIV, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0001 ~rn:rn_bits
     ~rd:rd_bits | FDIV, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0001 ~rn:rn_bits ~rd:rd_bits | FMAX, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0100 ~rn:rn_bits ~rd:rd_bits | FMAX, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0100 ~rn:rn_bits
     ~rd:rd_bits | FMAX, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0100 ~rn:rn_bits ~rd:rd_bits | FMIN, (Reg ({ reg_name
     = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0101 ~rn:rn_bits ~rd:rd_bits | FMIN, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b0101 ~rn:rn_bits
     ~rd:rd_bits | FMIN, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn,
     Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn
     in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b0101 ~rn:rn_bits ~rd:rd_bits | FNMUL, (Reg ({
     reg_name = Neon (Scalar S); _ } as rd), Reg rn, Reg rm) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b1000 ~rn:rn_bits ~rd:rd_bits | FNMUL, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_fp_2_source ~ftype:0b01 ~rm:rm_bits ~opcode:0b1000 ~rn:rn_bits
     ~rd:rd_bits | FNMUL, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg
     rn, Reg rm) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_fp_2_source ~ftype:0b11
     ~rm:rm_bits ~opcode:0b1000 ~rn:rn_bits ~rd:rd_bits | ( FCSEL, ( Reg ({
     reg_name = Neon (Scalar S); _ } as rd), Reg rn, Reg rm, Cond cond ) ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in let cond_bits = encode_condition cond in
     encode_fp_cond_select ~ftype:0b00 ~rm:rm_bits ~cond:cond_bits ~rn:rn_bits
     ~rd:rd_bits | ( FCSEL, ( Reg ({ reg_name = Neon (Scalar D); _ } as rd), Reg
     rn, Reg rm, Cond cond ) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let cond_bits =
     encode_condition cond in encode_fp_cond_select ~ftype:0b01 ~rm:rm_bits
     ~cond:cond_bits ~rn:rn_bits ~rd:rd_bits | ( FCSEL, ( Reg ({ reg_name = Neon
     (Scalar H); _ } as rd), Reg rn, Reg rm, Cond cond ) ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let cond_bits = encode_condition cond in
     encode_fp_cond_select ~ftype:0b11 ~rm:rm_bits ~cond:cond_bits ~rn:rn_bits
     ~rd:rd_bits | ( FMADD, (Reg ({ reg_name = Neon (Scalar S); _ } as rd), Reg
     rn, Reg rm, Reg ra) ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:0 ~rm:rm_bits ~o0:0
     ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( FMADD, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let ra_bits = Reg.encoding ra in encode_fp_3_source
     ~ftype:0b01 ~o1:0 ~rm:rm_bits ~o0:0 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | (
     FMADD, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn, Reg rm, Reg
     ra) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b11 ~o1:0 ~rm:rm_bits ~o0:0 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( FMSUB, (Reg ({ reg_name = Neon (Scalar S); _ }
     as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:0 ~rm:rm_bits ~o0:1
     ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( FMSUB, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let ra_bits = Reg.encoding ra in encode_fp_3_source
     ~ftype:0b01 ~o1:0 ~rm:rm_bits ~o0:1 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | (
     FMSUB, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn, Reg rm, Reg
     ra) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b11 ~o1:0 ~rm:rm_bits ~o0:1 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( FNMADD, (Reg ({ reg_name = Neon (Scalar S); _ }
     as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:1 ~rm:rm_bits ~o0:0
     ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( FNMADD, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let ra_bits = Reg.encoding ra in encode_fp_3_source
     ~ftype:0b01 ~o1:1 ~rm:rm_bits ~o0:0 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | (
     FNMADD, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn, Reg rm, Reg
     ra) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b11 ~o1:1 ~rm:rm_bits ~o0:0 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( FNMSUB, (Reg ({ reg_name = Neon (Scalar S); _ }
     as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:1 ~rm:rm_bits ~o0:1
     ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( FNMSUB, (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra) ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let ra_bits = Reg.encoding ra in encode_fp_3_source
     ~ftype:0b01 ~o1:1 ~rm:rm_bits ~o0:1 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | (
     FNMSUB, (Reg ({ reg_name = Neon (Scalar H); _ } as rd), Reg rn, Reg rm, Reg
     ra) ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b11 ~o1:1 ~rm:rm_bits ~o0:1 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits (* Load register (literal) - C4.1.96.19 *) (* XXX
     these need more work *) | LDR, (Reg ({ reg_name = GP W; _ } as rt), Mem
     (Offset (_, Symbol _))) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b00 ~v:0 ~imm19:0 ~rt:rt_bits | LDR, (Reg ({
     reg_name = GP X; _ } as rt), Mem (Offset (_, Symbol _))) -> let rt_bits =
     Reg.encoding rt in encode_load_literal ~opc:0b01 ~v:0 ~imm19:0 ~rt:rt_bits
     | LDR, (Reg ({ reg_name = GP LR; _ } as rt), Mem (Offset (_, Symbol _))) ->
     let rt_bits = Reg.encoding rt in encode_load_literal ~opc:0b01 ~v:0
     ~imm19:0 ~rt:rt_bits | LDRSW, (Reg ({ reg_name = GP X; _ } as rt), Mem
     (Offset (_, Symbol _))) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b10 ~v:0 ~imm19:0 ~rt:rt_bits | (
     LDR_simd_and_fp, ( Reg ({ reg_name = Neon (Scalar S); _ } as rt), Mem
     (Offset (_, Symbol _)) ) ) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b00 ~v:1 ~imm19:0 ~rt:rt_bits | (
     LDR_simd_and_fp, ( Reg ({ reg_name = Neon (Scalar D); _ } as rt), Mem
     (Offset (_, Symbol _)) ) ) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b01 ~v:1 ~imm19:0 ~rt:rt_bits | (
     LDR_simd_and_fp, ( Reg ({ reg_name = Neon (Scalar Q); _ } as rt), Mem
     (Offset (_, Symbol _)) ) ) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b10 ~v:1 ~imm19:0 ~rt:rt_bits | ( SMAX_vector, (
     Reg { reg_name = GP _ | Neon (Vector (V1D | V2D) | Scalar _ | Lane _); _ },
     _, _ ) ) -> assert false *)
  (* | ( SMIN_vector, ( Reg { reg_name = GP _ | Neon (Vector (V1D | V2D) |
     Scalar _ | Lane _); _ }, _, _ ) ) -> assert false *)
  (* | ( UMAX_vector, ( Reg { reg_name = Neon (Vector (V1D | V2D) | Scalar _ |
     Lane _); _ }, _, _ ) ) | UMAX_vector, (Reg { reg_name = GP _; _ }, _, _) |
     ( UMIN_vector, ( Reg { reg_name = Neon (Vector (V1D | V2D) | Scalar _ |
     Lane _); _ }, _, _ ) ) | UMIN_vector, (Reg { reg_name = GP _; _ }, _, _) ->
     assert false | SQADD_vector, (Reg { reg_name = GP _; _ }, _, _) -> assert
     false (* XXX TODO *) | MUL_vector, (Reg { reg_name = Neon (Vector V1D); _
     }, _, _) -> assert false (* XXX TODO *) | MUL_vector, (Reg { reg_name =
     Neon (Vector V2D); _ }, _, _) -> assert false (* XXX TODO *) | MUL_vector,
     (Reg { reg_name = Neon (Scalar _); _ }, _, _) -> assert false (* XXX TODO
     *) | MUL_vector, (Reg { reg_name = Neon (Lane _); _ }, _, _) -> assert
     false (* XXX TODO *) | MUL_vector, (Reg { reg_name = GP _; _ }, _, _) ->
     assert false (* XXX TODO *) | SQSUB_vector, (Reg { reg_name = Neon (Vector
     V1D); _ }, _, _) -> assert false (* XXX TODO *) | SQSUB_vector, (Reg {
     reg_name = Neon (Scalar _); _ }, _, _) -> assert false (* XXX TODO *) |
     SQSUB_vector, (Reg { reg_name = Neon (Lane _); _ }, _, _) -> assert false
     (* XXX TODO *) | SQSUB_vector, (Reg { reg_name = GP _; _ }, _, _) -> assert
     false (* XXX TODO *) | UQADD_vector, (Reg { reg_name = Neon (Vector V1D); _
     }, _, _) -> assert false (* XXX TODO *) | UQADD_vector, (Reg { reg_name =
     Neon (Scalar _); _ }, _, _) -> assert false (* XXX TODO *) | UQADD_vector,
     (Reg { reg_name = Neon (Lane _); _ }, _, _) -> assert false (* XXX TODO *)
     | UQADD_vector, (Reg { reg_name = GP _; _ }, _, _) -> assert false (* XXX
     TODO *) | UQSUB_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) ->
     assert false (* XXX TODO *) | UQSUB_vector, (Reg { reg_name = Neon (Scalar
     _); _ }, _, _) -> assert false (* XXX TODO *) | UQSUB_vector, (Reg {
     reg_name = Neon (Lane _); _ }, _, _) -> assert false (* XXX TODO *) |
     UQSUB_vector, (Reg { reg_name = GP _; _ }, _, _) -> assert false (* XXX
     TODO *) | SSHL_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) ->
     assert false (* XXX TODO *) | SSHL_vector, (Reg { reg_name = Neon (Scalar
     _); _ }, _, _) -> assert false (* XXX TODO *) | SSHL_vector, (Reg {
     reg_name = Neon (Lane _); _ }, _, _) -> assert false (* XXX TODO *) |
     SSHL_vector, (Reg { reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO
     *) | USHL_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | USHL_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | USHL_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | USHL_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FADD_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FADD_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FADD_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FADD_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FADD_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FADD_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FADD_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FADD_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FSUB_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FSUB_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FSUB_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FSUB_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FSUB_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FSUB_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FSUB_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FSUB_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FMUL_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FMUL_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FMUL_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FMUL_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FMUL_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FMUL_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FMUL_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FMUL_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FDIV_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FDIV_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FDIV_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FDIV_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FDIV_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FDIV_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FDIV_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FDIV_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FMAX_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FMAX_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FMAX_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FMAX_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FMAX_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FMAX_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FMAX_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FMAX_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FMIN_vector,
     (Reg { reg_name = Neon (Vector V8B); _ }, _, _) -> assert false (* XXX TODO
     *) | FMIN_vector, (Reg { reg_name = Neon (Vector V16B); _ }, _, _) ->
     assert false (* XXX TODO *) | FMIN_vector, (Reg { reg_name = Neon (Vector
     V4H); _ }, _, _) -> assert false (* XXX TODO *) | FMIN_vector, (Reg {
     reg_name = Neon (Vector V8H); _ }, _, _) -> assert false (* XXX TODO *) |
     FMIN_vector, (Reg { reg_name = Neon (Vector V1D); _ }, _, _) -> assert
     false (* XXX TODO *) | FMIN_vector, (Reg { reg_name = Neon (Scalar _); _ },
     _, _) -> assert false (* XXX TODO *) | FMIN_vector, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FMIN_vector, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) |
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar S); _ }, Sym _) ->
     assert false (* XXX TODO *) | FMOV_scalar_immediate, (Reg { reg_name = Neon
     (Scalar S); _ }, Imm _) -> assert false (* XXX TODO *) | (
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar S); _ },
     Imm_nativeint _) ) -> assert false (* XXX TODO *) | FMOV_scalar_immediate,
     (Reg { reg_name = Neon (Scalar D); _ }, Sym _) -> assert false (* XXX TODO
     *) | FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar D); _ }, Imm _)
     -> assert false (* XXX TODO *) | ( FMOV_scalar_immediate, (Reg { reg_name =
     Neon (Scalar D); _ }, Imm_nativeint _) ) -> assert false (* XXX TODO *) |
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar H); _ }, Sym _) ->
     assert false (* XXX TODO *) | FMOV_scalar_immediate, (Reg { reg_name = Neon
     (Scalar H); _ }, Imm _) -> assert false (* XXX TODO *) | (
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar H); _ },
     Imm_nativeint _) ) -> assert false (* XXX TODO *) | FMOV_scalar_immediate,
     (Reg { reg_name = Neon (Scalar B); _ }, Sym _) -> assert false (* XXX TODO
     *) | FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar B); _ }, Imm _)
     -> assert false (* XXX TODO *) | ( FMOV_scalar_immediate, (Reg { reg_name =
     Neon (Scalar B); _ }, Imm_nativeint _) ) -> assert false (* XXX TODO *) | (
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar B); _ }, Imm_float _)
     ) -> assert false (* XXX TODO *) | FMOV_scalar_immediate, (Reg { reg_name =
     Neon (Scalar Q); _ }, Sym _) -> assert false (* XXX TODO *) |
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar Q); _ }, Imm _) ->
     assert false (* XXX TODO *) | ( FMOV_scalar_immediate, (Reg { reg_name =
     Neon (Scalar Q); _ }, Imm_nativeint _) ) -> assert false (* XXX TODO *) | (
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Scalar Q); _ }, Imm_float _)
     ) -> assert false (* XXX TODO *) | FMOV_scalar_immediate, (Reg { reg_name =
     Neon (Vector _); _ }, _) -> assert false (* XXX TODO *) |
     FMOV_scalar_immediate, (Reg { reg_name = Neon (Lane _); _ }, _) -> assert
     false (* XXX TODO *) | FMOV_scalar_immediate, (Reg { reg_name = GP _; _ },
     _) -> assert false (* XXX TODO *) | FADD, (Reg { reg_name = Neon (Scalar
     B); _ }, _, _) -> assert false (* XXX TODO *) | FADD, (Reg { reg_name =
     Neon (Scalar Q); _ }, _, _) -> assert false (* XXX TODO *) | FADD, (Reg {
     reg_name = Neon (Vector _); _ }, _, _) -> assert false (* XXX TODO *) |
     FADD, (Reg { reg_name = Neon (Lane _); _ }, _, _) -> assert false (* XXX
     TODO *) | FADD, (Reg { reg_name = GP _; _ }, _, _) -> assert false (* XXX
     TODO *) | FSUB, (Reg { reg_name = Neon (Scalar B); _ }, _, _) -> assert
     false (* XXX TODO *) | FSUB, (Reg { reg_name = Neon (Scalar Q); _ }, _, _)
     -> assert false (* XXX TODO *) | FSUB, (Reg { reg_name = Neon (Vector _); _
     }, _, _) -> assert false (* XXX TODO *) | FSUB, (Reg { reg_name = Neon
     (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FSUB, (Reg { reg_name
     = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FMUL, (Reg { reg_name =
     Neon (Scalar B); _ }, _, _) -> assert false (* XXX TODO *) | FMUL, (Reg {
     reg_name = Neon (Scalar Q); _ }, _, _) -> assert false (* XXX TODO *) |
     FMUL, (Reg { reg_name = Neon (Vector _); _ }, _, _) -> assert false (* XXX
     TODO *) | FMUL, (Reg { reg_name = Neon (Lane _); _ }, _, _) -> assert false
     (* XXX TODO *) | FMUL, (Reg { reg_name = GP _; _ }, _, _) -> assert false
     (* XXX TODO *) | FDIV, (Reg { reg_name = Neon (Scalar B); _ }, _, _) ->
     assert false (* XXX TODO *) | FDIV, (Reg { reg_name = Neon (Scalar Q); _ },
     _, _) -> assert false (* XXX TODO *) | FDIV, (Reg { reg_name = Neon (Vector
     _); _ }, _, _) -> assert false (* XXX TODO *) | FDIV, (Reg { reg_name =
     Neon (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FDIV, (Reg {
     reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FMAX, (Reg {
     reg_name = Neon (Scalar B); _ }, _, _) -> assert false (* XXX TODO *) |
     FMAX, (Reg { reg_name = Neon (Scalar Q); _ }, _, _) -> assert false (* XXX
     TODO *) | FMAX, (Reg { reg_name = Neon (Vector _); _ }, _, _) -> assert
     false (* XXX TODO *) | FMAX, (Reg { reg_name = Neon (Lane _); _ }, _, _) ->
     assert false (* XXX TODO *) | FMAX, (Reg { reg_name = GP _; _ }, _, _) ->
     assert false (* XXX TODO *) | FMIN, (Reg { reg_name = Neon (Scalar B); _ },
     _, _) -> assert false (* XXX TODO *) | FMIN, (Reg { reg_name = Neon (Scalar
     Q); _ }, _, _) -> assert false (* XXX TODO *) | FMIN, (Reg { reg_name =
     Neon (Vector _); _ }, _, _) -> assert false (* XXX TODO *) | FMIN, (Reg {
     reg_name = Neon (Lane _); _ }, _, _) -> assert false (* XXX TODO *) | FMIN,
     (Reg { reg_name = GP _; _ }, _, _) -> assert false (* XXX TODO *) | FNMUL,
     (Reg { reg_name = Neon (Scalar B); _ }, _, _) -> assert false (* XXX TODO
     *) | FNMUL, (Reg { reg_name = Neon (Scalar Q); _ }, _, _) -> assert false
     (* XXX TODO *) | FNMUL, (Reg { reg_name = Neon (Vector _); _ }, _, _) ->
     assert false (* XXX TODO *) | FNMUL, (Reg { reg_name = Neon (Lane _); _ },
     _, _) -> assert false (* XXX TODO *) | FNMUL, (Reg { reg_name = GP _; _ },
     _, _) -> assert false (* XXX TODO *) | FMADD, (Reg { reg_name = Neon
     (Scalar B); _ }, _, _, _) -> assert false (* XXX TODO *) | FMADD, (Reg {
     reg_name = Neon (Scalar Q); _ }, _, _, _) -> assert false (* XXX TODO *) |
     FMADD, (Reg { reg_name = Neon (Vector _); _ }, _, _, _) -> assert false (*
     XXX TODO *) | FMADD, (Reg { reg_name = Neon (Lane _); _ }, _, _, _) ->
     assert false (* XXX TODO *) | FMADD, (Reg { reg_name = GP _; _ }, _, _, _)
     -> assert false (* XXX TODO *) | FMSUB, (Reg { reg_name = Neon (Scalar B);
     _ }, _, _, _) -> assert false (* XXX TODO *) | FMSUB, (Reg { reg_name =
     Neon (Scalar Q); _ }, _, _, _) -> assert false (* XXX TODO *) | FMSUB, (Reg
     { reg_name = Neon (Vector _); _ }, _, _, _) -> assert false (* XXX TODO *)
     | FMSUB, (Reg { reg_name = Neon (Lane _); _ }, _, _, _) -> assert false (*
     XXX TODO *) | FMSUB, (Reg { reg_name = GP _; _ }, _, _, _) -> assert false
     (* XXX TODO *) | FNMADD, (Reg { reg_name = Neon (Scalar B); _ }, _, _, _)
     -> assert false (* XXX TODO *) | FNMADD, (Reg { reg_name = Neon (Scalar Q);
     _ }, _, _, _) -> assert false (* XXX TODO *) | FNMADD, (Reg { reg_name =
     Neon (Vector _); _ }, _, _, _) -> assert false (* XXX TODO *) | FNMADD,
     (Reg { reg_name = Neon (Lane _); _ }, _, _, _) -> assert false (* XXX TODO
     *) | FNMADD, (Reg { reg_name = GP _; _ }, _, _, _) -> assert false (* XXX
     TODO *) | FNMSUB, (Reg { reg_name = Neon (Scalar B); _ }, _, _, _) ->
     assert false (* XXX TODO *) | FNMSUB, (Reg { reg_name = Neon (Scalar Q); _
     }, _, _, _) -> assert false (* XXX TODO *) | FNMSUB, (Reg { reg_name = Neon
     (Vector _); _ }, _, _, _) -> assert false (* XXX TODO *) | FNMSUB, (Reg {
     reg_name = Neon (Lane _); _ }, _, _, _) -> assert false (* XXX TODO *) |
     FNMSUB, (Reg { reg_name = GP _; _ }, _, _, _) -> assert false (* XXX TODO
     *) | FABS, (Reg { reg_name = Neon (Scalar B); _ }, _) -> assert false (*
     XXX TODO *) | FABS, (Reg { reg_name = Neon (Scalar H); _ }, _) -> assert
     false (* XXX TODO *) | FABS, (Reg { reg_name = Neon (Scalar S); _ }, _) ->
     assert false (* XXX TODO *) | FABS, (Reg { reg_name = Neon (Scalar D); _ },
     _) -> assert false (* XXX TODO *) | FABS, (Reg { reg_name = Neon (Scalar
     Q); _ }, _) -> assert false (* XXX TODO *) | FABS, (Reg { reg_name = Neon
     (Vector _); _ }, _) -> assert false (* XXX TODO *) | FABS, (Reg { reg_name
     = Neon (Lane _); _ }, _) -> assert false (* XXX TODO *) | FABS, (Reg {
     reg_name = GP _; _ }, _) -> assert false (* XXX TODO *) | FNEG, (Reg {
     reg_name = Neon (Scalar B); _ }, _) -> assert false (* XXX TODO *) | FNEG,
     (Reg { reg_name = Neon (Scalar H); _ }, _) -> assert false (* XXX TODO *) |
     FNEG, (Reg { reg_name = Neon (Scalar S); _ }, _) -> assert false (* XXX
     TODO *) | FNEG, (Reg { reg_name = Neon (Scalar D); _ }, _) -> assert false
     (* XXX TODO *) | FNEG, (Reg { reg_name = Neon (Scalar Q); _ }, _) -> assert
     false (* XXX TODO *) | FNEG, (Reg { reg_name = Neon (Vector _); _ }, _) ->
     assert false (* XXX TODO *) | FNEG, (Reg { reg_name = Neon (Lane _); _ },
     _) -> assert false (* XXX TODO *) | FNEG, (Reg { reg_name = GP _; _ }, _)
     -> assert false (* XXX TODO *) | FSQRT, (Reg { reg_name = Neon (Scalar B);
     _ }, _) -> assert false (* XXX TODO *) | FSQRT, (Reg { reg_name = Neon
     (Scalar H); _ }, _) -> assert false (* XXX TODO *) | FSQRT, (Reg { reg_name
     = Neon (Scalar S); _ }, _) -> assert false (* XXX TODO *) | FSQRT, (Reg {
     reg_name = Neon (Scalar D); _ }, _) -> assert false (* XXX TODO *) | FSQRT,
     (Reg { reg_name = Neon (Scalar Q); _ }, _) -> assert false (* XXX TODO *) |
     FSQRT, (Reg { reg_name = Neon (Vector _); _ }, _) -> assert false (* XXX
     TODO *) | FSQRT, (Reg { reg_name = Neon (Lane _); _ }, _) -> assert false
     (* XXX TODO *) | FSQRT, (Reg { reg_name = GP _; _ }, _) -> assert false (*
     XXX TODO *) | FCVT, _ -> assert false (* XXX TODO *) | FCVTZS, _ -> assert
     false (* XXX TODO *) | FCVTNS, _ -> assert false (* XXX TODO *) | SCVTF, _
     -> assert false (* XXX TODO *) | FCMP, _ -> assert false (* XXX TODO *) |
     FCSEL, _ -> assert false (* XXX TODO *) | FMOV_general_or_register, _ ->
     assert false (* XXX TODO *) | LDR, _ -> assert false (* XXX TODO *) | STR,
     _ -> assert false (* XXX TODO *) | LDP, _ -> assert false (* XXX TODO *) |
     STP, _ -> assert false (* XXX TODO *) | LDRB, _ -> assert false (* XXX TODO
     *) | LDRH, _ -> assert false (* XXX TODO *) | LDRSB, _ -> assert false (*
     XXX TODO *) | LDRSH, _ -> assert false (* XXX TODO *) | STRB, _ -> assert
     false (* XXX TODO *) | STRH, _ -> assert false (* XXX TODO *) | LDAR, _ ->
     assert false (* XXX TODO *) | STR_simd_and_fp, _ -> assert false (* XXX
     TODO *) | LDRSW, _ -> assert false (* XXX TODO *) | ABS_vector, _ -> assert
     false (* XXX TODO *) | ADDP_vector, _ -> assert false (* XXX TODO *) |
     ADDV, _ -> assert false (* XXX TODO *) | MVN_vector, _ -> assert false (*
     XXX TODO *) | NEG_vector, _ -> assert false (* XXX TODO *) | MOV_vector, _
     -> assert false (* XXX TODO *) | MULL_vector, _ -> assert false (* XXX TODO
     *) | UMULL_vector, _ -> assert false (* XXX TODO *) | SMULL_vector, _ ->
     assert false (* XXX TODO *) | UMULL2_vector, _ -> assert false (* XXX TODO
     *) | SMULL2_vector, _ -> assert false (* XXX TODO *) | FADDP_vector, _ ->
     assert false (* XXX TODO *) | FNEG_vector, _ -> assert false (* XXX TODO *)
     | FSQRT_vector, _ -> assert false (* XXX TODO *) | FCVTZS_vector, _ ->
     assert false (* XXX TODO *) | FCVTNS_vector, _ -> assert false (* XXX TODO
     *) | SCVTF_vector, _ -> assert false (* XXX TODO *) | FCVTN_vector, _ ->
     assert false (* XXX TODO *) | FCVTL_vector, _ -> assert false (* XXX TODO
     *) | FMOV_vector_immediate, _ -> assert false (* XXX TODO *) |
     FRECPE_vector, _ -> assert false (* XXX TODO *) | FRSQRTE_vector, _ ->
     assert false (* XXX TODO *) | FRINT _, _ -> assert false (* XXX TODO *) |
     FRINT_vector _, _ -> assert false (* XXX TODO *) | FCM_register _, _ ->
     assert false (* XXX TODO *) | FCM_zero _, _ -> assert false (* XXX TODO *)
     | CM_register _, _ -> assert false (* XXX TODO *) | CM_zero _, _ -> assert
     false (* XXX TODO *) | LDR_simd_and_fp, _ -> assert false (* XXX TODO *) |
     CVT_vector, _ -> assert false (* XXX TODO *) | CNT_vector, _ -> assert
     false (* XXX TODO *) | SQXTN, _ -> assert false (* XXX TODO *) | SQXTN2, _
     -> assert false (* XXX TODO *) | UQXTN, _ -> assert false (* XXX TODO *) |
     UQXTN2, _ -> assert false (* XXX TODO *) | XTN, _ -> assert false (* XXX
     TODO *) | XTN2, _ -> assert false (* XXX TODO *) | SXTL, _ -> assert false
     (* XXX TODO *) | UXTL, _ -> assert false (* XXX TODO *) | SHL, _ -> assert
     false (* XXX TODO *) | SSHR, _ -> assert false (* XXX TODO *) | USHR, _ ->
     assert false (* XXX TODO *) | DUP, _ -> assert false (* XXX TODO *) | INS,
     _ -> assert false (* XXX TODO *) | INS_V, _ -> assert false (* XXX TODO *)
     | UMOV, _ -> assert false (* XXX TODO *) | SMOV, _ -> assert false (* XXX
     TODO *) | EXT, _ -> assert false (* XXX TODO *) | ZIP1, _ -> assert false
     (* XXX TODO *) | ZIP2, _ -> assert false (* XXX TODO *) | UADDLP_vector, _
     -> assert false (* XXX TODO *) | ADDS, _ -> assert false (* XXX TODO *) |
     CSEL, _ -> assert false (* XXX TODO *) | CSINC, _ -> assert false (* XXX
     TODO *) | TST, _ -> assert false (* XXX TODO *) | B, _ -> assert false (*
     XXX TODO *) | BL, _ -> assert false (* XXX TODO *) | BR, _ -> assert false
     (* XXX TODO *) | BLR, _ -> assert false (* XXX TODO *) | B_cond _, _ ->
     assert false (* XXX TODO *) | B_cond_float _, _ -> assert false (* XXX TODO
     *) | CBZ, _ -> assert false (* XXX TODO *) | CBNZ, _ -> assert false (* XXX
     TODO *) | TBZ, _ -> assert false (* XXX TODO *) | TBNZ, _ -> assert false
     (* XXX TODO *) | RET, _ -> assert false (* XXX TODO *) | NOP, _ -> assert
     false (* XXX TODO *) | YIELD, _ -> assert false (* XXX TODO *) | DMB _, _
     -> assert false (* XXX TODO *) | DSB _, _ -> assert false (* XXX TODO *) |
     MOV, _ -> assert false (* XXX TODO *) | MOVI, _ -> assert false (* XXX TODO
     *) | CTZ, _ -> assert false *)
  (* XXX TODO *)

  (* let foo : type operands. operands -> operands Instruction_name.t -> int =
     fun operands insn -> match[@ocaml.warning "-4"] insn, operands with |
     SMAX_vector, operands -> ( match insn, operands with | ( SMAX_vector, ( Reg
     { reg_name = GP _ | Neon (Vector (V1D | V2D) | Scalar _ | Lane _); _ }, _,
     _ ) ) -> .) | _ -> 42 *)
end
