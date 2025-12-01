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
     | (V4H | V8H), H -> Equal_H | S, (V2S | V4S) -> Equal_S | D, (V1D | V2D) ->
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

  module Lane_index : sig
    type t = private int

    type lane_index = t

    val create : int -> t

    val to_int : t -> int

    module Src_and_dest : sig
      type t

      val create : src:lane_index -> dest:lane_index -> t

      val dest_index : t -> lane_index

      val src_index : t -> lane_index
    end
  end = struct
    type t = int

    type lane_index = t

    let create i = i

    let to_int i = i

    module Src_and_dest = struct
      type nonrec t =
        { src : t;
          dest : t
        }

      let create ~src ~dest = { src; dest }

      let dest_index t = t.dest

      let src_index t = t.src
    end
  end

  module Lane = struct
    (** Support representation with and without the optional number of lanes, for
        example Vn.4S[1] and Vn.S[1]. *)
    type 'a r =
      | V : ('v, 's) Vector.t -> [`Vector of 'v * 's] r
      | S : 's Scalar.t -> [`Scalar of 's] r

    type 'a t =
      { r : 'a r;
        lane : Lane_index.t
      }

    let num_lanes (type a) (r : a r) =
      match r with V v -> Vector.num_lanes v | S s -> Scalar.num_lanes s

    let check_index t =
      let last = num_lanes t.r - 1 in
      check_index 0 last (Lane_index.to_int t.lane)

    let name (type a) (t : a t) index =
      let suffix =
        match t.r with V v -> Vector.to_string v | S s -> Scalar.to_string s
      in
      Printf.sprintf "V%d.%s[%d]" index suffix (Lane_index.to_int t.lane)
  end

  type _ t =
    | Vector : ('v, 's) Vector.t -> [`Vector of 'v * 's] t
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
    | Optional : 'a t option -> [`Optional of 'a option] t
  [@@ocaml.warning "-37"]

  type 'a operand = 'a t

  let rec print : type a. Format.formatter -> a t -> unit =
   fun ppf t ->
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
    | Optional opt -> ( match opt with Some op -> print ppf op | None -> ())

  module Wrapped = struct
    type t = O : _ operand -> t

    let create (type a) (o : a operand) : t = O o

    let print ppf (O t) = print ppf t
  end
end

type singleton = [`Singleton]

type pair = [`Pair]

type triple = [`Triple]

type quad = [`Quad]

type (_, _) many =
  | Singleton : 'a Operand.t -> (singleton, 'a) many
  | Pair : 'a Operand.t * 'b Operand.t -> (pair, 'a * 'b) many
  | Triple :
      'a Operand.t * 'b Operand.t * 'c Operand.t
      -> (triple, 'a * 'b * 'c) many
  | Quad :
      'a Operand.t * 'b Operand.t * 'c Operand.t * 'd Operand.t
      -> (quad, 'a * 'b * 'c * 'd) many

module Instruction_name = struct
  type (_, _) t =
    | ABS_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | ADDP_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | ADDS
        : ( quad,
            [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Reg of [< `GP of [< `X | `SP]]]
            * [< `Imm of [< `Twelve]]
            * [< `Optional of [< `Fixed_shift of [< `Lsl_by_twelve]] option] )
          t
    | ADDV
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `B]]]]
            * [< `Reg of [< `Neon of [< `Vector of _]]] )
          t
    | ADD_immediate
        : ( quad,
            [< `Reg of [< `GP of [< `X | `SP | `FP]]]
            * [< `Reg of [< `GP of [< `X | `SP | `FP]]]
            * [< `Imm of [< `Twelve]]
            * [< `Optional of [< `Fixed_shift of [< `Lsl_by_twelve]] option] )
          t
    | ADD_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | ADD_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | ADR
        : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Imm of [< `Twenty_one]]) t
    | ADRP : (pair, [< `Reg of [< `GP of [< `X]]] * _) t
    | AND_immediate
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Bitmask] )
          t
    | AND_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | AND_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | ASRV
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | B : (singleton, [< `Imm of _]) t
    | BL : (singleton, [< `Imm of _]) t
    | BLR : (singleton, [< `Reg of [< `GP of [< `X]]]) t
    | BR : (singleton, [< `Reg of [< `GP of [< `X]]]) t
    | B_cond : Cond.t -> (singleton, [< `Imm of _]) t
    | B_cond_float : Float_cond.t -> (singleton, [< `Imm of _]) t
    | CBNZ : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Imm of _]) t
    | CBZ : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Imm of _]) t
    | CLZ
        : ( pair,
            [< `Reg of [< `GP of [< `X]]] * [< `Reg of [< `GP of [< `X]]] )
          t
    | CM_register :
        Cond.t
        -> ( triple,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | CM_zero :
        Cond.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | CNT
        : ( pair,
            [< `Reg of [< `GP of [< `X]]] * [< `Reg of [< `GP of [< `X]]] )
          t
    | CNT_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of _]]]
            * [< `Reg of [< `Neon of [< `Vector of _]]] )
          t
    | CSEL
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Cond] )
          t
    | CSINC
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Cond] )
          t
    | CTZ
        : ( pair,
            [< `Reg of [< `GP of [< `X]]] * [< `Reg of [< `GP of [< `X]]] )
          t
    | CVT_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | DMB : Memory_barrier.t -> (singleton, unit) t
    | DSB : Memory_barrier.t -> (singleton, unit) t
    | DUP :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | EOR_immediate
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Bitmask] )
          t
    | EOR_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | EOR_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | EXT
        : ( quad,
            [< `Reg of [< `Neon of [< `Vector of [< `V16B]]]]
            * [< `Reg of [< `Neon of [< `Vector of [< `V16B]]]]
            * [< `Reg of [< `Neon of [< `Vector of [< `V16B]]]]
            * [< `Imm of [< `Six]] )
          t
    | FABS
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FADD
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FADDP_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FADD_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FCMP
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCM_register :
        Float_cond.t
        -> ( triple,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | FCM_zero :
        Float_cond.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | FCSEL
        : ( quad,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Cond] )
          t
    | FCVT
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTL_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FCVTNS
        : ( pair,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTNS_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FCVTN_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    (* Binary vector operations with min/max *)
    | FCVTZS
        : ( pair,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FCVTZS_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FDIV
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FDIV_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FMADD
        : ( quad,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FMAX
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FMAX_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FMIN
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FMIN_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FMOV_general_or_register
        : ( pair,
            [< `Reg of
               [< `Neon of [< `Scalar of [< `S | `D]] | `GP of [< `X | `W]] ]
            * [< `Reg of
                 [< `Neon of [< `Scalar of [< `S | `D]]
                 | `GP of [< `X | `XZR | `W | `WZR] ] ] )
          t
    | FMOV_scalar_immediate
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Imm of [< `Sixty_four]] )
          t
    | FMOV_vector_immediate
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Imm of [< `Sixty_four]] )
          t
    | FMSUB
        : ( quad,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FMUL
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FMUL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FNEG
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FNEG_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FNMADD
        : ( quad,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FNMSUB
        : ( quad,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FNMUL
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FRECPE_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FRINT :
        Rounding_mode.t
        -> ( pair,
             [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
             * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
           t
    | FRINT_vector :
        Rounding_mode.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | FRSQRTE_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FSQRT
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FSQRT_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | FSUB
        : ( triple,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]] )
          t
    | FSUB_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | INS :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]
               ] )
           t
    | INS_V :
        Neon_reg_name.Lane_index.Src_and_dest.t
        -> ( pair,
             [< `Reg of
                [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | LDAR : (pair, [< `Reg of [< `GP of [< `X | `W]]] * [< `Mem]) t
    | LDP
        : ( triple,
            [< `Reg of [< `GP of [< `X | `W | `LR]]]
            * [< `Reg of [< `GP of [< `X | `W | `LR]]]
            * [< `Mem] )
          t
    | LDR : (pair, [< `Reg of [< `GP of [< `X | `W | `LR]]] * [< `Mem]) t
    | LDRB : (pair, [< `Reg of [< `GP of [< `W]]] * [< `Mem]) t
    | LDRH : (pair, [< `Reg of [< `GP of [< `W]]] * [< `Mem]) t
    | LDRSB : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Mem]) t
    | LDRSH : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Mem]) t
    | LDRSW : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Mem]) t
    | LDR_simd_and_fp
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `D | `S | `Q]]]] * [< `Mem]
          )
          t
    | LSLV
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | LSRV
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | MADD
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X | `XZR]]] )
          t
    | MOV
        : ( pair,
            [< `Reg of
               [< `GP of
                  [< `X | `W]
                  (* | `Neon of [< `Scalar of _ | `Vector of
                     Neon_reg_name.Vector.t] *) ] ]
            * [< `Reg of
                 [< `GP of
                    [< `X | `W | `XZR | `WZR]
                    (* | `Neon of [< `Scalar of _ | `Vector of
                       Neon_reg_name.Vector.t] *) ]
              | `Imm of _ ] )
          t
    | MOVI
        : ( pair,
            [< `Reg of
               [< `Neon of
                  [< `Scalar of _ | `Vector of [< any_vector] * [< any_width]]
               ] ]
            * [< `Imm of [< `Twelve]] )
          t
    | MOVK
        : ( triple,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Imm of [< `Sixty_four]]
            * [< `Shift of [< `Lsl] * [< `Six]] )
          t
    | MOVN
        : ( triple,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Imm of [< `Twelve | `Sixty_four]]
            * [< `Optional of [< `Shift of [< `Lsl] * [< `Six]] option] )
          t
    | MOVZ
        : ( triple,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Imm of [< `Sixty_four]]
            * [< `Optional of [< `Shift of [< `Lsl] * [< `Six]] option] )
          t
    | MOV_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of _]]]
            * [< `Reg of [< `Neon of [< `Vector of _]]] )
          t
    | MSUB
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | MULL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | MUL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | MVN_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | NEG_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | NOP : (singleton, unit) t
    | ORR_immediate
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Bitmask] )
          t
    | ORR_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | ORR_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | RBIT
        : ( pair,
            [< `Reg of [< `GP of [< `X]]] * [< `Reg of [< `GP of [< `X]]] )
          t
    | RET : (singleton, unit) t
    | REV
        : ( pair,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Reg of [< `GP of [< `X | `W]]] )
          t
    | REV16
        : ( pair,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Reg of [< `GP of [< `X | `W]]] )
          t
    | SBFM
        : ( quad,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Imm of [< `Six]]
            * [< `Imm of [< `Six]] )
          t
    | SCVTF
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `S | `D]]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | SCVTF_vector
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SDIV
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `Neon of [< `Vector of [< `V8B] * [< `B]]]
              | `GP of [< `X] ] )
          t
    | SHL
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Imm of [< `Six]] )
          t
    | SMAX_vector
        : ( triple,
            [< `Reg of
               [< `Neon of
                  [< `Vector of
                     [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                  ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ] )
          t
    | SMIN_vector
        : ( triple,
            [< `Reg of
               [< `Neon of
                  [< `Vector of
                     [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                  ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ] )
          t
    | SMOV :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | SMULH
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | SMULL2_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SMULL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SQADD_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SQSUB_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SQXTN
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SQXTN2
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SSHL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SSHR
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Imm of [< `Six]] )
          t
    | STP
        : ( triple,
            [< `Reg of [< `GP of [< `X | `W | `LR]]]
            * [< `Reg of [< `GP of [< `X | `W | `LR]]]
            * [< `Mem] )
          t
    | STR : (pair, [< `Reg of [< `GP of [< `X | `W | `LR]]] * [< `Mem]) t
    | STRB : (pair, [< `Reg of [< `GP of [< `W]]] * [< `Mem]) t
    | STRH : (pair, [< `Reg of [< `GP of [< `W]]] * [< `Mem]) t
    | STR_simd_and_fp
        : ( pair,
            [< `Reg of [< `Neon of [< `Scalar of [< `D | `S | `Q]]]] * [< `Mem]
          )
          t
    | SUBS_immediate
        : ( quad,
            [< `Reg of [< `GP of [< `W | `WZR | `X | `XZR]]]
            * [< `Reg of [< `GP of [< `W | `X | `SP]]]
            * [< `Imm of [< `Twelve]]
            * [< `Optional of [< `Fixed_shift of [< `Lsl_by_twelve]] option] )
          t
    | SUBS_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X | `XZR]]]
            * [< `Reg of [< `GP of [< `X | `SP]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | SUB_immediate
        : ( quad,
            [< `Reg of [< `GP of [< `X | `SP]]]
            * [< `Reg of [< `GP of [< `X | `SP]]]
            * [< `Imm of [< `Twelve]]
            * [< `Optional of [< `Fixed_shift of [< `Lsl_by_twelve]] option] )
          t
    | SUB_shifted_register
        : ( quad,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Optional of
                 [< `Shift of [< `Lsl | `Lsr | `Asr] * [< `Six]] option ] )
          t
    | SUB_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | SXTL
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | TBNZ
        : ( triple,
            [< `Reg of [< `GP of [< `X]]] * [< `Imm of [< `Six]] * [< `Imm of _]
          )
          t
    | TBZ
        : ( triple,
            [< `Reg of [< `GP of [< `X]]] * [< `Imm of [< `Six]] * [< `Imm of _]
          )
          t
    | TST : (pair, [< `Reg of [< `GP of [< `X]]] * [< `Bitmask]) t
    | UADDLP_vector
        : ( pair,
            [< `Reg of
               [< `Neon of
                  [< `Vector of
                     [< `V4H | `V8H | `V2S | `V4S | `V1D | `V2D] * [< any_width]
                  ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ] )
          t
    | UBFM
        : ( quad,
            [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Reg of [< `GP of [< `X | `W]]]
            * [< `Imm of [< `Six]]
            * [< `Imm of [< `Six]] )
          t
    | UMAX_vector
        : ( triple,
            [< `Reg of
               [< `Neon of
                  [< `Vector of
                     [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                  ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ] )
          t
    | UMIN_vector
        : ( triple,
            [< `Reg of
               [< `Neon of
                  [< `Vector of
                     [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S] * [< any_width]
                  ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ]
            * [< `Reg of
                 [< `Neon of
                    [< `Vector of
                       [< `V8B | `V16B | `V4H | `V8H | `V2S | `V4S]
                       * [< any_width] ] ] ] )
          t
    | UMOV :
        Neon_reg_name.Lane_index.t
        -> ( pair,
             [< `Reg of [< `GP of [< `W | `X] | `Neon of [< `Scalar of [< `D]]]]
             * [< `Reg of
                  [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
           )
           t
    | UMULH
        : ( triple,
            [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]]
            * [< `Reg of [< `GP of [< `X]]] )
          t
    | UMULL2_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | UMULL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | UQADD_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | UQSUB_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    (* Lane-indexed operations *)
    | UQXTN
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | UQXTN2
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | USHL_vector
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | USHR
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Imm of [< `Six]] )
          t
    | UXTL
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | XTN
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | XTN2
        : ( pair,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | YIELD : (singleton, unit) t
    | ZIP1
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t
    | ZIP2
        : ( triple,
            [< `Reg of [< `Neon of [< `Vector of [< any_vector] * [< any_width]]]
            ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
            * [< `Reg of
                 [< `Neon of [< `Vector of [< any_vector] * [< any_width]]] ]
          )
          t

  type ('num_operands, 'operands) instr = ('num_operands, 'operands) t

  module Wrapped = struct
    type t = I : (_, _) instr -> t

    let to_string t =
      match t with
      | I instr -> (
        match instr with
        | ABS_vector -> "abs"
        | ADD_immediate | ADD_shifted_register | ADD_vector -> "add"
        | ADDP_vector -> "addp"
        | ADDS -> "adds"
        | ADDV -> "addv"
        | ADR -> "adr"
        | ADRP -> "adrp"
        | AND_immediate | AND_shifted_register | AND_vector -> "and"
        | ASRV -> "asrv"
        | B -> "b"
        | B_cond c -> "b." ^ Cond.to_string c
        | B_cond_float c -> "b." ^ Float_cond.to_string c
        | BL -> "bl"
        | BLR -> "blr"
        | BR -> "br"
        | CBNZ -> "cbnz"
        | CBZ -> "cbz"
        | CLZ -> "clz"
        | CM_register cond -> "cm" ^ Cond.to_string cond
        | CM_zero cond -> "cm" ^ Cond.to_string cond
        | CNT -> "cnt"
        | CNT_vector -> "cnt"
        | CSEL -> "csel"
        | CSINC -> "csinc"
        | CTZ -> "ctz"
        | CVT_vector -> "cvt"
        | DMB b -> "dmb\t" ^ Memory_barrier.to_string b
        | DSB b -> "dsb\t" ^ Memory_barrier.to_string b
        | DUP _ -> "dup"
        | EOR_immediate | EOR_shifted_register | EOR_vector -> "eor"
        | EXT -> "ext"
        | FABS -> "fabs"
        | FADD -> "fadd"
        | FADD_vector -> "fadd"
        | FADDP_vector -> "faddp"
        | FCM_register cond -> "fcm" ^ Float_cond.to_string cond
        | FCM_zero cond -> "fcm" ^ Float_cond.to_string cond
        | FCMP -> "fcmp"
        | FCSEL -> "fcsel"
        | FCVT -> "fcvt"
        | FCVTL_vector -> "fcvtl"
        | FCVTN_vector -> "fcvtn"
        | FCVTNS -> "fcvtns"
        | FCVTNS_vector -> "fcvtns"
        | FCVTZS -> "fcvtzs"
        | FCVTZS_vector -> "fcvtzs"
        | FDIV -> "fdiv"
        | FDIV_vector -> "fdiv"
        | FMADD -> "fmadd"
        | FMAX -> "fmax"
        | FMAX_vector -> "fmax"
        | FMIN -> "fmin"
        | FMIN_vector -> "fmin"
        | FMOV_general_or_register | FMOV_scalar_immediate
        | FMOV_vector_immediate ->
          "fmov"
        | FMSUB -> "fmsub"
        | FMUL -> "fmul"
        | FMUL_vector -> "fmul"
        | FNEG -> "fneg"
        | FNEG_vector -> "fneg"
        | FNMADD -> "fnmadd"
        | FNMSUB -> "fnmsub"
        | FNMUL -> "fnmul"
        | FRECPE_vector -> "frecpe"
        | FRINT rm -> "frint" ^ Rounding_mode.to_string rm
        | FRINT_vector rm -> "frint" ^ Rounding_mode.to_string rm
        | FRSQRTE_vector -> "frsqrte"
        | FSQRT -> "fsqrt"
        | FSQRT_vector -> "fsqrt"
        | FSUB -> "fsub"
        | FSUB_vector -> "fsub"
        | INS _ -> "ins"
        | INS_V _ -> "ins"
        | LDAR -> "ldar"
        | LDP -> "ldp"
        | LDR -> "ldr"
        | LDR_simd_and_fp -> "ldr"
        | LDRB -> "ldrb"
        | LDRH -> "ldrh"
        | LDRSB -> "ldrsb"
        | LDRSH -> "ldrsh"
        | LDRSW -> "ldrsw"
        | LSLV -> "lslv"
        | LSRV -> "lsrv"
        | MADD -> "madd"
        | MOV -> "mov"
        | MOV_vector -> "mov"
        | MOVI -> "movi"
        | MOVK -> "movk"
        | MOVN -> "movn"
        | MOVZ -> "movz"
        | MSUB -> "msub"
        | MUL_vector -> "mul"
        | MULL_vector -> "mull"
        | MVN_vector -> "mvn"
        | NEG_vector -> "neg"
        | NOP -> "nop"
        | ORR_immediate | ORR_shifted_register | ORR_vector -> "orr"
        | RBIT -> "rbit"
        | RET -> "ret"
        | REV -> "rev"
        | REV16 -> "rev16"
        | SBFM -> "sbfm"
        | SCVTF -> "scvtf"
        | SCVTF_vector -> "scvtf"
        | SDIV -> "sdiv"
        | SHL -> "shl"
        | SMAX_vector -> "smax"
        | SMIN_vector -> "smin"
        | SMOV _ -> "smov"
        | SMULH -> "smulh"
        | SMULL2_vector -> "smull2"
        | SMULL_vector -> "smull"
        | SQADD_vector -> "sqadd"
        | SQSUB_vector -> "sqsub"
        | SQXTN -> "sqxtn"
        | SQXTN2 -> "sqxtn2"
        | SSHL_vector -> "sshl" (* XXX is this right? *)
        | SSHR -> "sshr"
        | STP -> "stp"
        | STR -> "str"
        | STR_simd_and_fp -> "str"
        | STRB -> "strb"
        | STRH -> "strh"
        | SUB_immediate | SUB_shifted_register | SUB_vector -> "sub"
        | SUBS_immediate | SUBS_shifted_register -> "subs"
        | SXTL -> "sxtl"
        | TBNZ -> "tbnz"
        | TBZ -> "tbz"
        | TST -> "tst"
        | UADDLP_vector -> "uaddlp"
        | UBFM -> "ubfm"
        | UMAX_vector -> "umax"
        | UMIN_vector -> "umin"
        | UMOV _ -> "umov"
        | UMULH -> "umulh"
        | UMULL2_vector -> "umull2"
        | UMULL_vector -> "umull"
        | UQADD_vector -> "uqadd"
        | UQSUB_vector -> "uqsub"
        | UQXTN -> "uqxtn"
        | UQXTN2 -> "uqxtn2"
        | USHL_vector -> "ushl"
        | USHR -> "ushr"
        | UXTL -> "uxtl"
        | XTN -> "xtn"
        | XTN2 -> "xtn2"
        | YIELD -> "yield"
        | ZIP1 -> "zip1"
        | ZIP2 -> "zip2")
  end

  module Untyped = struct
    let operands_as_array (type num operands) (instr : (num, operands) t)
        (ops : (num, operands) many) =
      let o = Operand.Wrapped.create in
      let imm n = Operand.Imm (Operand.Imm.Twelve n) (* XXX duplicate *) in
      (* Helper to convert a vector register operand to a lane-indexed scalar *)
      let vector_to_lane_operand (type a) (reg_op : a Operand.t)
          (lane : Neon_reg_name.Lane_index.t) =
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
        | Shift _ | Cond _ | Float_cond _ | Mem _ | Bitmask _ | Optional _ ->
          failwith "vector_to_lane_operand: not a register operand"
      in
      match instr with
      | ABS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | ADDP_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ADDS -> (
        let (Quad (rd, rn, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o imm |]
        | Optional (Some shift) -> [| o rd; o rn; o imm; o shift |])
      | ADDV ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | ADD_immediate -> (
        let (Quad (rd, rs, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o imm |]
        | Optional (Some shift) -> [| o rd; o rs; o imm; o shift |])
      | ADD_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | ADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ADR ->
        let (Pair (rd, label)) = ops in
        [| o rd; o label |]
      | ADRP ->
        let (Pair (rd, symbol)) = ops in
        [| o rd; o symbol |]
      | AND_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | AND_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | AND_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ASRV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | B ->
        let (Singleton target) = ops in
        [| o target |]
      | BL ->
        let (Singleton target) = ops in
        [| o target |]
      | BLR ->
        let (Singleton rn) = ops in
        [| o rn |]
      | BR ->
        let (Singleton rn) = ops in
        [| o rn |]
      | B_cond _ ->
        let (Singleton target) = ops in
        [| o target |]
      | B_cond_float _ ->
        let (Singleton target) = ops in
        [| o target |]
      | CBNZ ->
        let (Pair (reg, target)) = ops in
        [| o reg; o target |]
      | CBZ ->
        let (Pair (reg, target)) = ops in
        [| o reg; o target |]
      | CLZ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | CM_register _cond ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | CM_zero _cond ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn; o (imm 0) |]
      | CNT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | CNT_vector ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | CSEL ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | CSINC ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | CTZ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | CVT_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | DMB _ -> [||]
      | DSB _ -> [||]
      | DUP lane ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | EOR_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | EOR_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | EOR_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | EXT ->
        let (Quad (rd, rs1, rs2, idx)) = ops in
        [| o rd; o rs1; o rs2; o idx |]
      | FABS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FADD ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FADDP_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FCMP ->
        let (Pair (rn, rm)) = ops in
        [| o rn; o rm |]
      | FCM_register _cond ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FCM_zero _cond ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn; o (Operand.Imm_float 0.) |]
      | FCSEL ->
        let (Quad (rd, rn, rm, cond)) = ops in
        [| o rd; o rn; o rm; o cond |]
      | FCVT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTL_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTNS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTNS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTN_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FCVTZS ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FCVTZS_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FDIV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FDIV_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FMAX ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMIN ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FMOV_general_or_register ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FMOV_scalar_immediate ->
        let (Pair (rd, imm)) = ops in
        [| o rd; o imm |]
      | FMOV_vector_immediate ->
        let (Pair (rd, imm)) = ops in
        [| o rd; o imm |]
      | FMSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FMUL ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FMUL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | FNEG ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FNEG_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FNMADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FNMSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | FNMUL ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FRECPE_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FRINT _ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FRINT_vector _ ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FRSQRTE_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FSQRT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | FSQRT_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | FSUB ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | FSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | INS lane ->
        let (Pair (rd, rs)) = ops in
        [| vector_to_lane_operand rd lane; o rs |]
      | INS_V lanes ->
        let (Pair (rd, rs)) = ops in
        [| vector_to_lane_operand rd
             (Neon_reg_name.Lane_index.Src_and_dest.dest_index lanes);
           vector_to_lane_operand rs
             (Neon_reg_name.Lane_index.Src_and_dest.src_index lanes)
        |]
      | LDAR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDP ->
        let (Triple (rt1, rt2, addr)) = ops in
        [| o rt1; o rt2; o addr |]
      | LDR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDRSW ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LDR_simd_and_fp ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | LSLV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | LSRV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | MADD ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | MOV ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | MOVI ->
        let (Pair (rd, imm)) = ops in
        [| o rd; o imm |]
      | MOVK ->
        let (Triple (rd, imm, shift)) = ops in
        [| o rd; o imm; o shift |]
      | MOVN -> (
        let (Triple (rd, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o imm |]
        | Optional (Some shift) -> [| o rd; o imm; o shift |])
      | MOVZ -> (
        let (Triple (rd, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o imm |]
        | Optional (Some shift) -> [| o rd; o imm; o shift |])
      | MOV_vector ->
        let (Pair (rd, src)) = ops in
        [| o rd; o src |]
      | MSUB ->
        let (Quad (rd, rn, rm, ra)) = ops in
        [| o rd; o rn; o rm; o ra |]
      | MULL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | MUL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | MVN_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | NEG_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | NOP -> [||]
      | ORR_immediate ->
        let (Triple (rd, rs, bitmask)) = ops in
        [| o rd; o rs; o bitmask |]
      | ORR_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | ORR_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | RBIT ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | RET -> [||]
      | REV ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | REV16 ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | SBFM ->
        let (Quad (rd, rn, immr, imms)) = ops in
        [| o rd; o rn; o immr; o imms |]
      | SCVTF ->
        let (Pair (rd, rn)) = ops in
        [| o rd; o rn |]
      | SCVTF_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SDIV ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | SHL ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | SMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMOV lane ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | SMULH ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | SMULL2_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SMULL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SQXTN ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SQXTN2 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | SSHL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SSHR ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | STP ->
        let (Triple (rt1, rt2, addr)) = ops in
        [| o rt1; o rt2; o addr |]
      | STR ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STRB ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STRH ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | STR_simd_and_fp ->
        let (Pair (rt, addr)) = ops in
        [| o rt; o addr |]
      | SUBS_immediate -> (
        let (Quad (rd, rn, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o imm |]
        | Optional (Some shift) -> [| o rd; o rn; o imm; o shift |])
      | SUBS_shifted_register -> (
        let (Quad (rd, rn, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rn; o reg |]
        | Optional (Some shift) -> [| o rd; o rn; o reg; o shift |])
      | SUB_immediate -> (
        let (Quad (rd, rs, imm, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o imm |]
        | Optional (Some shift) -> [| o rd; o rs; o imm; o shift |])
      | SUB_shifted_register -> (
        let (Quad (rd, rs, reg, shift_opt)) = ops in
        match shift_opt with
        | Optional None -> [| o rd; o rs; o reg |]
        | Optional (Some shift) -> [| o rd; o rs; o reg; o shift |])
      | SUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | SXTL ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | TBNZ ->
        let (Triple (reg, bit, target)) = ops in
        [| o reg; o bit; o target |]
      | TBZ ->
        let (Triple (reg, bit, target)) = ops in
        [| o reg; o bit; o target |]
      | TST ->
        let (Pair (rn, op2)) = ops in
        [| o rn; o op2 |]
      | UADDLP_vector ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | UBFM ->
        let (Quad (rd, rn, immr, imms)) = ops in
        [| o rd; o rn; o immr; o imms |]
      | UMAX_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMIN_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMOV lane ->
        let (Pair (rd, rs)) = ops in
        [| o rd; vector_to_lane_operand rs lane |]
      | UMULH ->
        let (Triple (rd, rn, rm)) = ops in
        [| o rd; o rn; o rm |]
      | UMULL2_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UMULL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQADD_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQSUB_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | UQXTN ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | UQXTN2 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | USHL_vector ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | USHR ->
        let (Triple (rd, rs, imm)) = ops in
        [| o rd; o rs; o imm |]
      | UXTL ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | XTN ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | XTN2 ->
        let (Pair (rd, rs)) = ops in
        [| o rd; o rs |]
      | YIELD -> [||]
      | ZIP1 ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
      | ZIP2 ->
        let (Triple (rd, rs1, rs2)) = ops in
        [| o rd; o rs1; o rs2 |]
  end
end

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

  let encode_instruction :
      type num operands.
      (num, operands) Instruction_name.t -> (num, operands) many -> int32 =
   fun instr operands ->
    match operands, instr with
    (* Singleton unit operations *)
    | _, DMB _ -> assert false
    | _, DSB _ -> assert false
    | _, NOP -> assert false
    | _, RET -> assert false
    | _, YIELD -> assert false
    (* Singleton immediate/register operations *)
    | Singleton (Imm _), B -> assert false
    | Singleton (Sym _), B -> assert false
    | Singleton (Imm_float _), B -> assert false
    | Singleton (Imm_nativeint _), B -> assert false
    | Singleton (Imm _), BL -> assert false
    | Singleton (Sym _), BL -> assert false
    | Singleton (Imm_float _), BL -> assert false
    | Singleton (Imm_nativeint _), BL -> assert false
    | Singleton (Imm _), B_cond _ -> assert false
    | Singleton (Sym _), B_cond _ -> assert false
    | Singleton (Imm_float _), B_cond _ -> assert false
    | Singleton (Imm_nativeint _), B_cond _ -> assert false
    | Singleton (Imm _), B_cond_float _ -> assert false
    | Singleton (Sym _), B_cond_float _ -> assert false
    | Singleton (Imm_float _), B_cond_float _ -> assert false
    | Singleton (Imm_nativeint _), B_cond_float _ -> assert false
    | Singleton (Reg _), BLR -> assert false
    | Singleton (Reg _), BR -> assert false
    (* Pair operations *)
    | Pair (Reg _rd, Reg _rn), ABS_vector -> assert false
    | Pair (Reg _rd, _), ADR -> assert false
    | Pair (Reg _rd, _), ADRP -> assert false
    | Pair (Reg _rd, Reg _rn), ADDV -> assert false
    | Pair (Reg _rd, Imm _), CBNZ -> assert false
    | Pair (Reg _rd, Sym _), CBNZ -> assert false
    | Pair (Reg _rd, Imm_float _), CBNZ -> assert false
    | Pair (Reg _rd, Imm_nativeint _), CBNZ -> assert false
    | Pair (Reg _rd, Imm _), CBZ -> assert false
    | Pair (Reg _rd, Sym _), CBZ -> assert false
    | Pair (Reg _rd, Imm_float _), CBZ -> assert false
    | Pair (Reg _rd, Imm_nativeint _), CBZ -> assert false
    | Pair (Reg _rd, Reg _rn), CLZ -> assert false
    | Pair (Reg _rd, Reg _rn), CM_zero _ -> assert false
    | Pair (Reg _rd, Reg _rn), CNT -> assert false
    | Pair (Reg _rd, Reg _rn), CNT_vector -> assert false
    | Pair (Reg _rd, Reg _rn), CTZ -> assert false
    | Pair (Reg _rd, Reg _rn), CVT_vector -> assert false
    | Pair (Reg _rd, Reg _rn), DUP _ -> assert false
    | Pair (Reg _rd, Reg _rn), FABS -> assert false
    | Pair (Reg _rd, Reg _rn), FCMP -> assert false
    | Pair (Reg _rd, Reg _rn), FCM_zero _ -> assert false
    | Pair (Reg _rd, Reg _rn), FCVT -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTL_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTNS -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTNS_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTN_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTZS -> assert false
    | Pair (Reg _rd, Reg _rn), FCVTZS_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FMOV_general_or_register -> assert false
    | Pair (Reg _rd, _), FMOV_scalar_immediate -> assert false
    | Pair (Reg _rd, _), FMOV_vector_immediate -> assert false
    | Pair (Reg _rd, Reg _rn), FNEG -> assert false
    | Pair (Reg _rd, Reg _rn), FNEG_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FRECPE_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FRINT _ -> assert false
    | Pair (Reg _rd, Reg _rn), FRINT_vector _ -> assert false
    | Pair (Reg _rd, Reg _rn), FRSQRTE_vector -> assert false
    | Pair (Reg _rd, Reg _rn), FSQRT -> assert false
    | Pair (Reg _rd, Reg _rn), FSQRT_vector -> assert false
    | Pair (Reg _rd, Reg _rn), INS _ -> assert false
    | Pair (Reg _rd, Reg _rn), INS_V _ -> assert false
    | Pair (Reg _rd, Mem _addressing), LDAR -> assert false
    | Pair (Reg _rd, Mem _addressing), LDR -> assert false
    | Pair (Reg _rd, Mem _addressing), LDRB -> assert false
    | Pair (Reg _rd, Mem _addressing), LDRH -> assert false
    | Pair (Reg _rd, Mem _addressing), LDRSB -> assert false
    | Pair (Reg _rd, Mem _addressing), LDRSH -> assert false
    | Pair (Reg _rd, Mem _addressing), LDRSW -> assert false
    | Pair (Reg _rd, Mem _addressing), LDR_simd_and_fp -> assert false
    | Pair (Reg _rd, Reg _rn), MOV -> assert false
    | Pair (Reg _rd, Imm _), MOV -> assert false
    | Pair (Reg _rd, Sym _), MOV -> assert false
    | Pair (Reg _rd, Imm_float _), MOV -> assert false
    | Pair (Reg _rd, Imm_nativeint _), MOV -> assert false
    | Pair (Reg _rd, Imm _), MOVI -> assert false
    | Pair (Reg _rd, Sym _), MOVI -> assert false
    | Pair (Reg _rd, Reg _rn), MOV_vector -> assert false
    | Pair (Reg _rd, Reg _rn), MVN_vector -> assert false
    | Pair (Reg _rd, Reg _rn), NEG_vector -> assert false
    | Pair (Reg _rd, Reg _rn), RBIT -> assert false
    | Pair (Reg _rd, Reg _rn), REV -> assert false
    | Pair (Reg _rd, Reg _rn), REV16 -> assert false
    | Pair (Reg _rd, Reg _rn), SCVTF -> assert false
    | Pair (Reg _rd, Reg _rn), SCVTF_vector -> assert false
    | Pair (Reg _rd, Reg _rn), SMOV _ -> assert false
    | Pair (Reg _rd, Reg _rn), SQXTN -> assert false
    | Pair (Reg _rd, Reg _rn), SQXTN2 -> assert false
    | Pair (Reg _rd, Mem _addressing), STR -> assert false
    | Pair (Reg _rd, Mem _addressing), STRB -> assert false
    | Pair (Reg _rd, Mem _addressing), STRH -> assert false
    | Pair (Reg _rd, Mem _addressing), STR_simd_and_fp -> assert false
    | Pair (Reg _rd, Reg _rn), SXTL -> assert false
    | Pair (Reg _rd, Bitmask _), TST -> assert false
    | Pair (Reg _rd, Reg _rn), UADDLP_vector -> assert false
    | Pair (Reg _rd, Reg _rn), UMOV _ -> assert false
    | Pair (Reg _rd, Reg _rn), UQXTN -> assert false
    | Pair (Reg _rd, Reg _rn), UQXTN2 -> assert false
    | Pair (Reg _rd, Reg _rn), UXTL -> assert false
    | Pair (Reg _rd, Reg _rn), XTN -> assert false
    | Pair (Reg _rd, Reg _rn), XTN2 -> assert false
    (* Triple operations *)
    | Triple (Reg _rd, Reg _rn, Reg _rm), ADDP_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), ADD_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Bitmask _), AND_immediate -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), AND_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), ASRV -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), CM_register _ -> assert false
    | Triple (Reg _rd, Reg _rn, Bitmask _), EOR_immediate -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), EOR_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FADD -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FADDP_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FADD_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FCM_register _ -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FDIV -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FDIV_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMAX -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMAX_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMIN -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMIN_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMUL -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FMUL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FNMUL -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FSUB -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), FSUB_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Mem _), LDP -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), LSLV -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), LSRV -> assert false
    | Triple (Reg _rd, _, Shift _), MOVK -> assert false
    | Triple (Reg _rd, _, Optional _), MOVN -> assert false
    | Triple (Reg _rd, _, Optional _), MOVZ -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), MULL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), MUL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Bitmask _), ORR_immediate -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), ORR_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SDIV -> assert false
    | Triple (Reg _rd, Reg _rn, Imm _), SHL -> assert false
    | Triple (Reg _rd, Reg _rn, Sym _), SHL -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SMAX_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SMIN_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SMULH -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SMULL2_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SMULL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SQADD_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SQSUB_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SSHL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Imm _), SSHR -> assert false
    | Triple (Reg _rd, Reg _rn, Sym _), SSHR -> assert false
    | Triple (Reg _rd, Reg _rn, Mem _), STP -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), SUB_vector -> assert false
    | Triple (Reg _rd, Imm _, Imm _), TBNZ -> assert false
    | Triple (Reg _rd, Imm _, Sym _), TBNZ -> assert false
    | Triple (Reg _rd, Imm _, Imm_float _), TBNZ -> assert false
    | Triple (Reg _rd, Imm _, Imm_nativeint _), TBNZ -> assert false
    | Triple (Reg _rd, Sym _, Imm _), TBNZ -> assert false
    | Triple (Reg _rd, Sym _, Sym _), TBNZ -> assert false
    | Triple (Reg _rd, Sym _, Imm_float _), TBNZ -> assert false
    | Triple (Reg _rd, Sym _, Imm_nativeint _), TBNZ -> assert false
    | Triple (Reg _rd, Imm _, Imm _), TBZ -> assert false
    | Triple (Reg _rd, Imm _, Sym _), TBZ -> assert false
    | Triple (Reg _rd, Imm _, Imm_float _), TBZ -> assert false
    | Triple (Reg _rd, Imm _, Imm_nativeint _), TBZ -> assert false
    | Triple (Reg _rd, Sym _, Imm _), TBZ -> assert false
    | Triple (Reg _rd, Sym _, Sym _), TBZ -> assert false
    | Triple (Reg _rd, Sym _, Imm_float _), TBZ -> assert false
    | Triple (Reg _rd, Sym _, Imm_nativeint _), TBZ -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UMAX_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UMIN_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UMULH -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UMULL2_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UMULL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UQADD_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), UQSUB_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), USHL_vector -> assert false
    | Triple (Reg _rd, Reg _rn, Imm _), USHR -> assert false
    | Triple (Reg _rd, Reg _rn, Sym _), USHR -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), ZIP1 -> assert false
    | Triple (Reg _rd, Reg _rn, Reg _rm), ZIP2 -> assert false
    (* Quad operations *)
    | Quad (Reg _rd, Reg _rn, Imm _, Optional _), ADD_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Optional _), ADD_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), ADD_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Optional _), ADDS -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Optional _), ADDS -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), AND_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), CSEL -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), CSINC -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), EOR_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Imm _), EXT -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Sym _), EXT -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Cond _), FCSEL -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FMADD -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FMSUB -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FNMADD -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), FNMSUB -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), MADD -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Reg _ra), MSUB -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), ORR_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Imm _), SBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Imm _), SBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Sym _), SBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Sym _), SBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Optional _), SUB_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Optional _), SUB_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), SUB_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Optional _), SUBS_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Optional _), SUBS_immediate -> assert false
    | Quad (Reg _rd, Reg _rn, Reg _rm, Optional _), SUBS_shifted_register ->
      assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Imm _), UBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Imm _), UBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Imm _, Sym _), UBFM -> assert false
    | Quad (Reg _rd, Reg _rn, Sym _, Sym _), UBFM -> assert false

  (* (* PC-relative addressing - C4.1.92.2 *) | Pair (Reg rd, Sym _), ADR -> let
     rd_bits = Reg.encoding rd in let immlo = 0 in let immhi = 0 in encode_adr
     ~op:0 ~immlo ~immhi ~rd:rd_bits | Pair (Reg rd, _), ADRP -> let rd_bits =
     Reg.encoding rd in let immlo = 0 in let immhi = 0 in encode_adr ~op:1
     ~immlo ~immhi ~rd:rd_bits (* Logical (immediate) - C4.1.92.6 *) | Triple
     (Reg rd, Reg rn, Bitmask _), AND_immediate -> let rd_bits = Reg.encoding rd
     in let rn_bits = Reg.encoding rn in encode_logical_immediate ~sf:1
     ~opc:0b00 ~n:0 ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd,
     Reg rn, Bitmask _), ORR_immediate -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in encode_logical_immediate ~sf:1 ~opc:0b01 ~n:0
     ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Reg rn, Bitmask
     _), EOR_immediate -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in encode_logical_immediate ~sf:1 ~opc:0b10 ~n:0 ~immr:0
     ~imms:0 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Imm (Twelve imm12),
     Optional shift), ADD_immediate -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let sh_bit = match shift with None -> 0 | Some
     Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh:sh_bit
     ~imm12 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _, Optional
     shift), ADD_immediate -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let sh_bit = match shift with None -> 0 | Some
     Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh:sh_bit
     ~imm12:0 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Imm (Twelve
     imm12), Optional shift), SUB_immediate -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let sh_bit = match shift with None -> 0 |
     Some Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:0
     ~sh:sh_bit ~imm12 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _,
     Optional shift), SUB_immediate -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let sh_bit = match shift with None -> 0 | Some
     Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:0 ~sh:sh_bit
     ~imm12:0 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Imm (Twelve
     imm12), Optional shift), SUBS_immediate -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let sh_bit = match shift with None -> 0 |
     Some Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:1
     ~sh:sh_bit ~imm12 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _,
     Optional shift), SUBS_immediate -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let sh_bit = match shift with None -> 0 | Some
     Lsl_by_twelve -> 1 in encode_add_sub_immediate ~sf:1 ~op:1 ~s:1 ~sh:sh_bit
     ~imm12:0 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Reg rm, Optional
     None), ADD_shifted_register -> let rd_bits = Reg.encoding rd in let rn_bits
     = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_add_sub_shifted_register ~sf:1 ~op:0 ~s:0 ~shift:0 ~rm:rm_bits
     ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn, Reg rm, Optional
     (Some (Shift { kind; amount = Six imm6 })) ), ADD_shifted_register ) -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let shift_type = encode_shift_type kind in
     encode_add_sub_shifted_register ~sf:1 ~op:0 ~s:0 ~shift:shift_type
     ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Reg rm,
     Optional None), SUB_shifted_register -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:0 ~shift:0 ~rm:rm_bits
     ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn, Reg rm, Optional
     (Some (Shift { kind; amount = Six imm6 })) ), SUB_shifted_register ) -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let shift_type = encode_shift_type kind in
     encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:0 ~shift:shift_type
     ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Reg rm,
     Optional None), SUBS_shifted_register -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:1 ~shift:0 ~rm:rm_bits
     ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn, Reg rm, Optional
     (Some (Shift { kind; amount = Six imm6 })) ), SUBS_shifted_register ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in let shift_type = encode_shift_type kind in
     encode_add_sub_shifted_register ~sf:1 ~op:1 ~s:1 ~shift:shift_type
     ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Imm_nativeint
     imm, Optional shift), MOVZ -> let rd_bits = Reg.encoding rd in let imm16 =
     Nativeint.to_int imm land 0xFFFF in let hw = match shift with | None -> 0 |
     Some (Shift { kind = LSL; amount = Six sh }) -> if sh mod 16 <> 0 || sh < 0
     || sh > 48 then Misc.fatal_errorf "MOVZ: shift must be 0, 16, 32, or 48,
     got %d" sh (); sh / 16 | Some (Shift { kind = LSL; amount = Twelve _ }) |
     Some (Shift { kind = ASR; _ }) | Some (Shift { kind = LSR; _ }) ->
     Misc.fatal_error "MOVZ: invalid shift amount" in encode_move_wide ~sf:1
     ~opc:0b10 ~hw ~imm16 ~rd:rd_bits | Triple (Reg rd, Sym sym, Optional
     shift), MOVZ -> let rd_bits = Reg.encoding rd in ignore sym; (* XXX *) let
     imm16 = 0 in let hw = match shift with | None -> 0 | Some (Shift { kind =
     LSL; amount = Six sh }) -> if sh mod 16 <> 0 || sh < 0 || sh > 48 then
     Misc.fatal_errorf "MOVZ: shift must be 0, 16, 32, or 48, got %d" sh (); sh
     / 16 | Some (Shift { kind = LSL; amount = Twelve _ }) | Some (Shift { kind
     = ASR; _ }) | Some (Shift { kind = LSR; _ }) -> Misc.fatal_error "MOVZ:
     invalid shift amount" in encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16
     ~rd:rd_bits | Triple (Reg rd, Imm_float f, Optional shift), MOVZ -> let
     rd_bits = Reg.encoding rd in let imm16 = Int64.to_int (Int64.bits_of_float
     f) land 0xFFFF in let hw = match shift with | None -> 0 | Some (Shift {
     kind = LSL; amount = Six sh }) -> if sh mod 16 <> 0 || sh < 0 || sh > 48
     then Misc.fatal_errorf "MOVZ: shift must be 0, 16, 32, or 48, got %d" sh
     (); sh / 16 | Some (Shift { kind = LSL; amount = Twelve _ }) | Some (Shift
     { kind = ASR; _ }) | Some (Shift { kind = LSR; _ }) -> Misc.fatal_error
     "MOVZ: invalid shift amount" in encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16
     ~rd:rd_bits | Triple (Reg rd, Imm (Twelve imm), Optional None), MOVN -> let
     rd_bits = Reg.encoding rd in encode_move_wide ~sf:1 ~opc:0b00 ~hw:0
     ~imm16:imm ~rd:rd_bits | ( Triple ( Reg rd, Imm (Twelve imm), Optional
     (Some (Shift { kind = LSL; amount = Six sh })) ), MOVN ) -> let rd_bits =
     Reg.encoding rd in let imm16 = imm land 0xFFFF in if sh mod 16 <> 0 || sh <
     0 || sh > 48 then Misc.fatal_errorf "MOVN: shift must be 0, 16, 32, or 48,
     got %d" sh (); let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b00 ~hw
     ~imm16 ~rd:rd_bits | ( Triple (Reg rd, Imm_nativeint imm, Shift { kind =
     LSL; amount = Six sh }), MOVK ) -> let rd_bits = Reg.encoding rd in let
     imm16 = Nativeint.to_int imm land 0xFFFF in if sh mod 16 <> 0 || sh < 0 ||
     sh > 48 then Misc.fatal_errorf "MOVK: shift must be 0, 16, 32, or 48, got
     %d" sh (); let hw = sh / 16 in encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16
     ~rd:rd_bits | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), UBFM
     -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr ~imms ~rn:rn_bits ~rd:rd_bits |
     Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), SBFM -> let rd_bits
     = Reg.encoding rd in let rn_bits = Reg.encoding rn in encode_bitfield ~sf:1
     ~opc:0b00 ~n:1 ~immr ~imms ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn,
     Imm _, Sym _), UBFM -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr:0 ~imms:0
     ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _, Imm _), UBFM -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_bitfield ~sf:1 ~opc:0b10 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits
     ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _, Sym _), UBFM -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in encode_bitfield ~sf:1
     ~opc:0b10 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg
     rn, Imm _, Sym _), SBFM -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr:0 ~imms:0
     ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _, Imm _), SBFM -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_bitfield ~sf:1 ~opc:0b00 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits
     ~rd:rd_bits | Quad (Reg rd, Reg rn, Sym _, Sym _), SBFM -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in encode_bitfield ~sf:1
     ~opc:0b00 ~n:1 ~immr:0 ~imms:0 ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd,
     Reg rn, Reg rm), LSLV -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_data_proc_2_source ~sf:1 ~s:0 ~opcode:0b001000 ~rm:rm_bits
     ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Reg rn, Reg rm), LSRV -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_data_proc_2_source ~sf:1 ~s:0 ~opcode:0b001001
     ~rm:rm_bits ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Reg rn, Reg rm), ASRV
     -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_data_proc_2_source ~sf:1 ~s:0
     ~opcode:0b001010 ~rm:rm_bits ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg
     rn, Reg rm, Reg ra), MADD -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b000
     ~o0:0 ~rm:rm_bits ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg
     rn, Reg rm, Reg ra), MSUB -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let ra_bits =
     Reg.encoding ra in encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b000
     ~o0:1 ~rm:rm_bits ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | Quad (Reg rd, Reg
     rn, Reg rm, Optional None), AND_shifted_register -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_logical_shifted_register ~sf:1 ~opc:0b00 ~shift:0
     ~n:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn,
     Reg rm, Optional (Some (Shift { kind; amount = Six imm6 })) ),
     AND_shifted_register ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
     encode_shift_type kind in encode_logical_shifted_register ~sf:1 ~opc:0b00
     ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Quad
     (Reg rd, Reg rn, Reg rm, Optional None), ORR_shifted_register -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_logical_shifted_register ~sf:1 ~opc:0b01 ~shift:0
     ~n:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn,
     Reg rm, Optional (Some (Shift { kind; amount = Six imm6 })) ),
     ORR_shifted_register ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
     encode_shift_type kind in encode_logical_shifted_register ~sf:1 ~opc:0b01
     ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Quad
     (Reg rd, Reg rn, Reg rm, Optional None), EOR_shifted_register -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_logical_shifted_register ~sf:1 ~opc:0b10 ~shift:0
     ~n:0 ~rm:rm_bits ~imm6:0 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg rd, Reg rn,
     Reg rm, Optional (Some (Shift { kind; amount = Six imm6 })) ),
     EOR_shifted_register ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in let shift_type =
     encode_shift_type kind in encode_logical_shifted_register ~sf:1 ~opc:0b10
     ~shift:shift_type ~n:0 ~rm:rm_bits ~imm6 ~rn:rn_bits ~rd:rd_bits | Pair
     (Reg rd, Reg rn), RBIT -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000
     ~opcode:0b000000 ~rn:rn_bits ~rd:rd_bits | Pair (Reg rd, Reg rn), REV16 ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000001
     ~rn:rn_bits ~rd:rd_bits | Pair (Reg rd, Reg rn), REV -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000011
     ~rn:rn_bits ~rd:rd_bits | Pair (Reg rd, Reg rn), CLZ -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000100
     ~rn:rn_bits ~rd:rd_bits | Pair (Reg rd, Reg rn), CNT -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in
     encode_data_proc_1_source ~sf:1 ~s:0 ~opcode2:0b00000 ~opcode:0b000111
     ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Reg rn, Reg rm), SMULH -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b010
     ~o0:0 ~rm:rm_bits ~ra:0b11111 ~rn:rn_bits ~rd:rd_bits | Triple (Reg rd, Reg
     rn, Reg rm), UMULH -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in
     encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b110 ~o0:0 ~rm:rm_bits
     ~ra:0b11111 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V8B); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V16B); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V4H); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V8H); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V2S); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V4S); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V1D); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b11 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V2D); _ } as rd), Reg rn, Reg rm), SQADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits
     ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V8B); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V16B); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V4H); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V8H); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V2S); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V4S); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Vector V2D); _ } as rd), Reg rn, Reg rm), ADD_vector ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits
     ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | Triple (Reg { reg_name = _; _ },
     _, _), ADD_vector -> assert false (* TODO XXX *) | ( Triple (Reg ({
     reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b00
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b00
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b01
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b01
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:1 ~size:0b10
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b10
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({
     reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm), SUB_vector ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:1 ~size:0b11
     ~rm:rm_bits ~opcode:0b10000 ~rn:rn_bits ~rd:rd_bits | Triple (Reg {
     reg_name = _; _ }, _, _), SUB_vector -> assert false (* TODO XXX *) | (
     Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     AND_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm),
     AND_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits | Triple
     (Reg { reg_name = _; _ }, _, _), AND_vector -> assert false (* TODO XXX *)
     | ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg
     rm), ORR_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), ORR_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits |
     Triple (Reg { reg_name = _; _ }, _, _), ORR_vector -> assert false (* TODO
     XXX *) | ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn,
     Reg rm), EOR_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), EOR_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00011 ~rn:rn_bits ~rd:rd_bits |
     Triple (Reg { reg_name = _; _ }, _, _), EOR_vector -> assert false (* TODO
     XXX *) | ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn,
     Reg rm), SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     SMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     SMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     UMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01100 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     UMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b00 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b01 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:0 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     MUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in encode_simd_three_same ~q:1 ~u:0
     ~size:0b10 ~rm:rm_bits ~opcode:0b10011 ~rn:rn_bits ~rd:rd_bits | ( Triple
     (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     SQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     UQADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b11 ~rm:rm_bits ~opcode:0b00001 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     UQSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b11 ~rm:rm_bits ~opcode:0b00101 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     SSHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8B); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V16B); _ } as rd), Reg rn, Reg
     rm), USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4H); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V8H); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b10 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     USHL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b11 ~rm:rm_bits ~opcode:0b01000 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FADD_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FSUB_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits ~opcode:0b11010 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FMUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FMUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FMUL_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b11011 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FDIV_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FDIV_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b00 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FDIV_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:1 ~size:0b01 ~rm:rm_bits ~opcode:0b11111 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b00 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FMAX_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b01 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2S); _ } as rd), Reg rn, Reg rm),
     FMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:0 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V4S); _ } as rd), Reg rn, Reg rm),
     FMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b10 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Triple (Reg ({ reg_name = Neon (Vector V2D); _ } as rd), Reg rn, Reg rm),
     FMIN_vector ) -> let rd_bits = Reg.encoding rd in let rn_bits =
     Reg.encoding rn in let rm_bits = Reg.encoding rm in encode_simd_three_same
     ~q:1 ~u:0 ~size:0b11 ~rm:rm_bits ~opcode:0b11110 ~rn:rn_bits ~rd:rd_bits |
     ( Pair (Reg ({ reg_name = Neon (Scalar S); _ } as rd), Imm_float _f),
     FMOV_scalar_immediate ) -> let rd_bits = Reg.encoding rd in let imm8 = 0 in
     encode_fp_immediate ~ftype:0b00 ~imm8 ~rd:rd_bits | ( Pair (Reg ({ reg_name
     = Neon (Scalar D); _ } as rd), Imm_float _f), FMOV_scalar_immediate ) ->
     let rd_bits = Reg.encoding rd in let imm8 = 0 in encode_fp_immediate
     ~ftype:0b01 ~imm8 ~rd:rd_bits | ( Pair (Reg ({ reg_name = Neon (Scalar S);
     _ } as rd), Imm_float _f), FMOV_scalar_immediate ) -> . | ( Triple (Reg ({
     reg_name = Neon (Scalar S); _ } as rd), Reg rn, Reg rm), FADD ) -> let
     rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0010 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FADD ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0010 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FSUB ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0011 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FSUB ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0011 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FMUL ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FMUL ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FDIV ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FDIV ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0001 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FMAX ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0100 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FMAX ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0100 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FMIN ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b0101 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FMIN ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b0101 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm), FNMUL ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b00 ~rm:rm_bits
     ~opcode:0b1000 ~rn:rn_bits ~rd:rd_bits | ( Triple (Reg ({ reg_name = Neon
     (Scalar D); _ } as rd), Reg rn, Reg rm), FNMUL ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in encode_fp_2_source ~ftype:0b01 ~rm:rm_bits
     ~opcode:0b1000 ~rn:rn_bits ~rd:rd_bits | ( Quad ( Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm, Cond cond ), FCSEL ) -> let rd_bits
     = Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let cond_bits = encode_condition cond in
     encode_fp_cond_select ~ftype:0b00 ~rm:rm_bits ~cond:cond_bits ~rn:rn_bits
     ~rd:rd_bits | ( Quad ( Reg ({ reg_name = Neon (Scalar D); _ } as rd), Reg
     rn, Reg rm, Cond cond ), FCSEL ) -> let rd_bits = Reg.encoding rd in let
     rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let cond_bits
     = encode_condition cond in encode_fp_cond_select ~ftype:0b01 ~rm:rm_bits
     ~cond:cond_bits ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({ reg_name = Neon
     (Scalar S); _ } as rd), Reg rn, Reg rm, Reg ra), FMADD ) -> let rd_bits =
     Reg.encoding rd in let rn_bits = Reg.encoding rn in let rm_bits =
     Reg.encoding rm in let ra_bits = Reg.encoding ra in encode_fp_3_source
     ~ftype:0b00 ~o1:0 ~rm:rm_bits ~o0:0 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | (
     Quad (Reg ({ reg_name = Neon (Scalar D); _ } as rd), Reg rn, Reg rm, Reg
     ra), FMADD ) -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding
     rn in let rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b01 ~o1:0 ~rm:rm_bits ~o0:0 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({ reg_name = Neon (Scalar S); _ } as
     rd), Reg rn, Reg rm, Reg ra), FMSUB ) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let
     ra_bits = Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:0
     ~rm:rm_bits ~o0:1 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({
     reg_name = Neon (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra), FMSUB ) ->
     let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b01 ~o1:0 ~rm:rm_bits ~o0:1 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({ reg_name = Neon (Scalar S); _ } as
     rd), Reg rn, Reg rm, Reg ra), FNMADD ) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let
     ra_bits = Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:1
     ~rm:rm_bits ~o0:0 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({
     reg_name = Neon (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra), FNMADD )
     -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b01 ~o1:1 ~rm:rm_bits ~o0:0 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({ reg_name = Neon (Scalar S); _ } as
     rd), Reg rn, Reg rm, Reg ra), FNMSUB ) -> let rd_bits = Reg.encoding rd in
     let rn_bits = Reg.encoding rn in let rm_bits = Reg.encoding rm in let
     ra_bits = Reg.encoding ra in encode_fp_3_source ~ftype:0b00 ~o1:1
     ~rm:rm_bits ~o0:1 ~ra:ra_bits ~rn:rn_bits ~rd:rd_bits | ( Quad (Reg ({
     reg_name = Neon (Scalar D); _ } as rd), Reg rn, Reg rm, Reg ra), FNMSUB )
     -> let rd_bits = Reg.encoding rd in let rn_bits = Reg.encoding rn in let
     rm_bits = Reg.encoding rm in let ra_bits = Reg.encoding ra in
     encode_fp_3_source ~ftype:0b01 ~o1:1 ~rm:rm_bits ~o0:1 ~ra:ra_bits
     ~rn:rn_bits ~rd:rd_bits | Pair (Reg rd, addressing), LDAR -> assert false |
     Pair (Reg rd, addressing), LDR -> assert false | Pair (Reg rd, addressing),
     LDRB -> assert false | Pair (Reg rd, addressing), LDRH -> assert false |
     Pair (Reg rd, addressing), STR -> assert false | Pair (Reg rd, addressing),
     STRB -> assert false | Pair (Reg rd, addressing), STRH -> assert false | (
     Pair ( Reg ({ reg_name = Neon (Scalar S); _ } as rt), Mem (Offset (_,
     Symbol _)) ), LDR_simd_and_fp ) -> let rt_bits = Reg.encoding rt in
     encode_load_literal ~opc:0b00 ~v:1 ~imm19:0 ~rt:rt_bits | ( Pair ( Reg ({
     reg_name = Neon (Scalar D); _ } as rt), Mem (Offset (_, Symbol _)) ),
     LDR_simd_and_fp ) -> let rt_bits = Reg.encoding rt in encode_load_literal
     ~opc:0b01 ~v:1 ~imm19:0 ~rt:rt_bits | ( Pair ( Reg ({ reg_name = Neon
     (Scalar Q); _ } as rt), Mem (Offset (_, Symbol _)) ), LDR_simd_and_fp ) ->
     let rt_bits = Reg.encoding rt in encode_load_literal ~opc:0b10 ~v:1
     ~imm19:0 ~rt:rt_bits | ( Pair ( Reg ({ reg_name = Neon (Scalar _); _ } as
     _rt), Mem (Offset (_, Symbol _)) ), STR_simd_and_fp ) -> assert false | (
     Pair ( Reg ({ reg_name = Neon (Scalar _); _ } as _rt), Mem (Offset (_, Imm
     _)) ), LDR_simd_and_fp ) -> assert false | ( Pair ( Reg ({ reg_name = Neon
     (Scalar _); _ } as _rt), Mem (Offset (_, Imm _)) ), STR_simd_and_fp ) ->
     assert false | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rt), Mem
     (Reg _)), LDR_simd_and_fp ) -> assert false | ( Pair (Reg ({ reg_name =
     Neon (Scalar _); _ } as _rt), Mem (Reg _)), STR_simd_and_fp ) -> assert
     false | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rt), Mem (Pre
     (_, _))), LDR_simd_and_fp ) -> assert false | ( Pair (Reg ({ reg_name =
     Neon (Scalar _); _ } as _rt), Mem (Pre (_, _))), STR_simd_and_fp ) ->
     assert false | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rt), Mem
     (Post (_, _))), LDR_simd_and_fp ) -> assert false | ( Pair (Reg ({ reg_name
     = Neon (Scalar _); _ } as _rt), Mem (Post (_, _))), STR_simd_and_fp ) ->
     assert false | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rd), Sym
     _), FMOV_scalar_immediate ) -> assert false | Pair (Reg ({ reg_name = Neon
     (Scalar _); _ } as _rd), Imm _), MOVI -> assert false | Pair (Reg ({
     reg_name = Neon (Vector _); _ } as _rd), Imm _), MOVI -> assert false |
     Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rd), Sym _), MOVI ->
     assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Sym
     _), MOVI -> assert false | Pair (Reg ({ reg_name = Neon (Scalar _); _ } as
     _rd), Reg _), FABS -> assert false | Pair (Reg ({ reg_name = Neon (Scalar
     _); _ } as _rn), Reg _), FCMP -> assert false | Pair (Reg ({ reg_name =
     Neon (Scalar _); _ } as _rd), Reg _), FCVT -> assert false | ( Pair (Reg ({
     reg_name = Neon (Scalar _); _ } as _rd), Reg _), FMOV_general_or_register )
     -> assert false | Pair (Reg ({ reg_name = Neon (Scalar _); _ } as _rd), Reg
     _), FNEG -> assert false | Pair (Reg ({ reg_name = Neon (Scalar _); _ } as
     _rd), Reg _), FSQRT -> assert false | Pair (Reg ({ reg_name = Neon (Scalar
     _); _ } as _rd), Reg _), SCVTF -> assert false | Pair (Reg ({ reg_name =
     Neon (Scalar _); _ } as _rd), Reg _), FRINT _ -> assert false | ( Pair (Reg
     ({ reg_name = Neon (Scalar _); _ } as _rd), Imm _), FMOV_scalar_immediate )
     -> . | ( Pair (Reg ({ reg_name = Neon (Scalar S); _ } as _rd),
     Imm_nativeint _), FMOV_scalar_immediate ) -> assert false | ( Pair (Reg ({
     reg_name = Neon (Scalar D); _ } as _rd), Imm_nativeint _),
     FMOV_scalar_immediate ) -> assert false | Pair (Reg ({ reg_name = Neon
     (Scalar D); _ } as _rd), Reg _), SMOV _ -> assert false | Pair (Reg ({
     reg_name = Neon (Scalar D); _ } as _rd), Reg _), UMOV _ -> assert false |
     Pair (Reg ({ reg_name = Neon (Scalar B); _ } as _rd), Reg _), ADDV ->
     assert false | ( Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Sym
     _), FMOV_vector_immediate ) -> assert false | ( Pair (Reg ({ reg_name =
     Neon (Vector _); _ } as _rd), Imm_nativeint _), FMOV_vector_immediate ) ->
     assert false | ( Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd),
     Imm_float _), FMOV_vector_immediate ) -> assert false | ( Pair (Reg ({
     reg_name = Neon (Vector _); _ } as _rd), Imm _), FMOV_vector_immediate ) ->
     . | Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _),
     ABS_vector -> assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ }
     as _rd), Reg _), CNT_vector -> assert false | Pair (Reg ({ reg_name = Neon
     (Vector _); _ } as _rd), Reg _), CVT_vector -> assert false | Pair (Reg ({
     reg_name = Neon (Vector _); _ } as _rd), Reg _), FCVTL_vector -> assert
     false | ( Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _),
     FCVTNS_vector ) -> assert false | Pair (Reg ({ reg_name = Neon (Vector _);
     _ } as _rd), Reg _), FCVTN_vector -> assert false | ( Pair (Reg ({ reg_name
     = Neon (Vector _); _ } as _rd), Reg _), FCVTZS_vector ) -> assert false |
     Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _), FNEG_vector
     -> assert false | ( Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd),
     Reg _), FRECPE_vector ) -> assert false | ( Pair (Reg ({ reg_name = Neon
     (Vector _); _ } as _rd), Reg _), FRSQRTE_vector ) -> assert false | Pair
     (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _), FSQRT_vector ->
     assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg
     _), MOV_vector -> assert false | Pair (Reg ({ reg_name = Neon (Vector _); _
     } as _rd), Reg _), MVN_vector -> assert false | Pair (Reg ({ reg_name =
     Neon (Vector _); _ } as _rd), Reg _), NEG_vector -> assert false | Pair
     (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _), SCVTF_vector ->
     assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg
     _), SQXTN -> assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as
     _rd), Reg _), SQXTN2 -> assert false | Pair (Reg ({ reg_name = Neon (Vector
     _); _ } as _rd), Reg _), SXTL -> assert false | Pair (Reg ({ reg_name =
     Neon (Vector _); _ } as _rd), Reg _), UQXTN -> assert false | Pair (Reg ({
     reg_name = Neon (Vector _); _ } as _rd), Reg _), UQXTN2 -> assert false |
     Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _), UXTL ->
     assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg
     _), XTN -> assert false | Pair (Reg ({ reg_name = Neon (Vector _); _ } as
     _rd), Reg _), XTN2 -> assert false | Pair (Reg ({ reg_name = Neon (Vector
     _); _ } as _rd), Reg _), CM_zero _ -> assert false | Pair (Reg ({ reg_name
     = Neon (Vector _); _ } as _rd), Reg _), DUP _ -> assert false | Pair (Reg
     ({ reg_name = Neon (Vector _); _ } as _rd), Reg _), FCM_zero _ -> assert
     false | ( Pair (Reg ({ reg_name = Neon (Vector _); _ } as _rd), Reg _),
     FRINT_vector _ ) -> assert false | Pair (Reg ({ reg_name = Neon (Vector _);
     _ } as _rd), Reg _), INS _ -> assert false | Pair (Reg ({ reg_name = Neon
     (Vector _); _ } as _rd), Reg _), INS_V _ -> assert false | ( Pair (Reg ({
     reg_name = Neon (Vector _); _ } as _rd), Reg _), UADDLP_vector ) -> assert
     false *)
end
