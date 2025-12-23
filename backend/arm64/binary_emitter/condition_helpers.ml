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

(* Floating-point condition codes use the same encoding as integer conditions.
   The difference is semantic: after FCMP, the flags have different meanings. *)
let encode_float_condition (cond : Float_cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101
