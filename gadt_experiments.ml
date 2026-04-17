type _ t =
  | A : int t
  | B : string t
  | C : int -> int t
  | D : string -> string t

let f1 (x : int t) =
  match x with
  | A -> true

let f2 (x : int t) =
  match x with
  | A -> 1
  | C _ -> 2

let f3 (x : int t) =
  match x with
  | C _ -> true

let f4 (x : string t) =
  match x with
  | B -> 0
  | D _ -> 1
