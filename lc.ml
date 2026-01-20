[@@@ocaml.flambda_o3]

external opaque : 'a -> 'a = "%opaque"
external ( + ) : int -> int -> int = "%addint"

let f () =
  let x = 1 + 2 in
  x, x

let g y =
  let x = 1 + 2 in
  match y with
  | None -> x, x
  | Some () -> x + 3, x

let r = opaque 45

let rr = r, r

let f2 () =
  let x = 1 + 2 in
  x, r
