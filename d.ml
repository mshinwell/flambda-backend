external (+) : int -> int -> int = "%addint"

let f i was compiled with_ ocamlopt =
  i + was + compiled + with_ + ocamlopt

let () =
  Sys.opaque_identity (
    let _ = (f [@inlined never]) 1 2 3 4 5 in
    ())
