external noalloc : int -> unit = "noalloc" [@@noalloc]
(* Changing the string '"noalloc"' to '"noalloc0"' on the line above
   will show what happens without the magic wrapper. *)

exception Noalloc_exn

let () =
  Callback.register "noalloc_exn" Noalloc_exn;
  let l = List.init 100 (fun i -> i) in
  try
    noalloc 42;
    assert false
  with Noalloc_exn -> (
    let _ = List.init 50 (fun i -> "foo") |> Sys.opaque_identity in
    List.iter (fun i -> Printf.printf "%d " i) l;
    print_newline ()
  )
