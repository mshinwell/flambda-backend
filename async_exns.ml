let () = Sys.catch_break true

let () =
  try
    Sys.with_async_exns (fun () ->
      try
        while true do
          let _ = Sys.opaque_identity (42, Random.int 42) in
          ()
        done
      with exn -> Printf.printf "wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> Printf.printf "OK\n%!"
  | _ -> assert false

let () =
  let finished = ref false in
  try
    let b = Bytes.create 42 in
    Gc.finalise_last (fun () -> finished := true; raise Sys.Break) b;
    Sys.with_async_exns (fun () ->
      try
        let _ = Sys.opaque_identity b in
        while true do
          let _ = Sys.opaque_identity (42, Random.int 42) in
          ()
        done
      with exn -> assert false
    )
  with
  | Sys.Break -> assert !finished; Printf.printf "OK\n%!"
  | _ -> assert false

let () =
  try
    let b = Bytes.create 42 in
    Gc.finalise_last (fun () -> failwith "uncaught") b;
    Sys.with_async_exns (fun () ->
      try
        let _ = Sys.opaque_identity b in
        while true do
          let _ = Sys.opaque_identity (42, Random.int 42) in
          ()
        done
      with exn -> assert false
    )
  with
  | _ -> assert false

