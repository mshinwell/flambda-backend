let () =
  let done = ref false in
  try
    let b = Bytes.create 42 in
    Gc.finalise_last (fun () -> done := true; raise Break) b;
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
  | Break -> assert !done; Printf.printf "OK\n%!"
  | _ -> assert false

(*
exception E

let () =
  let done = ref false in
  try
    Sys.with_async_exns (fun () ->
      Sys.bracket ~acquire:(fun () ->
          let b = Bytes.create 42 in
          let finalised = ref false in
          Gc.finalise_last
            (fun () -> finalised := true; raise Exit)
            b;
          for x = 0 to 1_000_000 do
            let _ = Sys.opaque_identity (42, Random.int 42) in
            ()
          done;
          assert !finalised;
          12345)
        ~release:(fun n -> Printf.printf "release %d\n%!" n)
        (fun n ->
          let b2 = Bytes.create 42 in
          Gc.finalise_last (fun () -> done := true; raise Break) b2;
          for x = 0 to 1_000_000 do
            let _ = Sys.opaque_identity (42, Random.int 42) in
            ()
          done;
          assert false
        )
    )
  with Break -> assert !done; Printf.printf "OK\n"
  | _ -> assert false
*)
