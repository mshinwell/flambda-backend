type t = { mutable state : unit }

let[@inline never] [@local never] run (local_ f) = f ()

let[@inline never] [@local never] bad () =
  (fun () ->
    let ctx = { state = () } in
    run (fun () ->
      ignore (Sys.opaque_identity ctx : _);
      ())
    [@nontail])
    () [@nontail]

let () =
  while true do
    bad ()
  done
