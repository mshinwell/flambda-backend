(* Test to ensure that asynchronous exceptions from the GC skip the
   current frame under Flambda 2. *)

let whoops = "whoops"

let[@inline never] h x =
  try
    if Sys.opaque_identity false then begin
      failwith "just to keep exn handler alive"
    end;
    (* An allocation is done in a loop so that the finaliser for [pair],
       below, is eventually run. *)
    while true do
      (* The emerging async exn should skip the handler in [h] *)
      let _ = Sys.opaque_identity (x, Random.int 42) in
      ()
    done
  with exn ->
    Printf.eprintf "should not have caught exn here (%s)\n%!"
      (Printexc.to_string exn);
    exit 1

let[@inline never] g x =
  let pair = x, Random.int 42 in
  Gc.finalise_last (fun _ -> failwith whoops) pair;
  (* [pair] is now dead *)
  let () = h x in
  print_endline "should not have returned from [h]";
  exit 2

let[@inline never] f () =
  try
    g 1  (* not a tail call because of the try/with *)
  with (Failure msg) ->  (* the async exn should be delivered here *)
    assert (String.equal msg whoops);
    exit 0

let () =
  if Config.flambda2 then f ()
  else ()
