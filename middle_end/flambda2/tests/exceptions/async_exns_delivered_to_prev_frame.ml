(* Test to ensure that asynchronous exceptions from the GC skip exception
   handlers with extra arguments under Flambda 2. *)

let whoops = "whoops"

let[@inline never] h x =
  let mut_var = ref 0 in
  try
    if Sys.opaque_identity false then failwith "just to keep exn handler alive";
    (* An allocation is done in a loop so that the finaliser for [pair], below,
       is eventually run. *)
    while true do
      (* The emerging async exn from the allocation of the pair on the next line
         should skip the handler in [h] because it uses a mutable variable *)
      let _ = Sys.opaque_identity (x, Random.int 42) in
      incr mut_var
    done
  with exn ->
    Printf.eprintf "should not have caught exn here (%s, mut_var=%d)\n%!"
      (Printexc.to_string exn) !mut_var;
    exit 1

let[@inline never] g x =
  let pair = x, Random.int 42 in
  Gc.finalise_last (fun _ -> failwith whoops) pair;
  (* [pair] is now dead *)
  let () = h x in
  print_endline "should not have returned from [h]";
  exit 2

let[@inline never] f () =
  try g 1 (* not a tail call because of the try/with *)
  with Failure msg ->
    (* the async exn should be delivered here *)
    assert (String.equal msg whoops);
    exit 0

let () = if Config.flambda2 then f () else ()
