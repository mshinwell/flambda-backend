(* TEST
 not-windows;
 include unix;
 hasunix;
  Unix.alarm 1 |> ignore;
  flush ch;
  print_endline "ok"
