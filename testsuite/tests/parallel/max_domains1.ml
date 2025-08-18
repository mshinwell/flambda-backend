(* TEST
<<<<<<< HEAD
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 ocamlrunparam += ",d=1";
 runtime5;
 multidomain;
 { native; }
||||||| 23e84b8c4d
=======
 ocamlrunparam += ",d=1";
>>>>>>> ocaml/5.4
*)

let _ =
  try
    Domain.spawn (fun _ -> print_endline "Expect failure") |> ignore
  with Failure _ -> print_string "ok\n"
