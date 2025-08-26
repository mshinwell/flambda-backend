  done

let () =
  let domains = Array.init (num_domains - 1) (fun _ -> Domain.spawn go) in
  go ();
  Array.iter Domain.join domains;
  print_endline "ok"
