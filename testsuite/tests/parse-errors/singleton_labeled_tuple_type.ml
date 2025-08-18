(* TEST
<<<<<<< HEAD
 flags = "-extension labeled_tuples";
||||||| 23e84b8c4d
=======
>>>>>>> ocaml/5.4
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
let foo (just_x : (x : int)) = just_x
