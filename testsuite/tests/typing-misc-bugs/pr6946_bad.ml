(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

external foo : int = "%ignore";;
let _ = foo ();;

(* TEST
 flags = " -w -a ";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
