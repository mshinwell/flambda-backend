(* TEST
 compile_only = "true";
 flags = "-extension layouts_alpha -w +a-70";
 flambda2;
 {
   ocamlopt_flags = "-Oclassic";
   setup-ocamlopt.byte-build-env;
   ocamlopt.opt;
 }{
   ocamlopt_flags = "-O3";
   setup-ocamlopt.byte-build-env;
   ocamlopt.opt;
 }
*)

type void : void

(* Void parameter *)
external unsafe_set_void
  : ('a : value).
  'a or_null @ local -> ('a, void) idx_mut -> void -> unit
  = "%unsafe_set_idx"

let[@inline] unsafe_set_void (type a) (idx : (a, void) idx_mut) v =
  unsafe_set_void Null idx v

let f_void idx v = (unsafe_set_void [@inlined]) idx v

(* Unboxed product parameter *)
let[@inline] g_prod (x : #(int * float#)) = x

let call_g_prod (x : #(int * float#)) =
  (g_prod [@inlined]) x

(* Unboxed product with more fields *)
let[@inline] g_prod3 (x : #(int * float# * int)) = x

let call_g_prod3 (x : #(int * float# * int)) =
  (g_prod3 [@inlined]) x

(* Unboxed product parameter and return *)
let[@inline] id_prod (x : #(float# * float#)) : #(float# * float#) = x

let call_id_prod (x : #(float# * float#)) =
  (id_prod [@inlined]) x
