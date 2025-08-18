(* TEST
<<<<<<< HEAD
 expect;
||||||| 23e84b8c4d
=======
  expect;
>>>>>>> ocaml/5.4
*)

module M = struct type t = T end
let poly3 : 'b. M.t -> 'b -> 'b =
  fun T x -> x
[%%expect {|
module M : sig type t = T end
val poly3 : M.t -> 'b -> 'b = <fun>
|}];;
