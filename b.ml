external ( + ) : int -> int -> int = "%addint"

let x_in_b = A.x

let foo_in_b y = A.foo y + 2

let bar_in_b z = A.bar z

module type S = sig
  val x : int
  val foo : int -> int
  val bar : int -> int
end

let mod_A = (module A : S)
