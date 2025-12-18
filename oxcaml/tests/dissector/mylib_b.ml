(* Library module B - depends on A *)

let call_increment () = Mylib_a.increment ()

let call_double x = Mylib_a.double x

let combined x =
  let v = Mylib_a.increment () in
  Mylib_a.double x + Mylib_a.triple v

let get_data_element i = Mylib_a.data.(i)
