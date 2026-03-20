let f x y g =
  for i = x to y do g i done

let empty x y =
  for i = x to y do () done
