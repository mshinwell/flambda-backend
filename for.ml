let f () =
  let array = Array.make 10_000 0 in 
  let sum = ref 0 in
  for i = 0 to Array.length array - 1 do
    sum := !sum + array.(i)
  done;
  !sum
