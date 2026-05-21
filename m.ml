#syntax quotations on

let cell = ref 2
let cell_read = Eval.eval <[ !M.cell ]>
let () = cell := !cell + 1
let () = print_int !cell; print_newline ()
