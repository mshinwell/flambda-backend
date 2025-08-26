(* TEST *)

open Compiler_libs

let run s =
  let lexbuf = Lexing.from_string s in
  let s = Parse.implementation lexbuf in
  let tstr, typed_expr, _ = Typemod.type_structure (Env.initial) s in
  let untyped_expr = Untypeast.untype_structure tstr in
  Format.printf "%s\n"
    (Format.asprintf "%a" Pprintast.structure untyped_expr);
  (* Make sure we can type it again. *)
  ignore(Typemod.type_structure (Env.initial) untyped_expr : _ * _ * _)

let () = run {| type t = A of {x:int} |};;

[%%expect{|
type t = | A of {x: int}
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

run {| let open struct type t = { mutable x : int [@atomic] } end in
       let _ = fun (v : t) -> v.x in () |};;

[%%expect{|
let open struct type t = {
                  mutable x: int [@atomic ]} end in
  let _ = fun (v : t) -> v.x in ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . 'a -> 'a = fun (type a) -> ( (fun x -> x : a -> a)) in
foo
- : unit = ()
|}];;

(* CR: untypeast/pprintast are totally busted on programs with modes in value
   bindings. Fix this. *)
run {| let foo : ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let (foo : 'a -> 'a) = ((fun x -> x : 'a -> 'a) : _ @ portable) in foo
- : unit = ()
|}];;

run {| let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let (foo : ('a : value) . 'a -> 'a) =
  ((fun x -> x : ('a : value) . 'a -> 'a) : _ @ portable) in foo
- : unit = ()
|}];;

(* CR ncourant: this looks like the wrong output.  Why "(mode *** default)"? *)
run {| let (local_ foo : 'a -> 'a) = fun x -> x in foo |}

[%%expect{|
let ((local_ foo : ('a : value) -> ('a : value)) : _) = (fun x -> x
                                                         : 'a -> 'a) in
foo
- : unit = ()
|}];;

(***********************************)
(* Value-kind *)

run "match None with Some (None : type a b c . a * b * c) -> 10 | _ -> 0";;

[%%expect{|
match None with
| Some (None : ('a : value) ('b : value) ('c : value) . 'a * 'b * 'c) -> 10
| _ -> 0
- : unit = ()
|}];;

run "match None with Some _ : type a b c . a * b * c -> 10 | _ -> 0";;

[%%expect{|
match None with
| Some (_ : ('a : value) ('b : value) ('c : value) . 'a * 'b * 'c) -> 10
| _ -> 0
- : unit = ()
|}];;

run {| match None with Some (_ as x : type a b c . a * b * c) -> x | _ -> (1,2,3) |};;

[%%expect{|
match None with
| Some ((_ as x) : ('a : value) ('b : value) ('c : value) . 'a * 'b * 'c) ->
    x
| _ -> (1, 2, 3)
- : unit = ()
|}];;

(* Contrived example for testing precedence. *)
run {| match None with Some ( (_ as x : int) as y : type a b c . a * b * c) -> x | _ -> 0 |};;

[%%expect{|
match None with
| Some (((_ as x) : int) as y :
        ('a : value) ('b : value) ('c : value) . 'a * 'b * 'c) -> x
| _ -> 0
- : unit = ()
|}];;

(***********************************)
(* Modes in arrow types *)

run {| fun () : (_ -> _ -> _) @ portable -> (fun x y -> ()) : _ -> _ -> _ |}

[%%expect{|
fun () : (_ -> _ -> _) @ portable ->
  ((fun x y -> () : _ -> _ -> _) : _ @ portable)
- : unit = ()
|}];;

run {| let f (g : local_ _ -> unit) = () in () |}

[%%expect{|
let f (g : local_ _ -> unit) = () in ()
- : unit = ()
|}];;

run {| let f (g : unique_ once_ _ -> unit) = () in () |}

[%%expect{|
let f (g : unique_ once_ _ -> unit) = () in ()
- : unit = ()
|}];;

run {| let f (g : _ @ nonportable @ contended -> unit) = () in () |}

[%%expect{|
let f (g : _ @ contended @ nonportable -> unit) = () in ()
- : unit = ()
|}];;

run {| let f (g : _ -> unit @@ portable) = () in () |}

[%%expect{|
let f (g : _ -> unit @@ portable) = () in ()
- : unit = ()
|}];;

run {| let f (g : local_ unique_ _ -> unit @@ portable) = () in () |}

[%%expect{|
let f (g : local_ unique_ _ -> unit @@ portable) = () in ()
- : unit = ()
|}];;

run {| let f (g : _ @ foo bar baz -> unit) = () in () |}

[%%expect{|
let f (g : _ @ bar @ baz @ foo -> unit) = () in ()
- : unit = ()
|}];;

run {| let f (g : _ -> unit @@ foo bar baz) = () in () |}

[%%expect{|
let f (g : _ -> unit @@ bar @@ baz @@ foo) = () in ()
- : unit = ()
|}];;

run {| let f : type a . local_ unique_ a @ bar baz foo -> unit = () in () |}

[%%expect{|
let (f : ('a : value) . local_ unique_ 'a @ bar @ baz @ foo -> unit) = () in
()
- : unit = ()
|}];;

run {| let f : type a . a -> unit @@ foo bar baz = () in () |}

[%%expect{|
let (f : ('a : value) . 'a -> unit @@ bar @@ baz @@ foo) = () in ()
- : unit = ()
|}];;

(***********************************)
(* We now support parsing (and untyping) parameter modes in functions *)

run {| fun local_ x -> x |}

[%%expect{|
fun local_ x -> x
- : unit = ()
|}];;

run {| fun local_ x unique_ y once_ z -> (x, y, z) |}

[%%expect{|
fun local_ x unique_ y once_ z -> (x, y, z)
- : unit = ()
|}];;

run {| fun local_ x y z -> (x, y, z) |}

[%%expect{|
fun local_ x y z -> (x, y, z)
- : unit = ()
|}];;

run {| fun x local_ y z -> (x, y, z) |}

[%%expect{|
fun x local_ y z -> (x, y, z)
- : unit = ()
|}];;

run {| let x = fun local_ x -> x in x |}

[%%expect{|
let x = fun local_ x -> x in x
- : unit = ()
|}];;

run {| let x = fun local_ unique_ once_ x -> x in x |}

[%%expect{|
let x = fun local_ unique_ once_ x -> x in x
- : unit = ()
|}];;

(***********************************)
(* Printing of list syntax attributes *)

run {| let _ = [%list.A] in () |}

[%%expect{|
let _ = [%list.A ] in ()
- : unit = ()
|}];;

run {| let _ = [%list.A 1] in () |}

[%%expect{|
let _ = [%list.A 1] in ()
- : unit = ()
|}];;

run {| let _ = [%list.A 1;2;3] in () |}

[%%expect{|
let _ = [%list.A 1; 2; 3] in ()
- : unit = ()
|}];;

run {| let _ = [%list.A 1;2;3;] in () |}

[%%expect{|
let _ = [%list.A 1; 2; 3] in ()
- : unit = ()
|}];;

(***********************************)
(* Record/variant disambiguation attribute printing *)

run {| let _ = { Stdlib.Float.Array.zero = 0.; one = 1. } in () |}

[%%expect{|
let _ = { Stdlib.Float.Array.zero = 0.; one = 1. } in ()
- : unit = ()
|}];;

(***********************************)
(* Labeled tuples *)

run {| (~x:1, ~y:2) |}

[%%expect{|
(~x:1, ~y:2)
- : unit = ()
|}];;

run {| type t = x:int * y:int |}

[%%expect{|
type t = x:int * y:int
- : unit = ()
|}];;

run {| let (type a b) : a:a * b:b -> a * b = function ~a, ~b -> a, b |}

[%%expect{|
let (type a) (type b) : a:a * b:b -> a * b =
  function | (~a, ~b) -> (a, b)
- : unit = ()
|}];;

run {| let (type a b) : a:a * b:b -> a * b = function | ~a, ~b -> a, b |}

[%%expect{|
let (type a) (type b) : a:a * b:b -> a * b =
  function | (~a, ~b) -> (a, b)
- : unit = ()
|}];;

run {| let (type a b) : a:a * b:b -> a * b = fun (~a, ~b) -> (a, b) |}

[%%expect{|
let (type a) (type b) : a:a * b:b -> a * b = fun (~a, ~b) -> (a, b)
- : unit = ()
|}];;

run {| type ('a, 'b) t = 'a * x:'b |}

[%%expect{|
type ('a, 'b) t = 'a * x:'b
- : unit = ()
|}];;

run {| let x : (?foo:int * string) = (?foo:3, "hello") |}

[%%expect{|
let x : ?foo:int * string = (?foo:3, "hello")
- : unit = ()
|}];;

run {| let x : (foo:int * string) = (~foo:3, "hello") |}

[%%expect{|
let x : foo:int * string = (~foo:3, "hello")
- : unit = ()
|}];;

run {| let x : (?foo:int * ?bar:string) = (?foo:3, ?bar:"hello") |}

[%%expect{|
let x : ?foo:int * ?bar:string = (?foo:3, ?bar:"hello")
- : unit = ()
|}];;

(***********************************)
(* Effects *)

run {| perform E |}

[%%expect{|
perform E
- : unit = ()
|}];;

run {| try e with effect E k -> continue k v |}

[%%expect{|
try e with effect E k -> continue k v
- : unit = ()
|}];;

run {| try e with effect E k -> ignore k; 42 |}

[%%expect{|
try e with effect E k -> (ignore k; 42)
- : unit = ()
|}];;

run {| match e with effect E k -> continue k v | x -> x |}

[%%expect{|
match e with effect E k -> continue k v | x -> x
- : unit = ()
|}];;

run {| match e with effect E k -> ignore k; 42 | x -> x |}

[%%expect{|
match e with effect E k -> (ignore k; 42) | x -> x
- : unit = ()
|}];;

run {|
  deep
    match e with
    | effect E _ , k -> ignore k; ()
    | x -> x
|}

[%%expect{|
deep
  match e with
  | effect E _, k -> (ignore k; ())
  | x -> x
- : unit = ()
|}];;

run {|
  shallow
    match e with
    | effect (E | F) x, k -> (ignore x; ignore k; ())
|}

[%%expect{|
shallow
  match e with
  | effect E x, k | effect F x, k -> (ignore x; ignore k; ())
- : unit = ()
|}];;

(***********************************)
(* Begin with attributes *)

run {| (begin%foo end[@bar]) |}

[%%expect{|
((begin%foo end)[@bar ])
- : unit = ()
|}];;

(***********************************)
(* List and array comprehensions *)

run {| [x for x = 1 to 10] |}

[%%expect{|
[x for x = 1 to 10]
- : unit = ()
|}];;

run {| [|x for x = 1 to 10|] |}

[%%expect{|
[|x for x = 1 to 10|]
- : unit = ()
|}];;

run {| [x for x = 1 to 10 when x > 2] |}

[%%expect{|
[x for x = 1 to 10 when x > 2]
- : unit = ()
|}];;

run {| [x for x in [1;2;3;4;5] when x > 2] |}

[%%expect{|
[x for x in [1; 2; 3; 4; 5] when x > 2]
- : unit = ()
|}];;

run {| [x + y for x in [1;2;3;4;5] for y in [1;2;3;4;5] when x > 2 && y < 3] |}

[%%expect{|
[x + y for x in [1; 2; 3; 4; 5] for y in [1; 2; 3; 4; 5] when x > 2 && y < 3]
- : unit = ()
|}];;

run {| [x and x + 1 for x = 1 to 10] |}

[%%expect{|
[x and (x + 1) for x = 1 to 10]
- : unit = ()
|}];;

run {| [x for x = 1 to 10 and y = 10 downto 1] |}

[%%expect{|
[x for x = 1 to 10 and y = 10 downto 1]
- : unit = ()
|}];;

run {| [x + y for x = 1 to 10 and x in l and y in l when x > 2 && y < 3] |}

[%%expect{|
[x + y for x = 1 to 10 and x in l and y in l when x > 2 && y < 3]
- : unit = ()
|}];;