external[@layout_poly] len : ('a : any) . 'a array -> int = "%array_length"

let length_scannable (x : #(int * float * string) array) = len x

let length_ignorable (x : #(float# * int * int64# * bool) array) = len x


external[@layout_poly] get : ('a : any) . 'a array -> int -> 'a =
  "%array_unsafe_get"

let unsafe_get_scannable (x : #(int * float * string) array) = get x 42

let unsafe_get_ignorable (x : #(float# * int * int64# * bool) array) = get x 42


external[@layout_poly] get : ('a : any) . 'a array -> int -> 'a =
  "%array_safe_get"

let safe_get_scannable (x : #(int * float * string) array) = get x 42

let safe_get_ignorable (x : #(float# * int * int64# * bool) array) = get x 42


external[@layout_poly] set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"

let unsafe_set_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let unsafe_set_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)


external[@layout_poly] set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_safe_set"

let safe_set_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let safe_set_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)
