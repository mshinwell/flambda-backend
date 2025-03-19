type record = #{ a : int; b : int64#; c : float# }

type t = record array
external array_get
  : ('a : any_non_null).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
  = "%array_unsafe_get"
[@@layout_poly]

let f (t : t) i = array_get t i
