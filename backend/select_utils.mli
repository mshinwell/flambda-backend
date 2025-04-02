module Int : sig end

module V = Backend_var

module VP : sig end

type basic_or_terminator =
  | Basic of 'a
  | Terminator of 'b

type is_immediate_result =
  | Is_immediate of bool
  | Use_default

type is_simple_expr_result =
  | Simple_if_all_expressions_are of 'a list
  | Use_default

type effects_of_result =
  | Effects_of_all_expressions of 'a list
  | Use_default

type select_operation_then_rewrite_result =
  | Rewritten of basic_or_terminator * 'a list
  | Use_default

type select_operation_result =
  | Rewritten of basic_or_terminator * 'a list
  | Select_operation_then_rewrite of
      'b
      * 'c list
      * 'd
      * (basic_or_terminator ->
        args:'e list ->
        select_operation_then_rewrite_result)
  | Use_default

type select_store_result =
  | Maybe_out_of_range
  | Rewritten of 'a * 'b
  | Use_default

type is_store_out_of_range_result =
  | Within_range
  | Out_of_range

type insert_move_extcall_arg_result =
  | Rewritten of 'a * 'b array * 'c array
  | Use_default

type trap_stack_info =
  | Unreachable
  | Reachable of 'a

type static_handler =
  { regs : 'a array list;
    traps_ref : trap_stack_info ref;
    extra : 'b
  }

type environment =
  { vars : 'a;
    static_exceptions : 'b;
    trap_stack : 'c;
    regs_for_exception_extra_args : 'd
  }

val env_add : ?mut:'a -> 'b -> 'c -> environment -> environment

val env_add_static_exception :
  'a -> 'b array list -> environment -> 'c -> environment * trap_stack_info ref

val env_find : 'a -> environment -> 'b

val env_find_mut : 'a -> environment -> 'b * 'c

val env_add_regs_for_exception_extra_args :
  'a -> 'b -> environment -> environment

val env_find_regs_for_exception_extra_args : 'a -> environment -> 'b

val _env_find_with_provenance : 'a -> environment -> 'b

val env_find_static_exception : 'a -> environment -> 'b

val env_enter_trywith : environment -> 'a -> 'b -> environment

val env_set_trap_stack : environment -> 'a -> environment

val combine_traps : 'a -> 'b list -> 'c

val print_traps : Format.formatter -> 'a -> unit

val set_traps : 'a -> trap_stack_info ref -> 'b -> 'c list -> unit

val set_traps_for_raise : environment -> 'a

val trap_stack_is_empty : environment -> 'a

val pop_all_traps : environment -> 'a

val env_empty : environment

val select_mutable_flag : 'a -> 'b

val oper_result_type : 'a -> 'b

val size_component : 'a -> int

val size_machtype : 'a array -> int

val size_expr : environment -> 'a -> 'b

val swap_intcomp : 'a -> 'b

val all_regs_anonymous : 'a array -> bool

val name_regs : 'a -> 'b array -> unit

val current_function_name : string ref

val current_function_is_check_enabled : bool ref

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary

  val join : t -> t -> t

  val pure : t -> bool
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary

  val join : t -> t -> t

  val copure : t -> bool
end

module Effect_and_coeffect : sig
  type t

  val none : t

  val arbitrary : t

  val effect : t -> Effect.t

  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t

  val coeffect_only : Coeffect.t -> t

  val create : Effect.t -> Coeffect.t -> t

  val join : t -> t -> t

  val join_list_map : 'a list -> ('a -> t) -> t
end

val select_effects : 'a -> Effect.t

val select_coeffects : 'a -> Coeffect.t

module Or_never_returns : sig
  type 'a t =
    | Ok of 'a
    | Never_returns
end

val debug : bool

val float_test_of_float_comparison :
  'a -> 'b -> label_false:'c -> label_true:'d -> 'e

val int_test_of_integer_comparison :
  'a ->
  signed:bool ->
  immediate:int option ->
  label_false:'b ->
  label_true:'c ->
  'd

val terminator_of_test : 'a -> label_false:'b -> label_true:'c -> 'd

val invalid_stack_offset : int

module Stack_offset_and_exn : sig
  type handler_stack = 'a list

  val compute_stack_offset : stack_offset:int -> traps:'a list -> int

  val check_and_set_stack_offset :
    'a -> stack_offset:int -> traps:handler_stack -> unit

  val process_terminator :
    stack_offset:int -> traps:handler_stack -> 'a -> int * handler_stack

  val process_basic :
    'a -> stack_offset:int -> traps:handler_stack -> 'b -> int * handler_stack

  val update_block : 'a -> 'b -> stack_offset:int -> traps:handler_stack -> unit

  val update_cfg : 'a -> unit
end

val make_stack_offset : 'a -> 'b

val make_name_for_debugger :
  ident:'a ->
  which_parameter:'b ->
  provenance:'c ->
  is_assignment:'d ->
  regs:'e ->
  'f

val make_const_int : 'a -> 'b

val make_const_float32 : 'a -> 'b

val make_const_float : 'a -> 'b

val make_const_vec128 : 'a -> 'b

val make_const_symbol : 'a -> 'b

val make_opaque : unit -> 'a

val regs_for : 'a -> 'b

val basic_op : 'a -> basic_or_terminator

val insert_debug : environment -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f

val insert_op_debug_returning_id :
  environment -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f

val insert : environment -> 'a -> 'b -> 'c -> 'd -> 'e

val insert' : environment -> 'a -> 'b -> 'c -> 'd -> 'e

val insert_debug' : environment -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f

val insert_op_debug' : environment -> 'a -> 'b -> 'c -> 'd -> 'e -> 'e

val insert_move : environment -> 'a -> 'b -> 'c -> unit

val insert_moves : environment -> 'a -> 'b array -> 'c array -> unit

val insert_move_args : environment -> 'a -> 'b array -> 'c array -> int -> unit

val insert_move_results :
  environment -> 'a -> 'b array -> 'c array -> int -> unit

val insert_op_debug : environment -> 'a -> 'b -> 'c -> 'd -> 'e -> 'e

val insert_op : environment -> 'a -> 'b -> 'c -> 'd -> 'd

val maybe_emit_naming_op :
  environment -> 'a -> bound_name:'b option -> 'c -> unit

val join :
  environment ->
  ('a array * 'b) Or_never_returns.t ->
  ('a array * 'b) Or_never_returns.t ->
  bound_name:'c option ->
  ('a array * 'b * 'b) Or_never_returns.t

val join_array :
  environment ->
  ('a array * 'b) Or_never_returns.t array ->
  bound_name:'c option ->
  ('d array * 'b array) Or_never_returns.t
