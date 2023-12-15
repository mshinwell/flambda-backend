(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper functions and values for Flambda 2 to Cmm translation. Functions in
    this module, unlike the ones in [Cmm_helpers], depend on Flambda 2 data
    types. *)

module C : sig
  (**************************************************************************)
  (*                                                                        *)
  (*                                 OCaml                                  *)
  (*                                                                        *)
  (*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
  (*                                                                        *)
  (*   Copyright 1996 Institut National de Recherche en Informatique et     *)
  (*     en Automatique.                                                    *)
  (*                                                                        *)
  (*   All rights reserved.  This file is distributed under the terms of    *)
  (*   the GNU Lesser General Public License version 2.1, with the          *)
  (*   special exception on linking described in the file LICENSE.          *)
  (*                                                                        *)
  (**************************************************************************)

  open Cmm

  type arity =
    { function_kind : Lambda.function_kind;
      params_layout : Lambda.layout list;
      return_layout : Lambda.layout
    }

  (** [bind name arg fn] is equivalent to [let name = arg in fn name], or simply
    [fn arg] if [arg] is simple enough *)
  val bind : string -> expression -> (expression -> expression) -> expression

  (** Same as [bind], but also treats loads from a variable as simple *)
  val bind_load :
    string -> expression -> (expression -> expression) -> expression

  (** Same as [bind], but does not treat variables as simple *)
  val bind_nonvar :
    string -> expression -> (expression -> expression) -> expression

  (** Headers *)

  (** A null header with GC bits set to black *)
  val caml_black : nativeint

  (** A constant equal to the tag for float arrays *)
  val floatarray_tag : Debuginfo.t -> expression

  (** [block_header tag size] creates a header with tag [tag] for a block of size
    [size] *)
  val block_header : int -> int -> nativeint

  (** Same as block_header, but with GC bits set to black *)
  val black_block_header : int -> int -> nativeint

  (** Closure headers of the given size *)
  val white_closure_header : int -> nativeint

  val black_closure_header : int -> nativeint

  (** Infix header at the given offset *)
  val infix_header : int -> nativeint

  (** Header for a boxed float value *)
  val float_header : nativeint

  (** Boxed integer headers *)
  val boxedint32_header : nativeint

  val boxedint64_header : nativeint

  val boxedintnat_header : nativeint

  (** Closure info for a closure of given arity and distance to environment *)
  val closure_info : arity:arity -> startenv:int -> is_last:bool -> nativeint

  val closure_info' :
    arity:Lambda.function_kind * 'a list ->
    startenv:int ->
    is_last:bool ->
    nativeint

  (** Wrappers *)
  val alloc_infix_header : int -> Debuginfo.t -> expression

  val alloc_closure_info :
    arity:arity -> startenv:int -> is_last:bool -> Debuginfo.t -> expression

  (** Integers *)

  (** Minimal/maximal OCaml integer values whose backend representation fits in a
    regular OCaml integer *)
  val max_repr_int : int

  val min_repr_int : int

  (** Make an integer constant from the given integer (tags the integer) *)
  val int_const : Debuginfo.t -> int -> expression

  val cint_const : int -> data_item

  val targetint_const : int -> Targetint.t

  (** Make a Cmm constant holding the given nativeint value. Uses [Cconst_int]
    instead of [Cconst_nativeint] when possible to preserve peephole
    optimisations. *)
  val natint_const_untagged : Debuginfo.t -> Nativeint.t -> expression

  (** Add an integer to the given expression *)
  val add_const : expression -> int -> Debuginfo.t -> expression

  (** Increment/decrement of integers *)
  val incr_int : expression -> Debuginfo.t -> expression

  val decr_int : expression -> Debuginfo.t -> expression

  (** Simplify the given expression knowing its last bit will be irrelevant *)
  val ignore_low_bit_int : expression -> expression

  (** Simplify the given expression knowing its first bit will be irrelevant *)
  val ignore_high_bit_int : expression -> expression

  (** Arithmetical operations on integers *)
  val add_int : expression -> expression -> Debuginfo.t -> expression

  val sub_int : expression -> expression -> Debuginfo.t -> expression

  val neg_int : expression -> Debuginfo.t -> expression

  val lsl_int : expression -> expression -> Debuginfo.t -> expression

  val mul_int : expression -> expression -> Debuginfo.t -> expression

  val lsr_int : expression -> expression -> Debuginfo.t -> expression

  val asr_int : expression -> expression -> Debuginfo.t -> expression

  val div_int :
    expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression

  val mod_int :
    expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression

  val and_int : expression -> expression -> Debuginfo.t -> expression

  val or_int : expression -> expression -> Debuginfo.t -> expression

  val xor_int : expression -> expression -> Debuginfo.t -> expression

  (** Integer tagging. [tag_int x = (x lsl 1) + 1] *)
  val tag_int : expression -> Debuginfo.t -> expression

  (** Integer untagging. [untag_int x = (x asr 1)] *)
  val untag_int : expression -> Debuginfo.t -> expression

  (** Specific division operations for boxed integers *)
  val safe_div_bi :
    Lambda.is_safe ->
    expression ->
    expression ->
    Primitive.boxed_integer ->
    Debuginfo.t ->
    expression

  val safe_mod_bi :
    Lambda.is_safe ->
    expression ->
    expression ->
    Primitive.boxed_integer ->
    Debuginfo.t ->
    expression

  (** Boolean negation *)
  val mk_not : Debuginfo.t -> expression -> expression

  (** Integer and float comparison that returns int not bool. The untagged
    versions do not tag the result and do not optimise known-constant cases. *)
  val mk_compare_ints : Debuginfo.t -> expression -> expression -> expression

  val mk_compare_floats : Debuginfo.t -> expression -> expression -> expression

  val mk_compare_ints_untagged :
    Debuginfo.t -> expression -> expression -> expression

  val mk_compare_floats_untagged :
    Debuginfo.t -> expression -> expression -> expression

  (** Loop construction (while true do expr done). Used to be represented as
    Cloop. *)
  val create_loop : expression -> Debuginfo.t -> expression

  (** Exception raising *)
  val raise_symbol : Debuginfo.t -> string -> expression

  (** Convert a tagged integer into a raw integer with boolean meaning *)
  val test_bool : Debuginfo.t -> expression -> expression

  (** Float boxing and unboxing *)
  val box_float : Debuginfo.t -> Lambda.alloc_mode -> expression -> expression

  val unbox_float : Debuginfo.t -> expression -> expression

  (** Vector boxing and unboxing *)
  val box_vec128 : Debuginfo.t -> Lambda.alloc_mode -> expression -> expression

  val unbox_vec128 : Debuginfo.t -> expression -> expression

  (** Complex number creation and access *)
  val box_complex : Debuginfo.t -> expression -> expression -> expression

  val complex_re : expression -> Debuginfo.t -> expression

  val complex_im : expression -> Debuginfo.t -> expression

  (** Make the given expression return a unit value *)
  val return_unit : Debuginfo.t -> expression -> expression

  (** Remove a trailing unit return if any *)
  val remove_unit : expression -> expression

  (** Blocks *)

  (** Non-atomic load of a mutable field *)
  val mk_load_mut : memory_chunk -> operation

  (** Atomic load. All atomic fields are mutable. *)
  val mk_load_atomic : memory_chunk -> operation

  (** [field_address ptr n dbg] returns an expression for the address of the [n]th
    field of the block pointed to by [ptr] *)
  val field_address : expression -> int -> Debuginfo.t -> expression

  (** [get_field_gen mut ptr n dbg] returns an expression for the access to the
    [n]th field of the block pointed to by [ptr].  The [memory_chunk] used is
    always [Word_val]. *)
  val get_field_gen :
    Asttypes.mutable_flag -> expression -> int -> Debuginfo.t -> expression

  (** Like [get_field_gen] but allows use of a different [memory_chunk]. *)
  val get_field_gen_given_memory_chunk :
    Cmm.memory_chunk ->
    Asttypes.mutable_flag ->
    expression ->
    int ->
    Debuginfo.t ->
    expression

  (** Get the field of the given [block] whose index is specified by the Cmm
    expresson [index] (in words). *)
  val get_field_computed :
    Lambda.immediate_or_pointer ->
    Asttypes.mutable_flag ->
    block:expression ->
    index:expression ->
    Debuginfo.t ->
    expression

  (** [set_field ptr n newval init dbg] returns an expression for setting the
    [n]th field of the block pointed to by [ptr] to [newval] *)
  val set_field :
    expression ->
    int ->
    expression ->
    initialization_or_assignment ->
    Debuginfo.t ->
    expression

  (** Load a block's header *)
  val get_header : expression -> Debuginfo.t -> expression

  (** Same as [get_header], but also clear all reserved bits of the result *)
  val get_header_masked : expression -> Debuginfo.t -> expression

  (** Load a block's tag *)
  val get_tag : expression -> Debuginfo.t -> expression

  (** Load a block's size *)
  val get_size : expression -> Debuginfo.t -> expression

  (** Arrays *)

  val wordsize_shift : int

  val numfloat_shift : int

  (** Check whether the given array is an array of regular OCaml values (as
    opposed to unboxed floats), from its header or pointer *)
  val is_addr_array_hdr : expression -> Debuginfo.t -> expression

  val is_addr_array_ptr : expression -> Debuginfo.t -> expression

  (** Get the length of an array from its header

    Shifts by one bit fewer than necessary, keeping one of the GC colour bits,
    to save an operation when returning the length as a caml integer or when
    comparing it to a caml integer.
    Assumes that the reserved bits are clear (see get_header_masked) *)
  val addr_array_length_shifted : expression -> Debuginfo.t -> expression

  val float_array_length_shifted : expression -> Debuginfo.t -> expression

  (** For [array_indexing ?typ log2size ptr ofs dbg] :

    Produces a pointer to the element of the array [ptr] on the position [ofs]
    with the given element [log2size] log2 element size. [ofs] is given as a
    tagged int expression.

    The optional ?typ argument is the C-- type of the result. By default, it is
    Addr, meaning we are constructing a derived pointer into the heap. If we
    know the pointer is outside the heap (this is the case for bigarray
    indexing), we give type Int instead. *)
  val array_indexing :
    ?typ:machtype_component ->
    int ->
    expression ->
    expression ->
    Debuginfo.t ->
    expression

  (** Array loads and stores

    [unboxed_float_array_ref] and [float_array_ref] differ in the boxing of the
    result; [float_array_set] takes an unboxed float *)
  val addr_array_ref : expression -> expression -> Debuginfo.t -> expression

  val int_array_ref : expression -> expression -> Debuginfo.t -> expression

  val unboxed_float_array_ref :
    expression -> expression -> Debuginfo.t -> expression

  val float_array_ref :
    Lambda.alloc_mode -> expression -> expression -> Debuginfo.t -> expression

  val addr_array_set_heap :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val addr_array_set_local :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val addr_array_initialize :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val addr_array_set :
    Lambda.modify_mode ->
    expression ->
    expression ->
    expression ->
    Debuginfo.t ->
    expression

  val int_array_set :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val float_array_set :
    expression -> expression -> expression -> Debuginfo.t -> expression

  (** Strings *)

  val string_length : expression -> Debuginfo.t -> expression

  val bigstring_length : expression -> Debuginfo.t -> expression

  val bigstring_get_alignment :
    expression -> expression -> int -> Debuginfo.t -> expression

  module Extended_machtype_component : sig
    (** Like [Cmm.machtype_component] but has a case explicitly for tagged
      integers.  This enables caml_apply functions to be insensitive to whether
      a particular argument or return value is a tagged integer or a normal
      value.  In turn this significantly reduces the number of caml_apply
      functions that are generated. *)
    type t =
      | Val
      | Addr
      | Tagged_int
      | Any_int
      | Float
      | Vec128
  end

  module Extended_machtype : sig
    type t = Extended_machtype_component.t array

    val typ_val : t

    val typ_tagged_int : t

    val typ_any_int : t

    val typ_int64 : t

    val typ_float : t

    val typ_void : t

    val typ_vec128 : t

    (** Conversion from a normal Cmm machtype. *)
    val of_machtype : machtype -> t

    (** Conversion from a Lambda layout. *)
    val of_layout : Lambda.layout -> t

    (** Conversion to a normal Cmm machtype. *)
    val to_machtype : t -> machtype

    (** Like [to_machtype] but tagged integer extended machtypes are mapped to
      value machtypes.  This is used to avoid excessive numbers of generic
      functions being generated (see comments in cmm_helpers.ml). *)
    val change_tagged_int_to_val : t -> machtype
  end

  (** Objects *)

  (** Lookup a method by its hash, using [caml_get_public_method]. Arguments:

    - obj : the object from which to lookup

    - tag : the hash of the method name, as a tagged integer *)
  val lookup_tag : expression -> expression -> Debuginfo.t -> expression

  (** Lookup a method by its offset in the method table. Arguments:

    - obj : the object from which to lookup

    - lab : the position of the required method in the object's method array, as
    a tagged integer *)
  val lookup_label : expression -> expression -> Debuginfo.t -> expression

  (** Allocations *)

  (** Allocate a block of regular values with the given tag *)
  val make_alloc :
    mode:Lambda.alloc_mode ->
    Debuginfo.t ->
    int ->
    expression list ->
    expression

  (** Allocate a block of unboxed floats with the given tag *)
  val make_float_alloc :
    mode:Lambda.alloc_mode ->
    Debuginfo.t ->
    int ->
    expression list ->
    expression

  (** Sys.opaque_identity *)
  val opaque : expression -> Debuginfo.t -> expression

  (** Generic application functions *)

  (** Get an identifier for a given machtype, used in the name of the
    generic functions. *)
  val machtype_identifier : machtype -> string

  (** Get the symbol for the generic currying or tuplifying wrapper with [n]
    arguments, and ensure its presence in the set of defined symbols. *)
  val curry_function_sym :
    Lambda.function_kind -> machtype list -> machtype -> Cmm.symbol

  (** Bigarrays *)

  (** Returns the size (in number of bytes) of a single element contained in a
    bigarray. *)
  val bigarray_elt_size_in_bytes : Lambda.bigarray_kind -> int

  (** Returns the memory chunk corresponding to the kind of elements stored in a
    bigarray. *)
  val bigarray_word_kind : Lambda.bigarray_kind -> memory_chunk

  (** [bigarray_get unsafe kind layout b args dbg]

    - unsafe : if true, do not insert bound checks

    - kind : see [Lambda.bigarray_kind]

    - layout : see [Lambda.bigarray_layout]

    - b : the bigarray to load from

    - args : a list of tagged integer expressions, corresponding to the indices
    in the respective dimensions

    - dbg : debugging information *)
  val bigarray_get :
    bool ->
    Lambda.bigarray_kind ->
    Lambda.bigarray_layout ->
    expression ->
    expression list ->
    Debuginfo.t ->
    expression

  (** [bigarray_set unsafe kind layout b args newval dbg]

    Same as [bigarray_get], with [newval] the value being assigned *)
  val bigarray_set :
    bool ->
    Lambda.bigarray_kind ->
    Lambda.bigarray_layout ->
    expression ->
    expression list ->
    expression ->
    Debuginfo.t ->
    expression

  (** Operations on 32-bit integers *)

  (** [low_32 _ x] is a value which agrees with x on at least the low 32 bits *)
  val low_32 : Debuginfo.t -> expression -> expression

  (** Sign extend from 32 bits to the word size *)
  val sign_extend_32 : Debuginfo.t -> expression -> expression

  (** Zero extend from 32 bits to the word size *)
  val zero_extend_32 : Debuginfo.t -> expression -> expression

  (** Operations on 63-bit integers. These may only be used for compilation to
    64-bit targets. *)

  (** [low_63 _ x] is a value which agrees with x on at least the low 63 bits *)
  val low_63 : Debuginfo.t -> expression -> expression

  (** Sign extend from 63 bits to the word size *)
  val sign_extend_63 : Debuginfo.t -> expression -> expression

  (** Zero extend from 63 bits to the word size *)
  val zero_extend_63 : Debuginfo.t -> expression -> expression

  (** Boxed numbers *)

  (** Global symbols for the ops field of boxed integers *)
  val caml_nativeint_ops : string

  val caml_int32_ops : string

  val caml_int64_ops : string

  (** Box a given integer, without sharing of constants *)
  val box_int_gen :
    Debuginfo.t ->
    Primitive.boxed_integer ->
    Lambda.alloc_mode ->
    expression ->
    expression

  (** Unbox a given boxed integer *)
  val unbox_int :
    Debuginfo.t -> Primitive.boxed_integer -> expression -> expression

  (** Used to prepare 32-bit integers on 64-bit platforms for a lsr operation *)
  val make_unsigned_int :
    Primitive.boxed_integer -> expression -> Debuginfo.t -> expression

  val unaligned_load_16 : expression -> expression -> Debuginfo.t -> expression

  val unaligned_set_16 :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val unaligned_load_32 : expression -> expression -> Debuginfo.t -> expression

  val unaligned_set_32 :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val unaligned_load_64 : expression -> expression -> Debuginfo.t -> expression

  val unaligned_set_64 :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val unaligned_load_128 : expression -> expression -> Debuginfo.t -> expression

  val unaligned_set_128 :
    expression -> expression -> expression -> Debuginfo.t -> expression

  val aligned_load_128 : expression -> expression -> Debuginfo.t -> expression

  val aligned_set_128 :
    expression -> expression -> expression -> Debuginfo.t -> expression

  (** Primitives *)

  type unary_primitive = expression -> Debuginfo.t -> expression

  (** Return the n-th field of a float array (or float-only record), as an unboxed
    float *)
  val floatfield : int -> unary_primitive

  (** Int_as_pointer primitive *)
  val int_as_pointer : unary_primitive

  (** Raise primitive *)
  val raise_prim : Lambda.raise_kind -> unary_primitive

  (** Unary negation of an OCaml integer *)
  val negint : unary_primitive

  (** Add a constant number to an OCaml integer *)
  val offsetint : int -> unary_primitive

  (** Add a constant number to an OCaml integer reference *)
  val offsetref : int -> unary_primitive

  (** Return the length of the array argument, as an OCaml integer *)
  val arraylength : Lambda.array_kind -> unary_primitive

  (** Byte swap primitive Operates on Cmm integers (unboxed values) *)
  val bbswap : Primitive.boxed_integer -> unary_primitive

  (** 16-bit byte swap primitive Operates on Cmm integers (untagged integers) *)
  val bswap16 : unary_primitive

  type binary_primitive = expression -> expression -> Debuginfo.t -> expression

  (** [setfield offset value_is_ptr init ptr value dbg] *)
  val setfield :
    int ->
    Lambda.immediate_or_pointer ->
    Lambda.initialization_or_assignment ->
    binary_primitive

  (** [setfloatfield offset init ptr value dbg]

    [value] is expected to be an unboxed floating point number *)
  val setfloatfield :
    int -> Lambda.initialization_or_assignment -> binary_primitive

  (** Operations on OCaml integers *)
  val add_int_caml : binary_primitive

  val sub_int_caml : binary_primitive

  val mul_int_caml : binary_primitive

  val div_int_caml : Lambda.is_safe -> binary_primitive

  val mod_int_caml : Lambda.is_safe -> binary_primitive

  val and_int_caml : binary_primitive

  val or_int_caml : binary_primitive

  val xor_int_caml : binary_primitive

  val lsl_int_caml : binary_primitive

  val lsr_int_caml : binary_primitive

  val asr_int_caml : binary_primitive

  val int_comp_caml : Lambda.integer_comparison -> binary_primitive

  type ternary_primitive =
    expression -> expression -> expression -> Debuginfo.t -> expression

  (** Same as setfield, except the offset is one of the arguments. Args: pointer
    (structure/array/...), index, value *)
  val setfield_computed :
    Lambda.immediate_or_pointer ->
    Lambda.initialization_or_assignment ->
    ternary_primitive

  (** Switch *)

  (** [transl_isout h arg dbg] *)
  val transl_isout : expression -> expression -> Debuginfo.t -> expression

  (** [make_switch arg cases actions dbg kind] :

    Generate a Cswitch construct, or optimize as a static table lookup when
    possible. *)
  val make_switch :
    expression ->
    int array ->
    (expression * Debuginfo.t) array ->
    Debuginfo.t ->
    Cmm.kind_for_unboxing ->
    expression

  (** [transl_int_switch loc kind arg low high cases default] *)
  val transl_int_switch :
    Debuginfo.t ->
    Cmm.kind_for_unboxing ->
    expression ->
    int ->
    int ->
    (int * expression) list ->
    expression ->
    expression

  (** [transl_switch_clambda loc kind arg index cases] *)
  val transl_switch_clambda :
    Debuginfo.t ->
    Cmm.kind_for_unboxing ->
    expression ->
    int array ->
    expression array ->
    expression

  (** [strmatch_compile dbg arg default cases] *)
  val strmatch_compile :
    Debuginfo.t ->
    Cmm.kind_for_unboxing ->
    expression ->
    expression option ->
    (string * expression) list ->
    expression

  (** Closures and function applications *)

  (** Adds a constant offset to a pointer (for infix access) *)
  val ptr_offset : expression -> int -> Debuginfo.t -> expression

  (** Method call : [send kind met obj args dbg]

    - [met] is a method identifier, which can be a hashed variant or an index in
    [obj]'s method table, depending on [kind]

    - [obj] is the object whose method is being called

    - [args] is the extra arguments to the method call (Note: I'm not aware of
    any way for the frontend to generate any arguments other than the cache and
    cache position) *)
  val send :
    Lambda.meth_kind ->
    expression ->
    expression ->
    expression list ->
    Extended_machtype.t list ->
    Extended_machtype.t ->
    Lambda.region_close * Lambda.alloc_mode ->
    Debuginfo.t ->
    expression

  (** Construct [Cregion e], eliding some useless regions *)
  val region : expression -> expression

  (** Generic Cmm fragments *)

  val placeholder_dbg : unit -> Debuginfo.t

  val placeholder_fun_dbg : human_name:string -> Debuginfo.t

  (** Entry point *)
  val entry_point : Compilation_unit.t list -> phrase list

  (** Generate the caml_globals table *)
  val global_table : Compilation_unit.t list -> phrase

  (** Add references to the given symbols *)
  val reference_symbols : symbol list -> phrase

  (** Generate the caml_globals_map structure, as a marshalled string constant.
    The runtime representation of the type here must match that of [type
    global_map] in the natdynlink code. *)
  val globals_map :
    (Compilation_unit.t * Digest.t option * Digest.t option * Symbol.t list)
    list ->
    phrase

  (** Generate the caml_frametable table, referencing the frametables from the
    given compilation units *)
  val frame_table : Compilation_unit.t list -> phrase

  (** Generate the tables for data and code positions respectively of the given
    compilation units *)
  val data_segment_table : Compilation_unit.t list -> phrase

  val code_segment_table : Compilation_unit.t list -> phrase

  (** Generate data for a predefined exception *)
  val predef_exception : int -> string -> phrase

  val plugin_header : Cmxs_format.dynunit list -> phrase

  (** Emit constant symbols *)

  (** Produce the data_item list corresponding to a symbol definition *)
  val cdefine_symbol : symbol -> data_item list

  (** [emit_block symb white_header cont] prepends to [cont] the header and symbol
    for the block. [cont] must already contain the fields of the block (and may
    contain additional data items afterwards). *)
  val emit_block : symbol -> nativeint -> data_item list -> data_item list

  (** Emit specific kinds of constant blocks as data items *)
  val emit_float_constant : symbol -> float -> data_item list -> data_item list

  val emit_string_constant :
    symbol -> string -> data_item list -> data_item list

  val emit_int32_constant : symbol -> int32 -> data_item list -> data_item list

  val emit_int64_constant : symbol -> int64 -> data_item list -> data_item list

  val emit_nativeint_constant :
    symbol -> nativeint -> data_item list -> data_item list

  val emit_vec128_constant :
    symbol -> Cmm.vec128_bits -> data_item list -> data_item list

  val emit_float_array_constant :
    symbol -> float list -> data_item list -> data_item list

  (** {1} Helper functions and values used by Flambda 2. *)

  (** An adequate Cmm machtype for an int64 (including on a 32-bit target). *)
  val typ_int64 : Cmm.machtype

  (* CR mshinwell: [dbg] should not be optional. *)

  (** The void (i.e. empty tuple) cmm value. Not to be confused with [() : unit]. *)
  val void : Cmm.expression

  (** Create the single unit value. *)
  val unit : dbg:Debuginfo.t -> Cmm.expression

  (** Create an expression from a variable. *)
  val var : Backend_var.t -> Cmm.expression

  (** Create an expression that gives the value of an object file symbol. *)
  val symbol : dbg:Debuginfo.t -> Cmm.symbol -> Cmm.expression

  (** Create a constant float expression. *)
  val float : dbg:Debuginfo.t -> float -> expression

  (** Create a constant int expression. *)
  val int : dbg:Debuginfo.t -> int -> expression

  (** Create a constant int expression from an int32. *)
  val int32 : dbg:Debuginfo.t -> int32 -> expression

  (** Create a constant int expression from an int64. *)
  val int64 : dbg:Debuginfo.t -> int64 -> expression

  (** Create a constant vec128 expression from two int64s. *)
  val vec128 : dbg:Debuginfo.t -> Cmm.vec128_bits -> expression

  (** Create a constant int expression from a nativeint. *)
  val nativeint : dbg:Debuginfo.t -> Nativeint.t -> expression

  (** Create a [Clet], except if the body just returns the bound variable, in
    which case the [Clet] is elided. *)
  val letin :
    Backend_var.With_provenance.t ->
    defining_expr:expression ->
    body:expression ->
    expression

  (** [letin_mut v ty e body] binds a mutable variable [v] of machtype [ty] to [e]
    in [body]. (For immutable variables, use [Cmm_helpers.letin].) *)
  val letin_mut :
    Backend_var.With_provenance.t ->
    machtype ->
    expression ->
    expression ->
    expression

  val assign : Backend_var.t -> expression -> expression

  (** Create a sequence of expressions. Will erase void expressions as needed. *)
  val sequence : expression -> expression -> expression

  (** Creates a conditional branching on the given condition. *)
  val ite :
    dbg:Debuginfo.t ->
    then_dbg:Debuginfo.t ->
    then_:expression ->
    else_dbg:Debuginfo.t ->
    else_:expression ->
    expression ->
    expression

  (** Create a try-with structure. The [exn_var] is the variable bound to the
    caught exception in the handler. *)
  val trywith :
    dbg:Debuginfo.t ->
    kind:trywith_kind ->
    body:expression ->
    exn_var:Backend_var.With_provenance.t ->
    handler:expression ->
    unit ->
    expression

  (** {2 Static jumps} *)

  (** Opaque type for static handlers. *)
  type static_handler

  (** [handler id vars body is_cold] creates a static handler for exit number [id],
    binding variables [vars] in [body]. *)
  val handler :
    dbg:Debuginfo.t ->
    Lambda.static_label ->
    (Backend_var.With_provenance.t * Cmm.machtype) list ->
    Cmm.expression ->
    bool ->
    static_handler

  (** [cexit id args] creates the cmm expression for static to a static handler
    with exit number [id], with arguments [args]. *)
  val cexit :
    Lambda.static_label ->
    Cmm.expression list ->
    Cmm.trap_action list ->
    Cmm.expression

  (** [trap_return res traps] creates the cmm expression for returning [res] after
    applying the trap actions in [traps]. *)
  val trap_return : Cmm.expression -> Cmm.trap_action list -> Cmm.expression

  (** Enclose a body with some static handlers. *)
  val create_ccatch :
    rec_flag:bool ->
    handlers:static_handler list ->
    body:Cmm.expression ->
    Cmm.expression

  val lsl_int_caml_raw :
    dbg:Debuginfo.t -> expression -> expression -> expression

  val lsr_int_caml_raw :
    dbg:Debuginfo.t -> expression -> expression -> expression

  (** Shift operations. take as first argument a tagged caml integer, and as
    second argument an untagged machine intger which is the amount to shift the
    first argument by. *)
  val asr_int_caml_raw :
    dbg:Debuginfo.t -> expression -> expression -> expression

  val int_of_float : dbg:Debuginfo.t -> expression -> expression

  (** Conversions functions between integers and floats. *)
  val float_of_int : dbg:Debuginfo.t -> expression -> expression

  val eq : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Integer arithmetic (dis)equality of cmm expressions. Returns an untagged
    integer (either 0 or 1) to represent the result of the comparison. *)
  val neq : dbg:Debuginfo.t -> expression -> expression -> expression

  val lt : dbg:Debuginfo.t -> expression -> expression -> expression

  val le : dbg:Debuginfo.t -> expression -> expression -> expression

  val gt : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Integer arithmetic signed comparisons on cmm expressions. Returns an
    untagged integer (either 0 or 1) to represent the result of the comparison. *)
  val ge : dbg:Debuginfo.t -> expression -> expression -> expression

  val ult : dbg:Debuginfo.t -> expression -> expression -> expression

  val ule : dbg:Debuginfo.t -> expression -> expression -> expression

  val ugt : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Integer arithmetic unsigned comparisons on cmm expressions. Returns an
    untagged integer (either 0 or 1) to represent the result of the comparison. *)
  val uge : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Asbolute value on floats. *)
  val float_abs : dbg:Debuginfo.t -> expression -> expression

  (** Arithmetic negation on floats. *)
  val float_neg : dbg:Debuginfo.t -> expression -> expression

  val float_add : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_sub : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_mul : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Float arithmetic operations. *)
  val float_div : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_eq : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Float arithmetic (dis)equality of cmm expressions. Returns an untagged
    integer (either 0 or 1) to represent the result of the comparison. *)
  val float_neq : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_lt : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_le : dbg:Debuginfo.t -> expression -> expression -> expression

  val float_gt : dbg:Debuginfo.t -> expression -> expression -> expression

  (** Float arithmetic comparisons on cmm expressions. Returns an untagged integer
    (either 0 or 1) to represent the result of the comparison. *)
  val float_ge : dbg:Debuginfo.t -> expression -> expression -> expression

  val beginregion : dbg:Debuginfo.t -> expression

  val endregion : dbg:Debuginfo.t -> expression -> expression

  val probe :
    dbg:Debuginfo.t ->
    name:string ->
    handler_code_linkage_name:string ->
    enabled_at_init:bool ->
    args:expression list ->
    expression

  val load :
    dbg:Debuginfo.t ->
    memory_chunk ->
    Asttypes.mutable_flag ->
    addr:expression ->
    expression

  val store :
    dbg:Debuginfo.t ->
    memory_chunk ->
    initialization_or_assignment ->
    addr:expression ->
    new_value:expression ->
    expression

  (** [direct_call ty f_code args] creates a direct call to the function code
    [f_code] with arguments [args], with a return value of type [ty].

    If a closure needs to be passed, it must be included in [args]. *)
  val direct_call :
    dbg:Debuginfo.t ->
    machtype ->
    Lambda.region_close ->
    expression ->
    expression list ->
    expression

  (** Same as {!direct_call} but for an indirect call. *)
  val indirect_call :
    dbg:Debuginfo.t ->
    Extended_machtype.t ->
    Lambda.region_close ->
    Lambda.alloc_mode ->
    expression ->
    Extended_machtype.t list ->
    expression list ->
    expression

  (** Same as {!direct_call} but for an indirect call that is know to be a full
    application (since this enables a few optimisations). *)
  val indirect_full_call :
    dbg:Debuginfo.t ->
    Extended_machtype.t ->
    Lambda.region_close ->
    Lambda.alloc_mode ->
    expression ->
    Extended_machtype.t list ->
    expression list ->
    expression

  val bigarray_load :
    dbg:Debuginfo.t ->
    elt_kind:Lambda.bigarray_kind ->
    elt_size:int ->
    elt_chunk:memory_chunk ->
    bigarray:expression ->
    index:expression ->
    expression

  val bigarray_store :
    dbg:Debuginfo.t ->
    elt_kind:Lambda.bigarray_kind ->
    elt_size:int ->
    elt_chunk:memory_chunk ->
    bigarray:expression ->
    index:expression ->
    new_value:expression ->
    expression

  (** [infix_field_address ptr n dbg] returns an expression for the address of the
    [n]-th field of the set of closures block pointed to by [ptr]. This function
    assumes that the [n-1]-th field of the block is an infix header, so that the
    returned address is in fact a correct ocaml value. *)
  val infix_field_address : dbg:Debuginfo.t -> expression -> int -> expression

  (** {2 Data items} *)

  (** Static integer. *)
  val cint : nativeint -> data_item

  (** Static float. *)
  val cfloat : float -> data_item

  (** Static 128-bit vector. *)
  val cvec128 : Cmm.vec128_bits -> data_item

  (** Static symbol. *)
  val symbol_address : symbol -> data_item

  (** Definition for a static symbol. *)
  val define_symbol : symbol -> data_item list

  (** {2 Static structure helpers} *)

  (** [fundecl name args body codegen_options dbg] creates a cmm function
    declaration for a function [name] with binding [args] over [body]. *)
  val fundecl :
    symbol ->
    (Backend_var.With_provenance.t * machtype) list ->
    expression ->
    codegen_option list ->
    Debuginfo.t ->
    Lambda.poll_attribute ->
    fundecl

  (** Create a cmm phrase for a function declaration. *)
  val cfunction : fundecl -> phrase

  (** Create a cmm phrase for a static data item. *)
  val cdata : data_item list -> phrase

  (** Create the gc root table from a list of root symbols. *)
  val gc_root_table : Cmm.symbol list -> phrase

  (* An estimate of the number of arithmetic instructions in a Cmm expression.
     This is currently used in Flambda 2 to determine whether untagging an
     expression resulted in a smaller expression or not (as can happen because
     of some arithmetic simplifications performed by functions in this file).

     If [None] is returned, that means "no estimate available". The expression
     should be assumed to be potentially large. *)
  val cmm_arith_size : expression -> int option

  val transl_attrib : Lambda.check_attribute -> Cmm.codegen_option list

  (* CR lmaurer: Return [Linkage_name.t] instead *)
  val make_symbol : ?compilation_unit:Compilation_unit.t -> string -> string

  val kind_of_layout : Lambda.layout -> kind_for_unboxing

  val machtype_of_layout : Lambda.layout -> machtype

  val machtype_of_layout_changing_tagged_int_to_val : Lambda.layout -> machtype

  val make_tuple : expression list -> expression

  (* Generated functions *)
  val curry_function :
    Lambda.function_kind * Cmm.machtype list * Cmm.machtype -> Cmm.phrase list

  val send_function :
    Cmm.machtype list * Cmm.machtype * Lambda.alloc_mode -> Cmm.phrase

  val apply_function :
    Cmm.machtype list * Cmm.machtype * Lambda.alloc_mode -> Cmm.phrase

  (* Atomics *)

  val atomic_load :
    dbg:Debuginfo.t -> Lambda.immediate_or_pointer -> expression -> expression

  val atomic_exchange :
    dbg:Debuginfo.t -> expression -> expression -> expression

  val atomic_fetch_and_add :
    dbg:Debuginfo.t -> expression -> expression -> expression

  val atomic_compare_and_set :
    dbg:Debuginfo.t ->
    expression ->
    old_value:expression ->
    new_value:expression ->
    expression

  val emit_gc_roots_table : symbols:symbol list -> phrase list -> phrase list

  include module type of struct
    include Cmm_builtins
  end
end

val remove_var_with_provenance :
  To_cmm_env.free_vars -> Backend_var.With_provenance.t -> To_cmm_env.free_vars

val remove_vars_with_machtype :
  To_cmm_env.free_vars ->
  (Backend_var.With_provenance.t * Cmm.machtype) list ->
  To_cmm_env.free_vars

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

val machtype_of_kind : Flambda_kind.With_subkind.t -> Cmm.machtype

val extended_machtype_of_kind :
  Flambda_kind.With_subkind.t -> C.Extended_machtype.t

val machtype_of_kinded_parameter : Bound_parameter.t -> Cmm.machtype

val memory_chunk_of_kind : Flambda_kind.With_subkind.t -> Cmm.memory_chunk

(** Create a constant int expression from a targetint. *)
val targetint : dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

(** This does not inline effectful expressions. *)
val name :
  To_cmm_env.t -> To_cmm_result.t -> Name.t -> To_cmm_env.translation_result

val const : dbg:Debuginfo.t -> Reg_width_const.t -> Cmm.expression

(** The default behaviour is to use
    [Flambda_features.Expert.inline_effects_in_cmm], which defaults to [false]
    if no command-line flag is given. *)
val simple :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t ->
  To_cmm_env.translation_result

val simple_static :
  To_cmm_result.t ->
  Simple.t ->
  [`Data of Cmm.data_item list | `Var of Variable.t]

(** This function translates the [Simple] at the head of the list first.
    Regarding [consider_inlining_effectful_expressions], see [simple] above. *)
val simple_list :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t list ->
  Cmm.expression list
  * To_cmm_env.free_vars
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Cmm.memory_chunk ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:To_cmm_env.expr_with_info option ->
  To_cmm_env.t * To_cmm_result.t * To_cmm_env.expr_with_info option

val check_arity : _ Flambda_arity.t -> _ list -> bool

val extended_machtype_of_return_arity :
  [`Unarized] Flambda_arity.t -> C.Extended_machtype.t
