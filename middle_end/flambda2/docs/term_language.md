# The Flambda 2 term language

Flambda 2 can be thought of as a core calculus for OCaml.  A set of just six
constructs that form _expressions_ allow the basic structure and control flow of
a program to be represented.  These are augmented by _primitive_ operations
which do not perform control flow effects; some typical primitives are
arithmetic and memory operations.  The action of primitives is fine-grained:
they match up more with primitives in the `Cmm` language rather than those
in `Lambda`.  In particular, operations that might be thought to operate on
boxed values (for example floating-point addition) never do so: boxing is
always kept separate.  However tagging and untagging operations _are_
typically included in the action of primitives.

The term language provides the ability to represent both normal code, to be
executed at runtime, and statically-allocated entities (for example
floating-point constants).

The structure of the language draws heavily upon the ideas presented in
Kennedy's ICFP 2007 paper _Compiling with Continuations, Continued_.  Terms in
the language are structured using _second-class_ continuations with parameters.
Every point that may be jumped to in the program has a name; double-barreled
continuations are adopted to deal with exceptions raised to these points.
(There is no equivalent of block-structured `try...with` in Flambda 2.)
However classic "let"-bindings may be used without the need for continuations
when using primitive operations.  For such operations, as for expressions in
_A-normal form_, subexpressions are forbidden (one notable consequence being
that the term language is independent of evaluation order).

- good for inlining etc

- type system

## Identifiers and kinds

The term language has the usual notion of _variable_.  Variables (`Variable.t`
in the code) are always immutable.  They are lexically scoped in the normal way.
Each variable has a fixed _kind_ (`Flambda_kind.t` in the code) corresponding to
what values it may hold at runtime.  The most important kinds are:
- value (any OCaml value)
- naked (untagged) immediate (31 or 63 bits wide)
- naked (unboxed) float or integer (for the latter, 32 or 64 bits wide).

Certain of these kinds have subkinds (for example, the value kind has a subkind
saying that the value is always an tagged immediate).

Variables cannot be used to hold the name of a _piece of code_, although
they can be used to hold a closure value.  More on this later.

Variables cannot be used to bind items of statically-allocated data; instead,
_symbols_ must be used (`Symbol.t` in the code).  Symbols correspond 1:1 to
object file symbols.  Unusually, they are scoped based on the _dominator tree_
following their definition.  Symbols are always of value kind.

A _name_ (`Name.t` in the code) is taken to be either a variable or a symbol.
There is also the notion of _simple_ identifier (`Simple.t` in the code), which
is either a name, or a constant that fits in a register (for example, an integer
or unboxed floating-point constant, `Reg_width_const.t` in the code).  Simples
can be used in many places throughout the term language without the need to
let-bind them.

_Continuation identifiers_ (`Continuation.t` in the code) are the names of
continuations.

Other types of identifiers include _code IDs_, _closure IDs_ and
_closure variables_, which will be described later.

## Expressions

Expressions are formed using the following constructs.  The name as used in the
compiler code is given together with the form currently understood by the
Flambda parser (used for writing tests).

| In the code  | In the parser         | Description                       |
| ------------ | --------------------- | --------------------------------- |
| `Let`        | `let`                 | bind identifiers                  |
| `Let_cont`   | `where`               | define continuation               |
| `Apply`      | `apply`               | apply function, method or C call  |
| `Apply_cont` | `cont`                | unconditional jump                |
| `Switch`     | `switch`              | conditional jump table            |
| `Invalid`    | `halt_and_catch_fire` | invalid code: cause runtime trap, |
|              | _or_ `unreachable`    | or allow optimiser to delete code |

The `Let_cont` construct is used to define exception handlers, which are
just continuations marked in a special way.  `Apply_cont`, which accepts
instructions to adjust exception traps on the stack, is used to raise
an exception to these handlers.

In the grammar accepted by the parser, `Let_cont` is written in a postfix
manner, using the `where` keyword.  More on this later.

An `Apply_cont` expression to a continuation that takes no parameters is
known as a _goto_.

The only form of conditional control flow is the `Switch` expression which is a
map from untagged immediates to a restricted form of `Apply_cont` expressions.
There must be at least one destination in any given `Switch` expression.

## Let-expressions

The `Let` construct binds identifiers to _defining expressions_:
```
let <pattern> = <defining-expression> in <expression>
```
The allowable _defining expressions_ are very restrictive.  None of the
defining expressions have subexpressions and neither do they perform any
control flow effects (however see later section on asynchronous exceptions).

The form of patterns allowed varies depending on the defining expression
and is not statically enforced in the compiler code.  The various cases
are as follows:

- Simples, bound to variables.  This form only exists for convenience in
  the code, since all places in the term language where uses of variables may
  be found also accept simples.

- Primitive operations, with results bound to variables, described later.

- _Sets of closures_, which represent possibly mutually-recursive groups of
  zero or more closures and their (shared) environment.  There are two cases:
  closures allocated at runtime, bound to variables; and closures that
  are statically allocated, bound to symbols.  These forms of `Let`
  expressions also bind closure IDs, which identify each closure within a
  set of closures.  The handling of closures is described in more detail
  later.  It should be noted that sets of closures bindings do not bind
  the code of functions; they only deal with the closures.

- Statically-allocated constants, bound to symbols, described later.  One
  important case here is the binding of _pieces of code_ that form the
  bodies of functions and are referenced by closures.

- Recursion depth annotations ("rec info"), likewise.

### Primitive operations

Primitives have fixed parameter and return arities.  These arities specify
both the number of values being accepted or returned and their kinds.

There is a strict distinction between arrays and other kinds of block (e.g.
tuples and records).  Use of array primitives on values that were known to be
allocated as blocks, or vice-versa, will lead to `Invalid` being generated by
the simplifier.

At present a "floating-point number" is always taken to be a 64-bit IEEE float.

| Operations on arrays |
| -------------------- |
| Make_array           |
| Duplicate_array      |
| Array_length         |
| Array_load           |
| Array_set            |

| Operations on blocks |
| -------------------- |
| Make_block           |
| Duplicate_block      |
| Is_int               |
| Get_tag              |
| Block_load           |
| Block_set            |

| Bigarray_length |

| String_length |

| Int_as_pointer |
| Opaque_identity |


| Boxing and tagging | Description                                         |
| ------------------ | --------------------------------------------------- |
| Unbox_number       | Unbox a `float`, `int32`, `int64` or `nativeint`    |
|                    | or untag an `int`                                   |
| Box_number         | Allocate a `float`, `int32`, `int64` or `nativeint` |
|                    | or tag an untagged `int`                            |

| Operations on closures | Description                              |
| ---------------------- | ---------------------------------------- |
| Select_closure         | Given one closure in a set, find another |
| Project_var            | Read from a closure's environment        |

| String_or_bigstring_load |
| Bytes_or_bigstring_set |
| Bigarray_load |
| Bigarray_set |

| Phys_equal |

| Int_arith |
| Float_arith |
| Int_shift |
| Boolean_not |

| Numerical conversions      | Description                                    |
| -------------------------- | ---------------------------------------------- |
| Num_conv                   | Meaning-preserving conversion (as far as is possible) between kinds of unboxed, tagged and untagged numbers
| Reinterpret_int64_as_float | Turn an unboxed `int64` into an unboxed `float` without changing the bit pattern |

| Comparison | Description                                   |
| ---------- | --------------------------------------------- |
| Int_comp   | Comparisons on unboxed, tagged and untagged integers  |
| Float_comp | Comparisons on unboxed floating-point numbers |

The current OCaml runtime permits for those primitive operations that
allocate to raise so-called _asynchronous exceptions_.  These happen in
very specific circumstances such as:

- a signal handler raising an exception (including `Stack_overflow` and
  `Sys.Break`)
- a finaliser raising an exception
- an out-of-memory condition in the GC.

The intention is to design some kind of "superexception" path which allows
these asynchronous exceptions to bypass the normal exception raising
mechanism and be delivered to certain pre-registered handlers.  In the
meantime, when the compiler is configured in Flambda 2 mode, asynchronous
exceptions arising in an OCaml function will always be delivered to the
parent frame.  This is required because the exception handler inside the
current function may have a non-standard calling convention that is not known
to the GC.

### Sets of closures


## Let_cont expressions


## Apply expressions


## Apply_cont expressions


### Exception handling



## Switch expressions


## Invalid expressions

## Asynchronous exceptions


## Implementation notes

The terms of the language have an immutable representation in the compiler code
to reduce the risk of bugs.  Some performance optimisations have been made to
these representations, notably including the use of hash-consed integers for
the basic identifiers such as `Variable`, `Symbol`, `Name` and `Simple`.
The injection functions between these types do not allocate.  Patricia trees
are used as fast maps on these types.

Somewhat unusually, expressions are represented up to alpha equivalence,
using name permutations in the manner of nominal sets.  The application of
these permutations is done lazily to improve performance.  Terms are
automatically freshened upon examination (and this is enforced by
abstraction).

No manual freshening of names is necessary when terms are duplicated.  It is
permissible to construct terms where the same identifier (for example a
variable) occurs more than once in binding position (e.g. on the left-hand side
of a `Let` binding).  Such terms have the meaning that would be expected on
paper.

The code is written to support 32-bit target platforms but some aspects of
the 32-bit implementation are incomplete.  It is uncertain whether they will
ever be required.
