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
they can be used to hold a closure.  More on this later.

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

Other types of identifiers include _code IDs_ and _closure IDs_, which will
be described later.

## Expressions

Expressions are formed using the following constructs.  The name as used in
the compiler code is given together with the form understood by the Flambda
parser (used for writing tests).

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

The `Let` construct binds variables or symbols:  XX and closure IDs
```
let v = <defining-expression> in <expression>
```
The allowable _defining expressions_ are very restrictive.  Only the
following are allowed, none of which have subexpressions and none of
which perform any control flow effect:
- Simples, as described above
- Primitive operations, as mentioned above and described more fully later
- Sets of closures (the representation of the closures of a group of
  possibly mutually-recursive functions; more on this later)
- Statically-allocated constants, described later
- Recursion depth annotations ("rec info"), likewise.

## Let_cont expressions


## Apply expressions


## Apply_cont expressions


## Switch expressions


## Invalid expressions

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
