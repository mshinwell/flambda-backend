# The Flambda 2 Language

## Table of contents

- [The Flambda 2 Language](#the-flambda-2-language)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Design principles](#design-principles)
    - [Every intermediate value has a name (ANF)](#every-intermediate-value-has-a-name-anf)
    - [Every control flow target has a name (CPS)](#every-control-flow-target-has-a-name-cps)
    - [No nested subexpressions](#no-nested-subexpressions)
    - [Relationship to SSA](#relationship-to-ssa)
  - [Names and values](#names-and-values)
    - [Variables](#variables)
    - [Symbols](#symbols)
    - [Names](#names)
    - [Simples](#simples)
  - [Expressions](#expressions)
    - [Let](#let)
    - [Let_cont](#let_cont)
    - [Apply](#apply)
    - [Apply_cont](#apply_cont)
    - [Switch](#switch)
    - [Invalid](#invalid)
  - [Defining expressions (Named)](#defining-expressions-named)
  - [Binding patterns](#binding-patterns)
  - [Continuations](#continuations)
    - [Second-class continuations](#second-class-continuations)
    - [Non-recursive continuations](#non-recursive-continuations)
    - [Recursive continuations](#recursive-continuations)
    - [Continuation handlers](#continuation-handlers)
  - [Functions](#functions)
    - [Function declarations and closures](#function-declarations-and-closures)
    - [Function body representation](#function-body-representation)
  - [Static constants](#static-constants)
  - [Name binding and scoping](#name-binding-and-scoping)
    - [Variable scoping](#variable-scoping)
    - [Symbol and code ID scoping](#symbol-and-code-id-scoping)
    - [Alpha-equivalence](#alpha-equivalence)

## Introduction

Flambda 2 is an intermediate representation (IR) for OCaml that combines
**Continuation-Passing Style (CPS)** for control flow with **A-Normal Form
(ANF)** for value bindings. This design provides a uniform representation where:

- Every intermediate value has a name
- Every control flow target has a name
- There are no nested subexpressions
- Inlining doesn't require re-normalization

Unlike some CPS representations where all binding is done via continuation
parameters, Flambda 2 retains conventional `let`-bindings. This reduces
syntactic overhead while preserving the benefits of CPS. The key restriction
is that `let`-bindings can only bind values—they cannot affect control flow.
Any operation that might branch or not return (function calls, matches) must
use continuations.

The language is defined in `terms/flambda.mli`.

## Design principles

### Every intermediate value has a name (ANF)

In traditional expression-based IRs, intermediate values can be nested:

```
(* Traditional expression form *)
f (g x) (h y)
```

In Flambda 2, every intermediate value must be bound to a variable:

```
let v1 = g x in
let v2 = h y in
f v1 v2
```

This ensures that every value that might be worth tracking, optimizing, or
statically allocating has a name that can be referenced in the type environment
and analysis data structures.

### Every control flow target has a name (CPS)

Similarly, every point where control flow might branch or merge has a
continuation name:

```
let_cont k x = ... in
let_cont j y = ... in
if condition then
  apply_cont k result1
else
  apply_cont j result2
```

This makes control flow explicit and provides names for join points that can
be referenced during optimization.

### No nested subexpressions

The combination of ANF for values and CPS for control flow means there are
no nested `let`s or subexpressions. A function body is a linear sequence of
bindings followed by a control flow operation:

```
let x1 = prim1 in
let x2 = prim2 in
let_cont k ... = ... in
apply_cont k x2
```

This structure has an important practical benefit: **inlining doesn't require
re-normalization**. When a function call is replaced with the function's body,
the body can be spliced directly into place without restructuring.

### Relationship to SSA

The combination of CPS and ANF makes Flambda 2 equivalent to **Static Single
Assignment (SSA)** form, but with a different representation of join points.

In traditional SSA, when control flow merges, **phi-nodes** select which
incoming value to use:

```
// Traditional SSA
block1:
  x1 = ...
  jump block3
block2:
  x2 = ...
  jump block3
block3:
  x3 = phi(x1, x2)   // phi-node selects x1 or x2
  use x3
```

In Flambda 2, the same pattern is expressed with continuations and `Apply_cont`:

```
let_cont block3 (x3) =
  use x3
in
...
  (* from block1: *) apply_cont block3 x1
  (* from block2: *) apply_cont block3 x2
```

The continuation parameter `x3` plays the role of the phi-node—it receives
different values depending on which `Apply_cont` transferred control. This
representation has some advantages:

- The values flowing into a join point are explicit at each call site
- No special phi-node construct is needed
- The scoping of the joined variable is clear (it's a parameter)

## Names and values

Flambda 2 has a hierarchy of name-like entities:

### Variables

Variables (`Variable.t`) are the most common form of name. They are:
- Bound by `Let` expressions or continuation parameters
- Subject to alpha-equivalence (can be renamed)
- Scoped syntactically

### Symbols

Symbols (`Symbol.t`) name statically-allocated values. They are:
- Bound by `Let` expressions with `Static` patterns
- **Not** subject to alpha-equivalence (globally unique)
- Scoped by dominator tree (see below)

### Names

A `Name.t` is the sum type `Variable.t | Symbol.t`. This is used when either
kind of name is acceptable, for example in typing environments.

### Simples

A `Simple.t` represents anything that fits in a register:

```
Simple = Variable | Symbol | Constant
```

Simples are the arguments to primitives and continuations. They don't need
to be `Let`-bound because they have no computational content—they just name
or are values that already exist.

Simples can also carry **coercions**, which are lightweight transformations
(like unboxing) that can be fused with uses.

## Expressions

The core expression type has six forms:

### Let

```ocaml
Let of let_expr
```

Binds one or more names to a defining expression. The defining expression
never affects control flow—it produces a value (or values) that the body
can use.

### Let_cont

```ocaml
Let_cont of let_cont_expr
```

Defines one or more continuations. Continuations may be non-recursive (a
single handler) or recursive (possibly mutually-recursive handlers, as
used for loops).

### Apply

```ocaml
Apply of Apply_expr.t
```

Calls an OCaml function, external function, or method. Function calls in
Flambda 2 are "double-barrelled": they specify both a return continuation
and an exception continuation.

### Apply_cont

```ocaml
Apply_cont of Apply_cont_expr.t
```

Jumps to a continuation with arguments. This is the only way for control to
flow to a continuation handler. An `Apply_cont` can also manipulate the
exception trap stack (pushing or popping handlers).

### Switch

```ocaml
Switch of Switch_expr.t
```

Conditional branch on an integer discriminant. Each arm specifies a
discriminant value and an `Apply_cont` to execute.

### Invalid

```ocaml
Invalid of { message : string }
```

Represents code proven unreachable through type analysis. This can occur
when a branch is impossible given the types, or when a primitive has
undefined behavior on certain inputs.

## Defining expressions (Named)

The right-hand side of a `Let` binding is a `Named.t`:

| Form | Description |
|------|-------------|
| `Simple of Simple.t` | A value that already exists (variable, symbol, constant) |
| `Prim of Flambda_primitive.t * Debuginfo.t` | A primitive operation |
| `Set_of_closures of Set_of_closures.t` | A dynamically-allocated closure block |
| `Static_consts of static_const_group` | Statically-allocated constants |
| `Rec_info of Rec_info_expr.t` | Recursion depth tracking for inlining |

Note that `Simple` is allowed for convenience even though it has no
computational content. This simplifies code generation and transformation.

## Binding patterns

A `Let` expression binds names according to a `Bound_pattern.t`:

| Pattern | What it binds | Scoping |
|---------|---------------|---------|
| `Singleton` | One variable | Syntactic |
| `Set_of_closures` | One variable per closure | Syntactic |
| `Static` | Symbols and code IDs | Dominator tree |

The `Singleton` pattern is the common case for primitives and simples. The
`Set_of_closures` pattern binds multiple variables, one for each function
in a closure block. The `Static` pattern binds symbols (for data) and code
IDs (for function code).

## Continuations

### Second-class continuations

Unlike some CPS representations, Flambda 2's continuations are **second-class**:

- Continuations cannot be stored in data structures
- Continuations cannot be passed as arguments to functions
- Continuations cannot be returned from functions

This means continuations are purely a control flow mechanism. They can be
thought of as labeled basic blocks with parameters.

### Non-recursive continuations

A non-recursive continuation is defined, used, and falls out of scope:

```
let_cont k (x, y) =
  <handler using x and y>
in
<body that may call k>
```

Non-recursive continuations represent join points where multiple control
flow paths merge. For example, the two branches of an `if` expression
might both jump to the same continuation.

### Recursive continuations

A recursive continuation can call itself:

```
let_cont rec loop (i, acc) =
  if i = 0 then
    apply_cont return acc
  else
    apply_cont loop (i - 1, acc + i)
in
apply_cont loop (n, 0)
```

Recursive continuations represent loops. Multiple mutually-recursive
continuations can be defined together for complex loop structures.

Recursive continuations can have **invariant parameters**—parameters that
have the same value at every call site. These are factored out to avoid
passing them repeatedly.

### Continuation handlers

A continuation handler (`Continuation_handler.t`) comprises:

- Bound parameters
- The handler expression
- Metadata (is it an exception handler? is it cold?)

Exception handlers are marked specially because they have different
compilation requirements in the backend.

## Functions

### Function declarations and closures

Functions are represented in two parts:

1. **Code** (`Code0.t`): The actual function implementation, including
   parameters, body, and metadata. Code is bound to **code IDs** which
   are globally unique identifiers.

2. **Closures** (`Set_of_closures.t`): Runtime closure blocks that pair
   code with captured values. A set of closures can contain multiple
   mutually-recursive functions sharing the same captured environment.

This separation allows the same code to be shared across multiple closure
allocations, and allows code to be analyzed independently of specific
closure instances.

### Function body representation

A function body (`Function_params_and_body.t`) provides:

- **Return continuation**: Where to jump with the result
- **Exception continuation**: Where to jump if an exception is raised
- **Parameters**: The function's formal parameters
- **Body**: The function's code
- **Special variables**:
  - `my_closure`: Reference to the closure itself (for accessing captured values)
  - `my_region`: The region for local allocations
  - `my_ghost_region`: For ghost region tracking
  - `my_depth`: Recursion depth for inlining decisions

## Static constants

Static constants (`Static_const.t`) represent values allocated at compile
time rather than runtime:

- Immutable blocks (tuples, records, variants)
- Boxed numbers (floats, int32, int64, etc.)
- Strings
- Closures (when all captured values are also static)

A `static_const_group` can define multiple constants together, which is
necessary for mutually-recursive closures.

## Name binding and scoping

### Variable scoping

Variables follow standard syntactic scoping:

```
let x = 1 in      (* x is bound here *)
let y = x + 1 in  (* x is in scope *)
y                 (* x is still in scope *)
```

A variable is in scope from its binding point to the end of the body of
the `Let` that binds it.

Continuation parameters are scoped over the handler body:

```
let_cont k (x, y) =  (* x, y are bound here *)
  x + y              (* x, y are in scope *)
in
...                  (* x, y are NOT in scope *)
```

### Symbol and code ID scoping

Symbols and code IDs follow **dominator-tree scoping**, not syntactic
scoping. A symbol is in scope everywhere that is dominated by its
definition point.

This is necessary because symbols represent globally-visible values that
may need to be accessed from anywhere in the dominated region, including
inside nested functions that are textually before the symbol definition.

### Alpha-equivalence

Flambda 2 terms are represented up to **alpha-equivalence** of bound
variables and continuations. This means:

- Two terms that differ only in the names of bound variables are considered equal
- Pattern-matching operations on terms provide fresh names for bound variables
- The `pattern_match` functions handle alpha-conversion automatically

Symbols and code IDs are **not** subject to alpha-equivalence—they are
globally unique identifiers that must match exactly.
