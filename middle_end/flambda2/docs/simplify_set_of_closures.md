# Simplification of sets of closures

## Table of contents

- [Simplification of sets of closures](#simplification-of-sets-of-closures)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Optimizations overview](#optimizations-overview)
  - [Background: sets of closures](#background-sets-of-closures)
    - [Function slots and value slots](#function-slots-and-value-slots)
  - [Entry points](#entry-points)
  - [Simplifying function bodies](#simplifying-function-bodies)
    - [Setting up the environment](#setting-up-the-environment)
    - [Re-simplification](#re-simplification)
  - [Code generation](#code-generation)
    - [Code ID versioning](#code-id-versioning)
    - [What the new code contains](#what-the-new-code-contains)
  - [Lifting sets of closures](#lifting-sets-of-closures)
    - [What is lifting?](#what-is-lifting)
    - [Lifting eligibility](#lifting-eligibility)
    - [Symbol projections](#symbol-projections)
    - [The lifting transformation](#the-lifting-transformation)
  - [Context module](#context-module)
  - [Type propagation](#type-propagation)

## Introduction

The `Simplify_set_of_closures` module handles simplification of function
definitions in Flambda 2. When a set of closures is simplified, **new code
is generated** for each function - the original `Code.t` values are not
modified. Instead, new `Code.t` values are created with simplified bodies,
potentially new code IDs, and updated metadata such as cost metrics and
inlining decisions.

Function bodies are not simplified earlier in the pipeline because much more
information is available at the point where the set of closures is defined.
For example, the types of captured variables in value slots are known, and
the mutual recursion structure is visible.

**Special case: stub functions.** Stub functions are simplified once when first
encountered, not when their set of closures is simplified. This is because
stub functions are small wrappers (e.g., for tupled functions) that are
unlikely to benefit from the additional context available at the set of
closures definition. Simplifying them early also avoids potential performance
issues in pathological cases where many sets of closures reference the same
stub.

The `Simplify_set_of_closures` module is responsible for:
- Simplifying function bodies and generating new `Code.t` values
- Deciding whether a set of closures can be "lifted" to static allocation
- Computing types for closures and their captured variables
- Managing the context needed when simplifying mutually recursive functions

## Optimizations overview

Several optimizations occur when simplifying a set of closures:

**Lifting to static allocation.** If a set of closures captures only values
that are available at compile time (constants, symbols, or variables known to
equal symbols), the entire set can be "lifted" to become a statically-allocated
constant. This eliminates runtime allocation and enables the closures to be
referenced by symbol.

**Value slot simplification.** The contents of value slots (captured variables)
are simplified using Flambda 2's type system. This can:
- Replace a variable with a constant if the variable's type indicates it's
  always that constant
- Replace a variable with a symbol if the variable is known to be an alias
  for a symbol (e.g., from a previous lifting or from module-level bindings)
- Track "symbol projections" - if a variable is known to be a projection
  from a symbol (e.g., a field of a block stored in a symbol), this
  information is preserved and can enable lifting even when the variable
  itself isn't directly a symbol (see [Symbol projections](#symbol-projections)
  below)

**Function body simplification.** Each function body is simplified with full
access to the typing environment. This enables standard optimizations like
inlining, constant propagation, and dead code elimination. The types of
captured variables are available, which can enable optimizations that depend
on knowing what values are captured.

**Closure type computation.** After simplification, precise types are computed
for each closure. These types record which function is in each function slot
and what values are in each value slot. This information propagates to call
sites, enabling optimizations like converting indirect calls to direct calls.

## Background: sets of closures

In Flambda 2, closures are grouped into **sets of closures**. A set of closures
contains:
- One or more **function declarations** (possibly mutually recursive)
- A shared **closure environment** containing captured variables

For example, consider this OCaml code:

```ocaml
let f x =
  let captured = x + 1 in
  let rec even n = if n = 0 then true else odd (n - 1)
  and odd n = if n = 0 then false else even (n - 1) in
  (even, odd)
```

The `even` and `odd` functions form a single set of closures that shares
the captured variable `captured`.

### Function slots and value slots

A set of closures has two kinds of "slots":

- **Function slots**: Each function in the set occupies a function slot. The
  function slot is used to identify which closure in the set is being called
  or accessed.

- **Value slots**: The shared closure environment is stored in value slots.
  Each captured variable occupies a value slot. All functions in the set can
  access all value slots.

The set of closures is represented in memory as a block where some fields
contain code pointers (for function slots) and others contain captured values
(for value slots).

## Entry points

The module provides three main entry points:

**`simplify_non_lifted_set_of_closures`**: Simplifies a set of closures that
appears in a `Let` binding. This is the common case - the function decides
whether to:
- Keep the set of closures as a runtime allocation, or
- "Lift" it to become a static constant

**`simplify_lifted_sets_of_closures`**: Simplifies sets of closures that are
already in `Let_symbol` bindings (static constants). These can be mutually
recursive across multiple sets.

**`simplify_stub_function`**: Simplifies a single stub function (see the
exception noted above in the introduction).

## Simplifying function bodies

### Setting up the environment

Before simplifying a function body, the environment must be set up with:

1. **Parameters**: Function parameters are added with unknown types (since
   the function may be called from many places with different arguments).

2. **Special variables**:
   - `my_closure`: A variable referring to the closure itself
   - `my_region`: The region for local allocations (if present)
   - `my_ghost_region`: For ghost region tracking
   - `my_depth`: Tracks recursion depth for recursive functions

3. **Continuation scope**: The scope is incremented so that types from inside
   the function don't leak outside.

4. **Loopify state**: If the function has the `[@loop]` attribute, a loopify
   continuation is set up for self-tail-call optimization.

### Re-simplification

After simplifying a function body, the simplifier may detect that additional
simplification would be beneficial - for example, if new constant propagation
opportunities were discovered. In this case, the function can be simplified
again, up to a configurable maximum number of iterations
(`flambda2-expert-max-function-simplify-run`).

The decision to re-simplify is based on the `resimplify` flag in the upwards
accumulator, which is set when potentially beneficial simplifications were
blocked by lack of information that is now available.

## Code generation

When a function in a set of closures is simplified, the result is a completely
new `Code.t` value. The original code is never modified - Flambda 2's
simplification is purely functional in this regard.

### Code ID versioning

Each `Code.t` has a `Code_id.t` that uniquely identifies it. When simplification
produces new code, it may either:

1. **Reuse the same code ID**: If this is the first simplification of the code
   and no new version is needed.

2. **Create a new code ID**: If the code has been simplified before (e.g., from
   a previous round), a new code ID is created. The new code records its
   `newer_version_of` relationship to the old code ID, establishing a version
   chain.

The version chain is tracked in the **code age relation**, which is part of the
typing environment. This allows the simplifier to know when one piece of code
is a more recent version of another.

### What the new code contains

The generated `Code.t` includes:

- **Simplified body**: The function body after simplification, with inlining,
  constant propagation, and other optimizations applied.

- **Updated metadata**:
  - Cost metrics (code size, removed operations)
  - Inlining decision (whether this function should be inlined at call sites)
  - Result types (if function result type inference is enabled)
  - Recursion information (whether the function is recursive)
  - Whether `my_closure` is actually used in the body

- **Preserved attributes**: Properties like `inline`, `poll`, `zero_alloc`,
  and allocation modes are carried over from the original code.

The new code is added to the **lifted constants accumulator** so it will
appear in the final program's code section.

## Lifting sets of closures

### What is lifting?

**Lifting** transforms a dynamically-allocated set of closures into a
statically-allocated constant. This is desirable because:

1. **No runtime allocation**: The closures exist in the data section rather
   than being allocated at runtime.

2. **Better optimization**: Static closures can be referenced by symbol,
   enabling cross-module optimization.

3. **Potential code sharing**: Multiple uses of the same static closure
   share the same memory.

Before lifting:
```
let f = fun x -> x + captured in  (* allocates closure at runtime *)
...
```

After lifting:
```
(* In static constants: *)
let symbol_f = <closure for f with captured = some_constant>

(* In code: *)
let f = symbol_f in  (* just a reference to the symbol *)
...
```

### Lifting eligibility

A set of closures can be lifted only if all value slot contents permit lifting.
A value slot permits lifting if it contains:

- A constant
- A symbol
- A variable that is either:
  - **Defined at toplevel** and satisfies the allocation mode constraints
    (see below), OR
  - **A symbol projection** - i.e., known to be equal to a field of a symbol
    (the projection can be rematerialized at the static allocation site)

**What "defined at toplevel" means.** A variable is defined at toplevel if it
was bound in a position that postdominates the module entry point. This means:
- It is at module level (not inside a function body), AND
- It is not under any conditional branch

The postdomination requirement ensures the binding will definitely be executed
before any code that uses the lifted constant. A variable bound inside an
`if` branch, for example, might not be defined when the static constant is
initialized, so it cannot be used for lifting.

**Allocation mode constraints.** When the set of closures has `Local`
allocation mode, there's an additional requirement: the variable must be
proven to never hold locally-allocated values. This is because static data
(which lives forever) cannot safely point to locally-allocated blocks (which
may be deallocated when the region ends).

Additionally, the binding must be in normal name mode (not phantom).

The key insight is that simplifying function bodies doesn't change lifting
eligibility - only the value slots matter. This allows the lifting decision
to be made before simplifying the function bodies.

### Symbol projections

Symbol projections are a mechanism that enables more closures to be lifted
to static allocation. A **symbol projection** records that a variable is
known to equal a projection (such as a block field access) from a symbol.

Consider this example:

```ocaml
let config = (true, 42)   (* allocated as a symbol at module level *)

let f x =
  let (flag, _) = config in   (* flag = field 0 of config's symbol *)
  let g y = if flag then x + y else y in
  g
```

Here's what happens:
1. `config` is bound at module level, so it becomes a symbol (say, `Config`)
2. Inside `f`, the variable `flag` is bound to `fst config`, which is
   field 0 of the symbol `Config`
3. The closure `g` captures `flag`
4. Even though `flag` is a variable (not a symbol), the simplifier tracks
   that `flag` equals `Block_load(Config, 0)` - a symbol projection

When deciding whether `g` can be lifted:
- `flag` is not a symbol, so normally it would prevent lifting
- But `flag` is a symbol projection, so lifting is still possible
- The lifted closure can reference the projection directly:
  `let g_symbol = <closure capturing Block_load(Config, 0)>`

This is important because many OCaml programs define configuration or
constants at module level as tuples or records, then destructure them
inside functions. Without symbol projection tracking, closures capturing
these destructured values couldn't be lifted.

The simplifier tracks symbol projections for:
- `Block_load` (accessing a field of a block)
- `Project_value_slot` (accessing a value slot from a closure)

### The lifting transformation

When a set of closures is lifted:

1. **Symbols are created**: Each function slot gets a symbol name.

2. **The set becomes a static constant**: It's added to the lifted constants
   accumulator.

3. **Bindings are updated**: Variables that were bound to closures now become
   aliases for the corresponding symbols.

4. **Value slots may reference symbols**: If a value slot contained a variable
   that was bound to another closure in the same set, that reference is
   updated to use the symbol instead.

## Context module

The `Simplify_set_of_closures_context` module manages information that is
shared across all functions being simplified in a recursive group:

- **`dacc_prior_to_sets`**: The downwards accumulator before any function
  simplification began.

- **`dacc_inside_functions`**: An environment set up for simplifying function
  bodies, with bindings for all closures in the recursive group.

- **`old_to_new_code_ids_all_sets`**: Maps old code IDs to new ones when
  code is being versioned.

- **`closure_bound_names_inside_functions`**: The names (variables or symbols)
  bound to each closure, as seen from inside the functions.

The context ensures that all functions in a mutually-recursive group can
"see" each other's closures with proper types, even before they've all been
simplified.

## Type propagation

After simplifying all functions in a set, closure types are computed. Each
closure gets a type that includes:

- Its function slot
- Types for all function slots in the set (enabling type-based devirtualization)
- Types for all value slots (enabling optimization based on captured values)
- The allocation mode

These types are added to the environment so that uses of the closures
elsewhere in the program can benefit from this information. For example,
knowing the exact function in a closure enables direct calls instead of
indirect calls.
