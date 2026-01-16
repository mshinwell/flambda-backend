# Simplification of Let_cont expressions

## Table of contents

- [Simplification of Let\_cont expressions](#simplification-of-let_cont-expressions)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Background: continuations in Flambda 2](#background-continuations-in-flambda-2)
    - [Non-recursive continuations](#non-recursive-continuations)
    - [Recursive continuations](#recursive-continuations)
    - [Invariant parameters](#invariant-parameters)
  - [High-level workflow](#high-level-workflow)
  - [The downwards pass](#the-downwards-pass)
    - [Simplifying the body](#simplifying-the-body)
    - [Collecting continuation uses](#collecting-continuation-uses)
    - [Computing the handler environment (join points)](#computing-the-handler-environment-join-points)
    - [Simplifying handlers](#simplifying-handlers)
  - [The upwards pass](#the-upwards-pass)
    - [Flow analysis](#flow-analysis)
    - [Rebuilding handlers](#rebuilding-handlers)
    - [Rebuilding the Let\_cont](#rebuilding-the-let_cont)
  - [Strongly connected components](#strongly-connected-components)
  - [Match-in-match optimization](#match-in-match-optimization)
    - [Continuation lifting](#continuation-lifting)
    - [Continuation specialization](#continuation-specialization)
  - [Unit toplevel tracking](#unit-toplevel-tracking)
  - [Parameter handling](#parameter-handling)
    - [Unboxing decisions](#unboxing-decisions)
    - [Extra parameters and arguments](#extra-parameters-and-arguments)
    - [Dead parameter elimination](#dead-parameter-elimination)

## Introduction

The `Simplify_let_cont_expr` module handles simplification of `Let_cont`
expressions in Flambda 2. A `Let_cont` binds one or more continuation handlers
that can be invoked via `Apply_cont` expressions. This is one of the most
complex parts of the simplifier because:

1. Continuations can be recursive (loops) or non-recursive (join points)
2. Multiple continuations can be mutually recursive
3. The typing environment for a handler must be computed by joining
   information from all call sites
4. Various optimizations apply at this point

The module is responsible for:

- **Simplifying continuation handlers** with appropriate typing environments
  derived from all call sites (join point computation)
- **Parameter unboxing**: Eliminating boxing when continuation parameters are
  always passed boxed values that are immediately destructured
- **Dead parameter elimination**: Removing unused parameters discovered by
  flow analysis
- **Match-in-match optimization**: A combination of continuation lifting and
  specialization that optimizes nested pattern matches by moving join points
  outside outer matches and creating specialized versions for each call site

The two-pass structure (downwards then upwards) allows the simplifier to first
collect all uses of a continuation, compute a joined typing environment, and
simplify the handler with full information. The upwards pass then rebuilds the
expression, applying rewrites based on flow analysis results.

## Background: continuations in Flambda 2

### Non-recursive continuations

A non-recursive continuation is used once or a small number of times. It acts
as a "join point" where control flow merges:

```
let_cont k (x, y) =
  ... handler using x and y ...
in
if condition then
  apply_cont k (a, b)
else
  apply_cont k (c, d)
```

Here `k` is called from two places with potentially different argument values.
The handler must work correctly regardless of which call site invoked it.

### Recursive continuations

A recursive continuation represents a loop:

```
let_cont rec loop (i, acc) =
  if i > n then
    apply_cont return (acc)
  else
    apply_cont loop (i + 1, acc + arr.(i))
in
apply_cont loop (0, 0)
```

The handler can call itself, creating iteration.

### Invariant parameters

For recursive continuations, some parameters may have the same value on every
iteration. These are called **invariant parameters**:

```
let_cont rec loop [n] (i, acc) =    (* n is invariant *)
  if i > n then
    apply_cont return (acc)
  else
    apply_cont loop (i + 1, acc + 1)  (* n not passed - it's invariant *)
in
apply_cont loop [100] (0, 0)
```

Invariant parameters are passed once at the initial call and don't need to be
passed on recursive calls. This reduces register pressure in loops.

## High-level workflow

The simplification proceeds in two main passes, with the transition mediated
by a "down_to_up" callback:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            DOWNWARDS PASS                                   │
│                                                                             │
│  ┌──────────────────┐                                                       │
│  │ simplify_let_cont│  Entry point: open the Let_cont structure             │
│  └────────┬─────────┘                                                       │
│           │                                                                 │
│           │ Simplify body, collecting continuation uses                     │
│           ▼                                                                 │
│  ┌──────────────────────────────┐                                           │
│  │ after_downwards_traversal_   │  Body done. Decide: lift continuation     │
│  │ of_body                      │  out, or simplify handlers here?          │
│  └────────┬─────────────────────┘                                           │
│           │                                                                 │
│           │ For each handler: compute join, simplify                        │
│           ▼                                                                 │
│  ┌──────────────────────────────┐                                           │
│  │ after_downwards_traversal_   │  All handlers simplified.                 │
│  │ of_body_and_handlers         │  Maybe specialize. Run flow analysis.     │
│  └────────┬─────────────────────┘                                           │
│           │                                                                 │
└───────────┼─────────────────────────────────────────────────────────────────┘
            │
            │  down_to_up callback (runs flow analysis)
            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                             UPWARDS PASS                                    │
│                                                                             │
│  ┌──────────────────────────────┐                                           │
│  │ prepare_to_rebuild_handlers  │  Use flow results to set up rewrites      │
│  └────────┬─────────────────────┘                                           │
│           │                                                                 │
│           │ Rebuild each handler (outside to inside for nested conts)       │
│           ▼                                                                 │
│  ┌──────────────────────────────┐                                           │
│  │ prepare_to_rebuild_body      │  All handlers rebuilt and in environment  │
│  └────────┬─────────────────────┘                                           │
│           │                                                                 │
│           │ Rebuild body                                                    │
│           ▼                                                                 │
│  ┌──────────────────────────────┐                                           │
│  │ rebuild_let_cont             │  Reconstruct the Let_cont expression      │
│  └──────────────────────────────┘                                           │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

The "down_to_up" mechanism is part of the continuation-passing style used
throughout the simplifier (see `simplify_common.mli` for background).

## The downwards pass

### Simplifying the body

The body is simplified first so that all uses of the continuation are visible
before we simplify the handler. This is essential because:

1. We need to know the types of arguments at each call site
2. We can detect if the continuation is unused (dead code)
3. We can detect if it has a single use (enabling inlining)

Before simplifying the body, the environment is set up:

- **Scope increment**: The scope level is bumped twice - once for the join
  point level (n+1) and once for the body (n+2). This ensures types from
  inside don't leak outside.

- **Continuation registration**: The bound continuations are registered so
  `Apply_cont` expressions can find them.

### Collecting continuation uses

As the body is simplified, each `Apply_cont` to one of the bound continuations
records a **continuation use** containing:

- The typing environment at the use site
- The types of the arguments
- A unique use ID (for tracking rewrites)
- Whether the use is potentially inlinable

These uses are collected in the `Continuation_uses_env`.

### Computing the handler environment (join points)

Before simplifying a handler, we must compute its typing environment. This is
done by `Join_points.compute_handler_env`, which:

1. **Joins argument types**: For each parameter, compute the join (least upper
   bound) of argument types across all uses
2. **Computes the handler environment**: Merge information from all use sites
3. **Detects single inlinable use**: If there's exactly one use and it's
   inlinable, the handler can be inlined at that use site

```
            Use 1                    Use 2
         ┌─────────┐              ┌─────────┐
         │ x : int │              │ x : int │
         │ y : τ₁  │              │ y : τ₂  │
         └────┬────┘              └────┬────┘
              │                        │
              └───────────┬────────────┘
                          │ JOIN
                          ▼
                    ┌───────────┐
                    │ x : int   │  (same at both sites)
                    │ y : τ₁∨τ₂ │  (joined type)
                    └───────────┘
                          │
                          ▼
                  Handler environment
```

### Simplifying handlers

Each handler is simplified with the joined environment. For recursive
handlers:

- All handlers in the recursive group share invariant parameters
- Parameters are added with unknown types (since values change each iteration)
- The handlers can call themselves and each other

For non-recursive handlers:

- The handler is simplified once
- If single-use and inlinable, it may be inlined at the call site

**Important**: Continuations with more than one use are never inlined directly.
The handler is simplified once using the joined typing environment, which loses
the precise type information available at each individual call site. When
call-site-specific optimization is beneficial (e.g., one call site passes a
known constant while another passes an unknown value), **continuation
specialization** is used instead - this creates separate copies of the handler
for different call sites, each optimized with the types available at that site.
See [Match-in-match optimization](#match-in-match-optimization).

## The upwards pass

### Flow analysis

Before rebuilding, the **flow analysis** runs on the simplified code
(see `flow.md`). This provides:

- **Required names**: Which variables are actually needed
- **Alias information**: Which parameters are always equal to some other value
- **Mutable unboxing**: Which blocks can be unboxed into parameters

### Rebuilding handlers

Handlers are rebuilt from the outside to the inside (for nested continuations).
For each handler:

1. **Apply_cont rewrites**: The `Apply_cont_rewrite` records which parameters are
   used, unused, or replaced by extra parameters

2. **Add let bindings for aliases**: If flow analysis discovered aliases,
   introduce `let` bindings at the handler entry

3. **Create phantom bindings**: Unused parameters that are still referenced
   in debug info become phantom bindings

4. **Determine handler disposition**: The handler is classified as:
   - Inlinable (single use, will be inlined)
   - Shortcut (immediately jumps to another continuation)
   - Invalid (handler is unreachable)
   - Normal (will be kept as a handler)

### Rebuilding the Let_cont

Finally, the `Let_cont` expression is reconstructed:

- **Dead continuation**: If the continuation has zero uses, only the body
  is kept
- **Trivial body**: If the body is just `apply_cont k ()` with no args,
  only the handler is kept
- **Normal case**: The full `Let_cont` is reconstructed

## Strongly connected components

When simplifying recursive continuations, they may not all actually be
mutually recursive. The module performs **SCC (strongly connected components)
analysis** to split them:

```
Original:
  let_cont rec a (...) = ... apply_cont b ...
        and b (...) = ... apply_cont c ...
        and c (...) = ... apply_cont a ...
        and d (...) = ... apply_cont d ...
  in body

After SCC analysis:
  let_cont rec a (...) = ... apply_cont b ...     ┐
        and b (...) = ... apply_cont c ...        ├─ One recursive group
        and c (...) = ... apply_cont a ...        ┘
  in
    let_cont rec d (...) = ... apply_cont d ...   ─── Another recursive group
    in body
```

If a continuation in a "recursive" group doesn't actually call itself (directly
or indirectly), it becomes non-recursive:

```
Original:
  let_cont rec a (...) = ... apply_cont b ...
        and b (...) = ... return ...
  in body

After SCC:
  let_cont a (...) = ... apply_cont b ...   ─── Non-recursive!
  in
    let_cont b (...) = ... return ...       ─── Non-recursive!
    in body
```

This is beneficial because non-recursive continuations can be inlined and
have more precise typing.

## Match-in-match optimization

The **match-in-match** pattern occurs when a match expression appears inside
another match expression, and the inner match depends on a value that varies
across branches of the outer match:

```ocaml
match outer with
| A x ->
  (match x with         (* inner match on x *)
   | P -> ...
   | Q -> ...)
| B y ->
  (match y with         (* inner match on y *)
   | P -> ...
   | Q -> ...)
```

The challenge is that after the inner matches, control flow often merges at a
common join point. If the inner match can be specialized based on what we know
from the outer match, significant optimization is possible.

The simplifier addresses this with two related mechanisms: **continuation
lifting** (to move the join point outside the outer match) and **continuation
specialization** (to create optimized versions for each call site).

### Continuation lifting

**Continuation lifting** moves a continuation definition outside its current
context. In the match-in-match case, this means lifting the join continuation
out of the inner match so it can be shared across branches of the outer match:

```
Before lifting:
  let_cont outer_join (result) =
    ... use result ...
  in
    match outer with
    | A x ->
      let_cont inner_join (r) =       <- defined inside each branch
        apply_cont outer_join (r)
      in
        match x with ...
    | B y ->
      let_cont inner_join (r) =       <- duplicated
        apply_cont outer_join (r)
      in
        match y with ...

After lifting:
  let_cont inner_join (r) =            <- lifted outside
    apply_cont outer_join (r)
  in
    let_cont outer_join (result) = ...
    in
      match outer with
      | A x -> match x with ... apply_cont inner_join ...
      | B y -> match y with ... apply_cont inner_join ...
```

When a continuation is lifted, it may need **extra parameters** to capture
values that were in scope at its original location but aren't in scope at the
lifted position. These are tracked by `Lifted_cont_params`.

The decision to lift is made in `after_downwards_traversal_of_body` based on
the `Are_lifting_conts` state in the downwards accumulator.

### Continuation specialization

**Continuation specialization** creates separate versions of a continuation
for different call sites. After lifting creates a shared join point,
specialization can create optimized versions when different call sites provide
arguments with significantly different types:

```
Before specialization:
  let_cont k (x) =
    match x with           <- x could be A or B depending on call site
    | A -> ...
    | B -> ...
  in
    if condition then
      apply_cont k (A)     <- we know x = A here
    else
      apply_cont k (B)     <- we know x = B here

After specialization:
  let_cont k_for_A (x) =
    ... optimized knowing x = A ...
  in
    let_cont k_for_B (x) =
      ... optimized knowing x = B ...
    in
      if condition then
        apply_cont k_for_A (A)
      else
        apply_cont k_for_B (B)
```

Specialization is controlled by `DA.continuations_to_specialize` and a budget
system that limits how much code duplication occurs.

## Unit toplevel tracking

The simplifier tracks whether code is at the "unit toplevel" - a position that
**postdominates** the module entry point. This affects:

1. **Static allocation**: Values bound at toplevel can become symbols
2. **Lifted constants**: Constants can be placed at toplevel

A handler is at unit toplevel if:
- The enclosing environment is at unit toplevel
- The handler is not an exception handler
- The body can only exit through this continuation or the toplevel exception
  handler (i.e., the handler postdominates the body)

```ocaml
(* At toplevel: *)
let x = ...              (* x bound at toplevel *)

let f () =
  (* NOT at toplevel - inside a function body *)
  let y = ... in
  ...

(* Also NOT at toplevel: *)
if condition then
  let z = ...            (* z is under a conditional *)
```

The postdomination check (lines 1578-1590 of the implementation) verifies that
the body's only exits are to this handler or the toplevel exception continuation.

## Parameter handling

### Unboxing decisions

Before simplifying a handler, **unboxing decisions** are made for its
parameters. If a parameter always receives a boxed value that is immediately
destructured, the boxing can be eliminated:

```
Before unboxing:
  let_cont k (pair) =           <- pair is always a 2-tuple
    let (a, b) = pair in
    ... use a and b ...
  in
    apply_cont k ((x, y))

After unboxing:
  let_cont k (a, b) =           <- unboxed into two parameters
    ... use a and b ...
  in
    apply_cont k (x, y)
```

These decisions are made by `Unbox_continuation_params` and recorded in the
handler data for application during rebuilding.

### Extra parameters and arguments

Several optimizations add **extra parameters** to continuations:

- **CSE (Common Subexpression Elimination)**: Adds parameters for values that
  are computed before the continuation and used inside
- **Continuation lifting**: Adds parameters for values in scope at the
  original location
- **Mutable unboxing**: Adds parameters for unboxed mutable fields
- **Alias propagation**: Adds parameters for discovered aliases

These are tracked in `Continuation_extra_params_and_args` (EPA) and applied
via `Apply_cont_rewrite`.

### Dead parameter elimination

The flow analysis identifies parameters that are never used. These are handled
during rebuilding:

1. **Truly dead**: Parameter removed, argument not evaluated
2. **Used only in debug info**: Parameter becomes a phantom binding
3. **Invariant in recursive continuation**: Marked as `Used_as_invariant`

The `decide_param_usage_*` functions make these determinations based on
free names (precise, from rebuilt code) and required names (from flow analysis).
