# Simplification of Let expressions

## Table of contents

- [Simplification of Let expressions](#simplification-of-let-expressions)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [High-level structure](#high-level-structure)
  - [Downwards pass](#downwards-pass)
    - [Simplifying the defining expression](#simplifying-the-defining-expression)
    - [Simplifying the body](#simplifying-the-body)
  - [Upwards pass](#upwards-pass)
    - [Binding lifecycle decisions](#binding-lifecycle-decisions)
    - [Phantom bindings](#phantom-bindings)
    - [Mutable unboxing rewrites](#mutable-unboxing-rewrites)
  - [Lifted constants](#lifted-constants)
    - [Constant placement](#constant-placement)
    - [Data flow tracking](#data-flow-tracking)

## Introduction

The `Simplify_let_expr` module handles simplification of `Let` expressions.
A `Let` expression binds one or more variables to a defining expression, then
evaluates a body with those bindings in scope.

The module coordinates:

- **Defining expression simplification**: Delegated to `Simplify_named`, which
  handles primitives, simples, sets of closures, and rec-info expressions
- **Body simplification**: After the defining expression, the body is simplified
  with the new bindings in scope
- **Binding lifecycle decisions**: Whether to keep, delete, or phantomize each
  binding based on usage and effects
- **Lifted constant management**: Tracking and placing constants that have been
  lifted to static allocation
- **Data flow recording**: Registering bindings and their dependencies for
  the flow analysis

## High-level structure

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           simplify_let                                  │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      Simplify defining expression                       │
│                        (via Simplify_named)                             │
│                                                                         │
│  ┌─────────────────────┐              ┌─────────────────────────────┐   │
│  │      Simplified     │              │         Rewritten           │   │
│  │   (bindings ready)  │              │  (let replaced by new expr) │   │
│  └──────────┬──────────┘              └──────────────┬──────────────┘   │
│             │                                        │                  │
└─────────────┼────────────────────────────────────────┼──────────────────┘
              │                                        │
              ▼                                        ▼
┌─────────────────────────┐              ┌─────────────────────────────┐
│    Simplify body        │              │   Simplify replacement      │
│                         │              │       expression            │
└───────────┬─────────────┘              └─────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                           rebuild_let                                   │
│                                                                         │
│  For each binding:                                                      │
│    - Check if binding has uses (from free names of body)                │
│    - Check if binding has effects that must be preserved                │
│    - Apply mutable unboxing rewrites if any                             │
│    - Decide: Keep / Delete / Phantomize                                 │
│                                                                         │
│  Place lifted constants if at unit toplevel                             │
└─────────────────────────────────────────────────────────────────────────┘
```

## Downwards pass

### Simplifying the defining expression

The defining expression is simplified by `Simplify_named`, which can produce
two kinds of results:

**`Simplified`**: The defining expression has been simplified into one or more
bindings to place. This is the common case. The bindings are stored in a
`Simplify_named_result.t` along with the updated `dacc`.

**`Rewritten`**: The entire `Let` expression should be replaced by a different
expression. This happens when:
- A set of closures is lifted to static allocation, transforming the let into
  a `Let_symbol` binding
- Other whole-expression transformations apply

When the result is `Rewritten`, simplification continues with the replacement
expression rather than following the normal let simplification path.

**`Invalid`**: The defining expression is unreachable. The let expression
becomes `Invalid` without simplifying the body.

### Simplifying the body

After the defining expression is simplified:

1. **Lifted constants are tracked**: Constants generated during defining
   expression simplification are remembered separately from those that existed
   before.

2. **A rewrite ID is created**: This unique identifier links the binding to
   any mutable unboxing rewrites that the flow analysis might produce later.

3. **Data flow is updated**: The binding and its dependencies are recorded
   in the flow accumulator.

4. **The body is simplified**: The body is simplified with the new bindings
   in scope.

## Upwards pass

### Binding lifecycle decisions

During the upwards pass, `rebuild_let` decides the fate of each binding:

```
        ┌────────────────────┐
        │  Has side effects? │
        └─────────┬──────────┘
                  │
       ┌──────────┴──────────┐
       │ Yes                 │ No
       ▼                     ▼
┌─────────────┐    ┌────────────────────┐
│    KEEP     │    │  Has uses in body? │
└─────────────┘    └─────────┬──────────┘
                             │
                  ┌──────────┴──────────┐
                  │ Yes                 │ No
                  ▼                     ▼
           ┌─────────────┐    ┌───────────────────────┐
           │    KEEP     │    │  User-visible?        │
           └─────────────┘    │  Phantom lets on?     │
                              └───────────┬───────────┘
                                          │
                               ┌──────────┴──────────┐
                               │ Both                │ No
                               ▼                     ▼
                        ┌─────────────┐       ┌─────────────┐
                        │ PHANTOMIZE  │       │   DELETE    │
                        └─────────────┘       └─────────────┘
```

**Side effects** include:
- Primitives with more than generative effects
- `End_region` for regions that are actually used

**Uses** are determined by checking if the bound variable appears in the free
names of the body.

### Phantom bindings

When generating debug information, unused bindings for **user-visible variables**
can be kept as **phantom bindings** rather than deleted entirely. A phantom
binding:

- Has name mode `Phantom` instead of `Normal`
- Doesn't contribute to runtime code
- Preserves the association between variable names and their values for debuggers

Phantom bindings are only created when:
- The `-g` flag is used (generating debug info)
- The variable has a user-visible name (not compiler-generated)
- The variable is not a rec-info (depth tracking variable)

### Mutable unboxing rewrites

The flow analysis may produce rewrites for bindings involved in mutable
unboxing. These rewrites are indexed by the `rewrite_id` assigned during the
downwards pass:

- **`Remove_prim`**: The primitive is removed entirely (e.g., a `Make_block`
  for an unboxed mutable block)
- **`Invalid k`**: Replace the primitive with `Invalid k`
- **`Replace_by_binding { var; bound_to }`**: Replace the primitive with a
  simple binding (e.g., replacing `Block_load` with a direct variable reference)

## Lifted constants

### Constant placement

Lifted constants accumulate during simplification but are only placed (turned
into `Let_symbol` bindings) at specific points:

```
┌────────────────────────────────────────────────────────────────────────────┐
│                           Constant placement                               │
│                                                                            │
│   At unit toplevel:                                                        │
│     - Constants from defining expr are placed                              │
│     - Constants from body are placed                                       │
│     - Creates Let_symbol bindings                                          │
│                                                                            │
│   Not at unit toplevel:                                                    │
│     - Constants accumulate in uacc                                         │
│     - Float up to an outer Let at toplevel                                 │
│                                                                            │
└────────────────────────────────────────────────────────────────────────────┘
```

Before placing, constants are filtered to keep only those that are actually
used. A constant is considered used if:
- Any symbol it defines is in the `required_names` of the uacc
- Any code ID it defines is an ancestor of a live code ID

This filtering is essential because:
- Unused constants would waste space in the output
- More importantly, the free names must stay consistent with the data flow
  analysis

### Data flow tracking

The module records binding information for the flow analysis:

**Let bindings**: Each binding is recorded with its bound pattern, simplified
defining expression, and rewrite ID.

**Lifted constants**: For constants that aren't inside closures (where they
can't be placed anyway), the module records:
- Symbol bindings and their free names
- Code ID bindings and their free names
- Set of closures bindings, including:
  - Function slot to symbol mappings
  - Value slot contents and their dependencies
  - Function declaration free names
- Symbol projections (variables known to equal projections from symbols)

This dependency information allows the flow analysis to determine which names
are truly required and which can be eliminated.
