# Simplification of Switch expressions

## Table of contents

- [Simplification of Switch expressions](#simplification-of-switch-expressions)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Downwards pass](#downwards-pass)
  - [Upwards pass and optimizations](#upwards-pass-and-optimizations)
    - [Dead arm elimination](#dead-arm-elimination)
    - [Arm merging](#arm-merging)
    - [Identity switch](#identity-switch)
    - [Boolean NOT](#boolean-not)
    - [Lookup table optimization](#lookup-table-optimization)
    - [Affine optimization](#affine-optimization)
  - [Triggering continuation specialization](#triggering-continuation-specialization)

## Introduction

The `Simplify_switch_expr` module handles simplification of `Switch` expressions.
A `Switch` expression branches on an integer scrutinee, with each arm specifying
a discriminant (the integer value that selects that arm) and an action (an
`Apply_cont` to execute).

The module performs several important optimizations:

- **Dead arm elimination**: Arms that can never be taken (based on the scrutinee's
  type) are removed entirely
- **Arm merging**: When all arms go to the same continuation with compatible
  arguments, the switch can be replaced by a single `Apply_cont`
- **Pattern recognition**: Special cases like identity switches and boolean NOT
  are detected and compiled more efficiently
- **Lookup table optimization**: Large switches that pass different constants to
  the same continuation can be compiled using a lookup table, dramatically
  reducing code size
- **Continuation specialization triggering**: Switches are the key site where
  the match-in-match optimization is triggered

## Downwards pass

During the downwards pass, `simplify_switch` processes each arm of the switch:

1. **Scrutinee simplification**: The scrutinee is simplified and its type is
   obtained from the typing environment.

2. **Arm filtering**: For each arm, the scrutinee type is met with the shape
   `this_naked_immediate arm_discriminant`. If the meet is bottom, the arm can
   never be taken and is discarded.

3. **Type refinement**: For arms that survive filtering, the typing environment
   is refined to reflect the knowledge that the scrutinee equals the arm's
   discriminant. This refined environment is used when recording the
   continuation use.

4. **Continuation use recording**: Each arm's destination continuation is
   recorded as a use with the `Switch_branch` context. This affects how the
   continuation is treated during join point computation.

## Upwards pass and optimizations

During the upwards pass (`rebuild_switch`), several optimizations are attempted
in sequence. The first applicable optimization is applied.

### Dead arm elimination

Arms are eliminated during the downwards pass if their discriminant is
inconsistent with the scrutinee's type. For example, if the scrutinee is known
to be in the range `[0, 2]`, an arm for discriminant `5` is removed.

If all arms are eliminated, the switch becomes invalid (unreachable code).
If only one arm remains, it's simplified to a direct `Apply_cont`.

### Arm merging

If all arms call the same continuation with arguments that have a common alias,
the entire switch can be replaced with a single `Apply_cont`:

```

   switch x
   | 0 -> k a
   | 1 -> k a        ──────►   k a
   | 2 -> k a

```

The module tracks `mergeable_arms` while processing each arm. Arms are mergeable
if they:
- All call the same continuation
- Have no trap action
- Pass arguments that share at least one common alias

The alias analysis uses `TE.aliases_of_simple` to find all aliases of each
argument, then intersects the alias sets across arms to find a common
representative.

### Identity switch

An identity switch passes its discriminant unchanged to the continuation:

```

   switch x
   | 0 -> k 0
   | 1 -> k 1        ──────►   k (tag x)
   | 2 -> k 2

```

When detected, the switch is replaced with tagging the scrutinee and passing
it directly to the continuation.

### Boolean NOT

A boolean NOT switch swaps true and false:

```

   switch x
   | 0 -> k 1
   | 1 -> k 0        ──────►   k (not (tag x))

```

When detected, the switch is replaced with a `Boolean_not` primitive.

### Lookup table optimization

For larger switches where all arms pass a single constant to the same
continuation, a lookup table can be used:

```

   switch x                         let block = [| 10; 20; 30 |] in
   | 0 -> k 10                      let arg = block.(tag x) in
   | 1 -> k 20       ──────►        k arg
   | 2 -> k 30

```

This optimization:
- Requires at least 3 arms
- Requires discriminants to be consecutive starting from 0
- Requires all arms to pass a single constant (tagged or naked immediate)
- Creates a static block containing the constants
- Replaces the switch with an array load

The lookup table is created as a lifted constant (static data), so there's
no runtime allocation.

### Affine optimization

When the lookup table values form an arithmetic sequence, even the table
lookup can be avoided:

```

   switch x
   | 0 -> k 10
   | 1 -> k 20       ──────►   k (10 + x * 10)
   | 2 -> k 30

```

The module detects affine patterns of the form `offset + index * slope` and
generates the arithmetic directly. This is both smaller and faster than a
table lookup.

## Triggering continuation specialization

The switch simplifier is the key location where continuation specialization
(for the match-in-match optimization) is triggered. When processing a switch:

1. **Check if analyzing a continuation**: If the downwards accumulator indicates
   we're in the `Analyzing` state for a continuation, the switch might trigger
   specialization.

2. **Eligibility checks**:
   - The continuation must have more than one use
   - It must not be an exception handler
   - We must not be at unit toplevel (to avoid symbol duplication)
   - Both the lifting budget and specialization budget must be sufficient

3. **Budget calculation**:
   - **Lifting cost**: Based on `cost_of_lifting_continuations_out_of_current_one`,
     which accounts for extra parameters
   - **Specialization cost**: `n_uses + 1` (one traversal per use plus one for
     the unspecialized version)

4. **State transition**: If eligible, the `Are_lifting_conts` state transitions
   from `Analyzing` to `Lifting_out_of`, and the continuation is marked for
   specialization.

This mechanism allows nested pattern matches to be optimized by creating
specialized versions of the outer continuation for each branch of the inner
match, eliminating redundant matching.
