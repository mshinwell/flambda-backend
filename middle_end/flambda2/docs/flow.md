# Flow Analysis in Flambda 2

## Table of contents

- [Flow Analysis in Flambda 2](#flow-analysis-in-flambda-2)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Optimizations enabled](#optimizations-enabled)
    - [Mutable unboxing](#mutable-unboxing)
      - [Eligibility criteria](#eligibility-criteria)
      - [Escaping analysis](#escaping-analysis)
      - [What happens when a block is unboxed](#what-happens-when-a-block-is-unboxed)
      - [Blocks with multiple fields](#blocks-with-multiple-fields)
      - [Examples](#examples)
      - [Interaction with aliases](#interaction-with-aliases)
    - [Dead name analysis](#dead-name-analysis)
      - [Why this requires a separate analysis](#why-this-requires-a-separate-analysis)
      - [How it works](#how-it-works)
    - [Alias analysis](#alias-analysis)
      - [Why this can't be done during the main simplification pass](#why-this-cant-be-done-during-the-main-simplification-pass)
      - [How it works](#how-it-works-1)
  - [Architecture](#architecture)
    - [Module overview](#module-overview)
  - [Integration with the simplifier](#integration-with-the-simplifier)
  - [Debugging](#debugging)

## Introduction

The flow analysis framework (`simplify/flow/`) is a comprehensive dataflow and
control flow analysis system that enables optimizations which require whole-function
analysis. While the main simplifier works locally, some optimizations need global
information about how values flow through the program. The flow analysis provides
this by building and analyzing various graphs after the simplifier has processed
a function.

The primary optimization enabled by flow analysis is **mutable unboxing**, which
transforms heap-allocated mutable blocks into unboxed representations. This is
particularly valuable in loops where allocations can be moved outside the hot path.

## Optimizations enabled

The flow analysis enables several optimizations that require whole-function
information:

- **Mutable unboxing**: Transform heap-allocated mutable blocks (like `ref`
  cells) into unboxed values passed through continuation parameters, eliminating
  allocation in loops.
- **Dead name elimination**: Determine which variables and symbols are truly
  unused, even when they appear to be used in recursive continuation calls
  (e.g., a loop parameter that is passed through but never actually used in
  computation).
- **Alias propagation**: Discover that continuation parameters are always equal
  to some other value (possibly a constant), enabling further simplification.

### Mutable unboxing

The most important optimization enabled by flow analysis is mutable unboxing.
The standard simplifier can unbox immutable values when their types are known
precisely, but mutable blocks present a challenge: their contents can change,
so type information becomes imprecise in loops.

#### Eligibility criteria

A block is eligible for mutable unboxing if **all** of the following hold:

1. **It is created by a `Make_block` primitive** - The analysis only considers
   blocks whose allocation site is visible within the current scope.

2. **It is required** - Dead blocks are not unboxed (this is a correctness
   requirement, not just an optimization: unboxing a dead block could keep
   alive references to other unboxed blocks whose creation was removed).

3. **It does not escape** - The block must not be observable outside the
   current scope.

#### Escaping analysis

A block escapes if any of the following occur:

- **Returned from the function**: The block is passed to the return continuation.
- **Passed to the exception continuation**: Exception handlers may capture the
  block.
- **Used in general code**: The block appears in `used_in_handler` (free
  variables of the handler body, excluding tracked mutable primitives).
- **Stored in a closure**: The block is captured in a value slot of a closure.
- **Used in a required binding**: The block is a dependency of another required
  name (excluding mutable primitives which are handled specially).
- **Escapes through an alias**: If the block has an alias that flows to a
  different value, the block escapes. This handles cases like:
  ```
  let r = ref x in
  let r' = r in      (* r' is an alias of r *)
  use_somehow r'     (* r escapes through r' *)
  ```

#### What happens when a block is unboxed

When a block is eligible for unboxing:

1. The `Make_block` primitive is removed
2. Block fields become extra parameters on continuations where the block is
   "live" (used but not yet defined)
3. `Block_load` operations are replaced with direct references to the field
   variable
4. `Block_set` operations are removed, and the new value is passed as an
   argument to the next continuation
5. `Is_int` is replaced with `false` (unboxed blocks are always blocks)
6. `Get_tag` is replaced with the statically known tag

#### Blocks with multiple fields

Blocks with multiple fields are fully supported. Each field becomes a separate
extra parameter on the continuations where the block is live. For example:

```ocaml
type point = { mutable x : float; mutable y : float }

let move_right p n =
  let pt = { x = p.x; y = p.y } in
  for _ = 1 to n do
    pt.x <- pt.x +. 1.0
  done;
  (pt.x, pt.y)
```

Here `pt` has two fields. When unboxed, the loop continuation gains two extra
parameters (one for `x`, one for `y`). On each iteration, `x` is updated while
`y` is passed through unchanged:

```
(* Conceptually, after unboxing: *)
let_cont rec loop (i, pt_x, pt_y) =
  if i > n then
    (pt_x, pt_y)
  else
    apply_cont loop (i + 1, pt_x +. 1.0, pt_y)
in
apply_cont loop (1, p.x, p.y)
```

The analysis tracks which fields are accessed at each point, so loads and
stores are correctly rewritten to reference the appropriate field parameter.

#### Examples

**Eligible: Simple ref in a loop**
```ocaml
let sum arr =
  let acc = ref 0.0 in
  for i = 0 to Array.length arr - 1 do
    acc := !acc +. arr.(i)
  done;
  !acc
```
The `ref` cell doesn't escape - only its contents are returned. The cell
can be unboxed, with `!acc` becoming a continuation parameter.

**Eligible: Mutable record in a loop**
```ocaml
type t = { mutable x : int }

let count l =
  let t = { x = 0 } in
  List.iter (fun () -> t.x <- t.x + 1) l;
  t.x  (* only the field is returned, not the record *)
```

**Eligible: Invariant ref (not modified in loop)**
```ocaml
let f x =
  let r = ref x in
  while condition do
    let _ = !r in
    ()
  done;
  !r + 1
```
Even though `r` is not modified, it can still be unboxed. The value `x`
becomes a continuation parameter passed through the loop.

**NOT Eligible: Returned block**
```ocaml
let f l =
  let t = { x = 0 } in
  List.iter (fun () -> t.x <- t.x + 1) l;
  t  (* the record itself is returned - escapes! *)
```
The record escapes because it's returned from the function.

**NOT Eligible: Stored in another structure**
```ocaml
let split_on_char sep s =
  let r = ref [] in  (* this ref escapes *)
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if s.[i] = sep then (
      r := sub s (i + 1) (!j - i - 1) :: !r;  (* r's contents become part of result *)
      j := i)
  done;
  sub s 0 !j :: !r  (* r's contents are returned *)
```
Here `j` can be unboxed (only its integer contents are used), but `r`
cannot because the list it contains is returned.

**NOT Eligible: Exception handling complications**
```ocaml
let f x =
  let r = ref x in
  try
    while condition do
      may_raise ();
      r := !r + 1
    done;
    !r
  with _ -> !r  (* r must be accessible in exception handler *)
```
If an exception can be raised, the block must survive to the exception
handler, which may complicate unboxing (though some cases can still work).

#### Interaction with aliases

The analysis tracks direct aliases like `let r' = r` and considers them
equivalent for escaping purposes. If `r'` escapes, so does `r`. This
prevents incorrect optimizations when the same block is referenced through
multiple names.

### Dead name analysis

The flow analysis identifies which *names* (variables and symbols) are actually
needed to compute the final result. This enables removal of unused continuation
parameters, which is particularly valuable for recursive continuations where
some parameters may be artifacts of code generation.

#### Why this requires a separate analysis

For non-recursive continuations, the simplifier can track which parameters are
used and remove unused ones during the normal simplification pass. However,
for **recursive continuations**, determining whether a parameter is truly dead
requires a separate dataflow analysis.

Consider:

```
let_cont rec k (x, y, z) =
  let result = ... computation using x and z ... in
  if done then
    return result
  else
    apply_cont k (x', y, z')   (* y passed through unchanged *)
in
apply_cont k (a, b, c)
```

Here, `y` appears in the body (in the recursive `apply_cont`), so a simple
free-variable check would consider it "used". But `y` is never actually used
in any computation - it's just passed through to the next iteration. If all
uses of `y` are like this (only flowing to the parameter position of recursive
calls), then `y` is effectively dead and can be removed.

Detecting this requires tracing how values flow through the recursive structure.
The simplifier can't determine this locally - it needs to analyze the entire
continuation to see that `y` only ever flows back to itself without contributing
to any actual result.

#### How it works

The analysis builds a dependency graph with edges representing "if A is needed,
then B is also needed":

- **Name-to-name edges**: If variable `x` is defined as `let x = f y z`, then
  edges are added from `x` to `y` and `z`.
- **Parameter-to-argument edges**: If continuation `k` has parameter `p` and is
  called with `apply_cont k (arg)`, an edge is added from `p` to `arg`. This
  captures the fact that if the parameter is needed, the argument must also be
  computed.
- **Code ID edges**: Dependencies through function code are also tracked,
  including code age relations (for versioned code objects).

Starting from the return continuation's arguments (which are unconditionally
needed), a breadth-first reachability analysis computes the transitive closure
of required names. Any name (variable or symbol) not in this set is dead and
can be eliminated.

### Alias analysis

Using dominator analysis on the variable dependency graph, the flow analysis
identifies which variables are aliases of each other. This information enables:

- Removing redundant continuation parameters that are always equal to another
  variable
- Introducing explicit `let` bindings when aliases are discovered
- Detecting when useful aliases (constants, symbols with known types) are
  discovered inside loops, triggering resimplification

#### Why this can't be done during the main simplification pass

Alias information flows backwards through the control flow graph, opposite to
how simplification normally works. Consider:

```
let_cont k (x) =
  let_cont j (y) =
    ... use y ...
  in
  apply_cont j (x)
in
apply_cont k (some_constant)
```

When simplifying the handler for `j`, the simplifier knows that `y` is a
parameter but doesn't know what value it will receive. Only by examining all
call sites to `j` can we determine that `y` is always equal to `x`. And only
by examining all call sites to `k` can we determine that `x` is always equal
to `some_constant`.

This backward flow of information requires a whole-function view that the
forward-processing simplifier doesn't naturally have.

#### How it works

The analysis builds a graph where edges go from continuation parameters to
the arguments provided at call sites. If parameter `p` receives argument `a`
at some call site, there's an edge from `p` to `a`.

Using this graph, a **dominator analysis** finds the "canonical representative"
for each alias class. A variable `v` dominates another variable `w` if every
path from the roots to `w` passes through `v`. In the alias graph, this means
`v` is a valid replacement for `w` because `v` is always in scope when `w` is
used.

The analysis uses strongly connected components to handle cycles (which arise
from recursive continuations) and computes a fixpoint for the dominator
relation within each cycle.

The result is a map from each variable to its dominating alias. Variables that
map to constants or symbols are particularly valuable, as this information can
trigger resimplification of loops where such aliases were previously unknown.

## Architecture

The flow analysis is structured as a pipeline of analyses:

```
Accumulator Building (flow_acc.ml)
         |
         v
Main Orchestration (flow_analysis.ml)
    |         |         |         |
    v         v         v         v
  Data      Dominator  Control   Mutable
  Flow      Graph      Flow      Unboxing
  Graph                Graph
```

### Module overview

**flow_types.ml** - Core type definitions shared across all modules:
- `Acc.t` - The accumulator containing continuation info and bindings
- `Continuation_info.t` - Information about each continuation handler
- `Mutable_prim.t` - Tracked primitives for unboxing analysis
- Result types for each analysis phase

**flow_acc.ml** - Builds the accumulator during simplification:
- Maintains a stack of continuation handlers
- Records variable bindings and their uses
- Tracks apply continuation arguments
- Collects mutable primitives for later analysis

**flow_analysis.ml** - Orchestrates the analysis pipeline:
- Normalizes the accumulator
- Invokes each sub-analysis in sequence
- Combines results into a final `Flow_result.t`

**data_flow_graph.ml** - Dead variable analysis:
- Creates a dependency graph where names depend on other names
- Implements breadth-first reachability from return values
- Handles code IDs and their age relations
- Returns the set of required names

**dominator_graph.ml** - Alias analysis via dominators:
- Builds a simple dependency graph from variable aliases
- Performs dominator analysis using strongly connected components
- Produces an alias map for each continuation

**control_flow_graph.ml** - Control flow extraction:
- Constructs the continuation call graph
- Computes which variables are available at each continuation
- Determines extra parameters and arguments for aliases
- Can output DOT graphs for visualization (useful for debugging)

**mutable_unboxing.ml** - The core unboxing optimization:
- Analyzes which blocks can be unboxed via escaping analysis
- Determines which fields of blocks are accessed
- Generates extra parameters and arguments for unboxed fields
- Creates rewrites to transform primitive operations

## Integration with the simplifier

The flow analysis runs after the simplifier has processed the body of a
`Let_cont` expression. The results feed back into the simplifier to:

1. Remove dead parameters from continuation handlers
2. Add extra parameters for unboxed block fields
3. Rewrite primitive operations on unboxed blocks
4. Introduce `let` bindings for aliases
5. Potentially trigger resimplification when useful aliases are found

The key data structure returned is `Flow_result.t`, which contains:
- `required_names` - Variables that are actually used
- `aliases` - Alias information for each continuation
- `mutable_unboxing` - Rewrites for unboxed blocks

## Debugging

The control flow graph module can output DOT format graphs for visualization.
This is helpful for understanding how continuations relate to each other and
how values flow through the program.
