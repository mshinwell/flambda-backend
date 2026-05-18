# `only-default-codegen` poll-insertion migration analysis

Follow-up to the `--poll-insertion` / `--no-poll-insertion` flag work.

## Current state

`ocamltest/ocaml_actions.ml:1504` defines:

```ocaml
let only_default_codegen = Actions.make
  ~name:"only-default-codegen"
  ~description:"Passes if all the codegen options are at the current default, \
                useful for [%%expect_asm]"
  ~does_something:false
  (Actions_helpers.predicate
    (Config.no_stack_checks
      && Config.runtime5
      && not Config.poll_insertion
      && not Config.with_address_sanitizer
      && not Config.with_frame_pointers)
    "default codegen"
    "non-default codegen")
```

This is the only remaining `Config.poll_insertion` reference outside the
infrastructure (`utils/config.*`, `utils/clflags.ml` seed). It functions as a
test gate: tests using `[%%expect_asm]` are skipped on builds whose configure
defaults don't match the assembly expectations baked into the test.

Now that we have a runtime override (`-no-poll-insertion`), poll-insertion is
the one item in the predicate that we can force at compile time. The other
four are build-time properties with no per-invocation toggle.

## Proposed change

Convert `only-default-codegen` from a pure predicate into an action that:

1. **Predicate check (keep)**: still gate on the four build-time knobs that
   can't be overridden — `Config.no_stack_checks`, `Config.runtime5`,
   `not Config.with_address_sanitizer`, `not Config.with_frame_pointers`.
2. **Inject flag**: drop `not Config.poll_insertion` from the predicate;
   instead, when the predicate passes, append `-no-poll-insertion` to
   `Ocaml_variables.flags` so the test runs regardless of how the compiler
   was built.

## Wrinkle: `flags = ` clobbers the injection

`run_expect_once` (ocaml_actions.ml:907) builds its compiler command line
from `flags env` only — it does not consume `ocamlopt_default_flags`, so the
new value has to live in `Ocaml_variables.flags`.

`Environments.append var v env` works fine — but test files that subsequently
do `flags = " -O3 ..."` (an `Assignment`, not an `Append`) overwrite the entire
variable, losing the `-no-poll-insertion` we injected.

Audit of the 25 tests using `only-default-codegen`:

### Tests using `flags = ` (would clobber the injection) — 17 files

```
testsuite/tests/codegen/float32_u.ml
testsuite/tests/codegen/int32_u.ml
testsuite/tests/codegen/int64_u.ml
testsuite/tests/codegen/select.ml
testsuite/tests/codegen/nativeint_u.ml
testsuite/tests/codegen/float_u.ml
testsuite/tests/codegen/instruction_selection.ml
testsuite/tests/codegen/loops.ml
testsuite/tests/codegen/int16_u.ml
testsuite/tests/codegen/register_allocation.ml
testsuite/tests/codegen/bytes.ml
testsuite/tests/codegen/builtins.ml
testsuite/tests/codegen/fields.ml
testsuite/tests/codegen/int8_u.ml
testsuite/tests/codegen/unused_temporaries.ml
testsuite/tests/codegen/load_elimination.ml
testsuite/tests/codegen/arrays.ml
```

These all follow the pattern:
```
only-default-codegen;
flags = " -O3 -I ocamlopt.opt";
flags += " -cfg-prologue-shrink-wrap";
...
```

### Tests using only `flags += ` (safe as-is) — 8 files

```
testsuite/tests/codegen/allocation.ml
testsuite/tests/codegen/int.ml
testsuite/tests/codegen/variants.ml
testsuite/tests/codegen/check_elimination.ml
testsuite/tests/codegen/functions.ml
testsuite/tests/flambda2/for_loop_structure.ml
testsuite/tests/flambda2/switch_lookup_table_kinds.ml
testsuite/tests/tool-expect-test/expect_assembly.ml
```

## Options for fixing the 17 files

- **A.** Change the leading `flags = ` to `flags += ` in each file. Single-
  character edit per file; semantically equivalent because the variable is
  unset prior, and our injection contributes one extra token.
- **B.** Move `only-default-codegen;` to the position immediately before
  `expect.opt;` in each file. Preserves the `flags = ...` style. Slightly
  bigger diff per file but keeps the surface syntax consistent.
- **C.** Leave the test files alone; just modify the action. Skipped tests
  on poll-insertion-enabled builds will start failing because the
  `not Config.poll_insertion` gate is gone. Don't pick this.

## Question about the predicate

If the action no longer gates on `poll_insertion` but still gates on the
other four, the action becomes semi-imperative: predicate-then-inject. The
alternative is to drop the whole predicate and rely on tests failing if run
on non-default builds — but that would regress behavior on builds that
toggle stack-checks / runtime4 / asan / frame-pointers.

Recommendation: keep gating on the other four; only move `poll_insertion`
from gate to injection.

## Implementation sketch

```ocaml
let only_default_codegen =
  let body _log env =
    let other_defaults =
      Config.no_stack_checks
      && Config.runtime5
      && not Config.with_address_sanitizer
      && not Config.with_frame_pointers
    in
    if other_defaults then
      let env =
        Environments.append Ocaml_variables.flags " -no-poll-insertion " env
      in
      (Result.predicate_satisfied_with_reason "default codegen", env)
    else
      (Result.predicate_not_satisfied_with_reason "default codegen", env)
  in
  Actions.make
    ~name:"only-default-codegen"
    ~description:"Passes if non-overridable codegen options match the default \
                  (poll-insertion is forced via -no-poll-insertion). Useful \
                  for [%%expect_asm]."
    ~does_something:false
    body
```

(The exact `Result.*` constructor names match the helpers in
`actions_helpers.ml:predicate`.)

## Open questions

1. Action ordering in test files (option A vs B above).
2. Whether `~does_something:false` is still appropriate — it now mutates env.
3. Should we extend the same treatment to `address_sanitizer` /
   `frame_pointers` once those grow per-invocation overrides? (Out of scope
   for this task.)
