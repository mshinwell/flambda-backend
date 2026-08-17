# Repro: `-flambda2-reaper` breaks cross-unit `[@zero_alloc]` checks

Two files, no dependencies beyond the compiler. Run `./run.sh` (override the
compiler with `COMPILER=/path/to/ocamlopt.opt ./run.sh`).

- `lib_a.ml` — mimics `Base.List`'s stack `map`: a `let[@inline] map_stack`
  wrapper whose exported inlinable body defines a lambda-lifted inner
  recursive `loop`, closed over an unknown `f`.  The self call is not a tail
  call, so the loop cannot be loopified and can never be inlined: every use
  of `map_stack` retains a direct call to it.

- `main.ml` — a `[@zero_alloc]`-checked function calling `Lib_a.map_stack`
  with a concrete heap-allocation-free `f`; everything on the path is
  stack-allocated.

## What happens

`main.ml` is compiled identically (`-O3`) in both runs; only `lib_a.ml`'s
flags differ.  `-reaper-local-fields` is not needed; `-flambda2-reaper` alone
suffices.

Without the reaper:

- `lib_a.cmx` exports the inner loop's code as `Code_present`.
- When `main.ml` inlines `map_stack`'s body, flambda2 *duplicates and
  specializes* the loop's code into the caller's unit (`main.o` defines
  `camlMain__loop_1_5_code`; there is no reference to `camlLib_a__loop`
  at all).  With `f` known and inlined, the zero_alloc checker analyzes the
  same-unit copy and the check passes.

With the reaper on `lib_a.ml` only:

- The reaper strips the loop's exported code to `Metadata_only`, even though
  the loop is still referenced by the (still exported, still `Code_present`)
  inlinable body of `map_stack`.
- The caller can no longer duplicate/specialize; a cross-unit direct call to
  `camlLib_a__loop_1_3_code` remains in `main`'s generated code.  No
  zero_alloc summary exists for it in `lib_a.cmx`'s "function summaries for
  static checks" table (the generic loop, with unknown `f`, could not have a
  safe summary anyway), so the check conservatively fails:

```
Error: Annotation check for zero_alloc failed on function Main.sum_squares ...
Error: called function may allocate (direct call camlLib_a__loop_1_3_code)
inlined from main.ml:9,16--50[Main.sum_squares]
Hint: Build artifacts for the library containing the callee are not available.
Try adding the library as an explicit dependency.
```

(The hint is misleading — the artifacts are available; they just contain
neither the code body nor a summary for that symbol.)
