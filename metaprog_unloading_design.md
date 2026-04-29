# Compilation Unit Unloading: Design Scope

## Background

Metaprogramming-generated code (tests in `testsuite/tests/quotation/eval`) is
emitted via the arm64 / x86 binary emitters into a memory buffer that is then
executed in-process. Today these buffers can never be freed: code pointers and
closures may still reference them, and we have no mechanism to determine
liveness.

The mechanism designed here is general — anything compiled as an
"unloadable" CU can be reclaimed when no live reference remains. The
primary consumer is the metaprogramming JIT path, but the same flag and
machinery should also serve `Dynlink` units that opt in.

## Strategy

Treat each unloadable compilation unit as a "scannable heap region with
explicit dependency edges". Standard mark/sweep keeps code+data alive
transitively; the unit is unloaded when nothing in it is marked at end of
major cycle.

Core mechanisms:

1. **Code blocks as heap-shaped objects.** For every function in an
   unloadable CU the compiler emits a `Code_block` — a regular OCaml
   block (new tag `Code_block_tag`, standard heap header) in a writable
   static section. Its scannable fields are the function's direct
   **unloadable** dependencies: pointers to other unloadable
   `Code_block`s and to data blocks (the module block among them). The
   mark bit lives in the standard heap color bits.

2. **Back-pointer in `.text` for unloadable functions.** Immediately
   before each labeled entry of an unloadable function, the compiler
   emits one read-only word holding the function's `Code_block` address.
   Set at link/load time, never written again — `.text` stays RX.
   Non-unloadable functions emit nothing.

3. **A closinfo flag bit** identifies closures whose code lives in an
   unloadable CU. The major-GC closure-scan, on a closure with the bit
   set, takes Field 0 (the code pointer), reads
   `*((value *)entry - 1)` to recover the `Code_block` address, and
   calls `caml_darken` on it.

4. **A frame-descriptor flag bit** identifies stack frames whose return
   address points into unloadable code. The stack walker maps the RA to
   its function entry via the frame table / code fragment, reads the
   back-pointer, and calls `caml_darken` on the `Code_block`.

5. **A `Code_pointer` Cmm machtype** identifies stack/register slots
   that transiently hold a code pointer (e.g. between loading Field 0
   of a closure and issuing an indirect call, when the value is
   spilled or held across a safepoint). Frame descriptors record these
   slots in a parallel `code_ptr_live_ofs[]` array. The stack walker
   reads each such slot, finds the back-pointer at `entry - 1`, and
   calls `caml_darken` on the `Code_block`. This makes code-pointer
   liveness explicit at every safepoint rather than depending on the
   closure being kept live across the load-call sequence.

6. **Static data in unloadable CUs is emitted unmarked in a writable
   section**, so the standard mark traversal can mark it via heap
   references (the same section also holds the `Code_block`s).

7. At end of marking, for each unloadable CU: if no `Code_block` and
   no data block in the unit is marked, **unload** (remove frame table
   entries, code fragments, munmap the buffer, free the registration).
   Otherwise reset marks for the next cycle.

## A. Per-CU `is_unloadable` flag plumbing

### A.0 Trigger: a new internal `Clflags` flag

The unloadability of a CU is orthogonal to the existing
`requires_metaprogramming` / `uses_metaprogramming` flags
(`utils/clflags.ml:117–118`), which signal that a unit *contains
quotations* or *invokes `Eval.eval`* respectively. Unloadability is a
distinct property: "this CU's compiled output should be reclaimable
when nothing references it."

- Add `Clflags.unit_is_unloadable : bool ref` (in
  `utils/clflags.ml`/`.mli`), default `false`.
- Initially **not exposed via `main_args.ml`** as a user-facing CLI
  option. Set programmatically by the producers that know they need
  unloadable output:
  - The `Eval.eval` JIT path (`otherlibs/eval/eval.ml`) for
    quotation-driven compilation. (Existing site that already drives
    the JIT pipeline.)
  - The `expectnat` test driver
    (`oxcaml/testsuite/tools/expectnat.ml`), so each compiled phrase
    can be exercised as an unloadable CU. This is the harness for the
    test cases described under "Testing" below.
  - Future: a Dynlink opt-in API (out of scope for the initial
    landing).
- A hidden `-unit-is-unloadable` CLI flag can be added later for
  direct manual testing if useful, but isn't needed for the initial
  PRs.

`Clflags` is global mutable state; producers must save/restore the
flag around the compilation call. Both Eval and expectnat already
manage Clflags this way for other flags.

### A.1 `Code_metadata` field

**Single source of truth: `Code_metadata.t`.**

- Add `bool is_unloadable` field at
  `middle_end/flambda2/terms/code_metadata.ml` (alongside `cold`).
- Mirror the existing `cold` threading: accessor in
  `code_metadata.mli` → set during closure conversion from
  `Clflags.unit_is_unloadable` (snapshot at the start of compilation
  so a mid-compile Clflags change can't poison things) → consumed in
  `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml` around line
  1170 and OR'd into `fun_flags : Cmm.fun_flag list`.

The flag then fans out to four emit-time consumers (renumbered below
as A.2–A.5; previous A.1–A.4 shift up by one):

### A.2 Closinfo bit (closures)

- Sites: `backend/cmm_helpers.ml:357` (`pack_closure_info`) and the
  partial-app path at line 3984.
- Claim **closinfo bit 54** (steals 1 bit from the 54-bit `start_env`
  delta — still covers ~128 PB per closure).
- Update macros in `runtime/caml/mlvalues.h:477`: new
  `Unloadable_closinfo(info)`, updated `Make_closinfo` and
  `Start_env_closinfo`.
- Mirror `Make_closinfo` updates in `runtime/interp.c:730/737` and
  `caml_alloc_closure` so dynamic closures (e.g., partial application)
  over unloadable code propagate the flag.

### A.3 Frame descriptor bit (stack frames)

- `backend/emitaux.ml:44` `frame_descr` record gains
  `fd_unloadable : bool`; `record_frame_descr` (line 78) takes it.
- Callers: `backend/amd64/emit.ml:616` and `backend/arm64/emit.ml:741`
  pass it.
- In `emit_frames` (`emitaux.ml:114+`), OR **bit 2** of the `frame_data`
  word. Bits 0/1 are `DEBUG`/`ALLOC` per
  `runtime/caml/frame_descriptors.h:57`; bit 2 is currently free.
- Update `frame_size()` mask in that header to `~0xF` (we'll claim bit 3
  in A.5 for `HAS_CODE_PTR_SLOTS`) and add `FRAME_DESCRIPTOR_UNLOADABLE`
  (0x4).

### A.4 Static data section + header color

See section B.

### A.5 `Code_pointer` Cmm machtype

The frame-descriptor `UNLOADABLE` bit covers the implicit code pointer
that *every* active frame holds (its return address). It does **not**
cover an explicit code-pointer value held in a stack/register slot —
e.g. the indirect-call sequence that loads Field 0 of a closure into a
register, then spills it under register pressure or holds it live
across a safepoint. We need a third mechanism for that, because we do
**not** want to depend on the compiler keeping the parent closure live
across the load-call sequence — the contract is fragile (currying
stubs, effect handler resumption, generic_apply trampolines) and
suspected not to hold universally.

**Add `Code_pointer` to `Cmm.machtype_component`.** One word; not a
heap value. Update exhaustive matches: cmm_invariants, printer,
register allocator, selection, mach/CFG IRs.

**Tag at producers (`backend/cmm_helpers.ml`).** Sites that materialize
a code pointer:

- Closure code-pointer load (Field 0 of a closure for indirect call
  setup).
- Static code-symbol references (`Caddrof_label` or equivalent).

These currently use `Int`/`Addr` machtype; switch to `Code_pointer`.
Thread the new machtype through callers in
`middle_end/flambda2/to_cmm/` (`to_cmm_expr.ml` for indirect calls,
`to_cmm_set_of_closures.ml` for static code refs).

**Register allocator (`backend/cfg/`, `backend/regalloc/`).** Machtype
is a property of pseudoregs, so propagation through copies/moves/spills
is mostly automatic. Verify that:

- The spill-slot allocator does not collapse `Code_pointer` to `Int`.
- Live-set bookkeeping preserves the machtype on each slot.
- Stack-slot record-keeping at safepoints exposes which slots are
  `Code_pointer`-typed for frame descriptor emission.

This is the riskiest plumbing in the plan and worth a small spike
before the rest of the work.

**Frame descriptor live-slot encoding.** Today `live_ofs[]` (per
`emitaux.ml:114+` and `frame_descriptors.h:63–97`) lists offsets, with
stack offsets even and register entries `(reg << 1) | 1`. Add a
parallel **`code_ptr_live_ofs[]`** array using the same encoding
scheme, gated by a new flag bit:

- **Bit 3** of `frame_data` = `HAS_CODE_PTR_SLOTS` (0x8). Independent
  of bit 2 (`UNLOADABLE`): a non-unloadable frame can still spill an
  unloadable code pointer across an indirect call into unloadable
  code, so we need this everywhere.
- After the existing `live_ofs[]` (and after `alloc_lengths`/
  `debug_info` if present), append `n_code_ptr_live` (u16/u32 matching
  short/long format) followed by the array.
- `next_frame_descr()` in `runtime/frame_descriptors.c:36` extends to
  skip this section.

**Use sites are universal, not unloadable-only.** Any code that loads
Field 0 of a closure for an indirect call should tag the result
`Code_pointer`. For non-unloadable targets, the runtime check (see
F.3) is a fast no-op. This keeps the frontend uniform and avoids
needing static knowledge of the call target's CU flavor.

## B. Static data for unloadable CUs

Two changes in the unloadable emit path (AOT path unchanged):

### B.1 Header color

- `backend/cmm_helpers.ml:263`: `caml_black = 3 << 8`, used
  unconditionally by `black_block_header` and at lines 4222–4278 for
  structured constants.
- Add an unloadable-aware variant emitting `Caml_white` (UNMARKED).
- Thread `~unloadable:bool` (or a record carrying it) into the
  emit-block path so this only flips for unloadable CUs.

### B.2 Section

No new section needed. Normal Cmm data items emit into the standard
`.data` section, which the JIT loader already maps writable (only
`.text` is RX and only `.rodata`-prefixed sections are RO; see
`external/ocaml-jit/lib/jit.ml:144–159`).

This means the unmarked-header static data of B.1 and the
`Code_block`s of section C just need to emit as ordinary Cmm data
items — they land in `.data` and get the right protection
automatically.

## C. Code blocks

For each function in an unloadable CU, emit a `Code_block` as an
ordinary Cmm data item (lands in `.data`, which is writable — see
B.2):

- Standard OCaml block layout: header (`Code_block_tag`, `wosize = N`,
  color `UNMARKED`) followed by N value-typed fields.
- Each field is a value pointing to another heap-shaped block:
  - For a code dependency (a direct callee or static code reference):
    the pointer is the dep function's `Code_block` address.
  - For a data dependency: the pointer is the dep block's address (a
    regular static block, also unmarked per B.1).
  - The module block is one of the data fields (compiler invariant; no
    special encoding).
- All fields are regular Vals → standard mark scan recursively darkens
  them. No special arm in `caml_darken` is needed; `Code_block_tag` is
  just a tag for identification (used by the unload bookkeeping pass
  in section G).

### C.1 Dep-list filtering

When computing the dep list during the to_cmm Code_block emission
pass, **exclude code IDs whose `Code_metadata.is_unloadable` is
false**. Non-unloadable callees are always live (they belong to
statically-linked code that can never be unloaded), so listing them is
redundant — it just bloats the `Code_block` and adds redundant darken
calls during marking.

Concretely, when iterating direct callees / static code references:

```
for each dep_code_id:
    if Code_metadata.is_unloadable dep_code_id:
        emit Field pointing to dep's Code_block
    else:
        skip
```

Data dependencies need the same treatment in principle, but in
practice every data symbol referenced by an unloadable function is
emitted as part of some CU; if that CU is non-unloadable the data is
in a `MARKED` static block and listing it is harmless. We can either
filter symmetrically or accept the small over-listing.

**Cross-CU direct references:** assume cross-CU goes through closures
(carrying the closinfo flag). The dep list captures only same-CU
direct edges; this is consistent with the filter above.

**Build site for `Code_block`s.** Flambda 2 already knows direct
callees and referenced data symbols per function. Emit the
`Code_block` from a new pass in `middle_end/flambda2/to_cmm/` that
runs only for unloadable CUs, alongside the existing static-data
emission. This is the same pass that applies the C.1 filter.

**Reserve a new tag in `runtime/caml/mlvalues.h`:** `Code_block_tag`.
Used for safety assertions, the unload bookkeeping pass, and any
future evolution to a mixed layout.

## D. Back-pointer convention (unloadable functions only)

For each labeled entry of an unloadable function — closure Field 0
entries, infix entries, partial-app trampolines — emit one read-only
word at `entry - 1` holding the function's `Code_block` address.

- Compiler emit: extend the function-prologue logic in
  `backend/{amd64,arm64}/emit.ml`, gated on
  `Code_metadata.is_unloadable`.
- All entries of a single function share the same `Code_block` target.
- Non-unloadable functions emit nothing — `.text` is unchanged for
  them.

The back-pointer is consulted only by the GC paths in F.1 and F.2,
both of which already know they're dealing with an unloadable target
(closinfo bit / frame UNLOADABLE bit set). The runtime never
dereferences `*(entry - 1)` for a non-unloadable function.

## E. Runtime registration

- New per-unit registration record in the runtime: list of
  `Code_block` addresses, list of static-data block addresses, list of
  `(text_start, text_end)` ranges, and the unit's frame table pointer.
- New `caml_register_unloadable_unit(...)` called from
  `external/ocaml-jit/lib/jit.ml` after the buffer is mapped — it:
  - Records the unit so the end-of-cycle pass can iterate it.
  - Registers each text range as a code fragment
    (`runtime/caml/codefrag.h:33`), tagged with the unit pointer for
    fast lookup from RA → unit / entry.
  - Registers the unit's frame table.

The runtime structure is much smaller than the original
`metaprog_descriptor` (Appendix A): it owns no per-symbol mark bits
and no dep arrays — those live in the heap-shaped `Code_block`s
themselves.

## F. Mark phase changes

Three narrow injections, all calling the same helper:

```c
static inline void darken_code_block_for(value entry) {
    value code_block = *((value *)entry - 1);
    caml_darken(code_block, ...);
}
```

Standard `caml_darken` then scans the `Code_block`'s fields,
recursively darkening dep `Code_block`s and dep data blocks.

### F.1 Closure scan injection

- `runtime/major_gc.c` lines 931–937 and 1133–1137 (the
  `Start_env_closinfo` skip).
- If `Unloadable_closinfo(info)`:
  `darken_code_block_for(Field(v, 0))`, then fall through to the
  existing env scan.
- For multi-function closures, repeat for each `(code, closinfo)`
  pair in the prefix when the corresponding closinfo is flagged.
- Same change in `runtime/minor_gc.c:~695`.

### F.2 Stack return-address scan

- Frame-iteration sites: `runtime/signals_nat.c:65`,
  `runtime/fiber.c:593`, generic stack scan.
- When `frame_data & FRAME_DESCRIPTOR_UNLOADABLE`:
  - Look up the function entry from the code fragment registered for
    this RA range.
  - `darken_code_block_for(entry)`.
- This fires before scanning `live_ofs[]` so the `Code_block` is
  marked even if the frame happens to have no other references.

### F.3 Stack code-pointer slot scan

- After scanning regular `live_ofs[]` slots: if
  `frame_data & FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS`, walk the parallel
  `code_ptr_live_ofs[]` array.
- For each slot, read the word and check via the code fragment lookup
  whether it points into a registered unloadable text region. If so,
  `darken_code_block_for(slot_value)`. Non-unloadable targets are a
  fast no-op (the lookup miss).
- This branch fires regardless of whether the *current* frame is
  unloadable: a non-unloadable frame can hold an unloadable code
  pointer in flight to an indirect call.

### F.4 Mark propagation semantics

There are no special mark domains. Marking a `Code_block` darkens its
fields via the standard scan — pointers to other `Code_block`s and to
data blocks. All recursion is the standard mark loop.

## G. Unload trigger + reset pass

End-of-major-cycle hook (after `caml_finish_marking`, before sweep
starts):

```
for each registered unloadable unit u:
    live = any Code_block in u is MARKED || any data block in u is MARKED
    if !live:
        schedule_unload(u)
    else:
        for each block b in u: reset b's color MARKED → UNMARKED
```

`schedule_unload`:

- Removes frame table entries (rebuild hashtable in
  `runtime/frame_descriptors.c`).
- Removes the code fragment(s).
- munmaps / frees the text buffer and the data buffer (the `.data`
  region holding `Code_block`s and unmarked static blocks for this
  unit).
- Drops the registration.

Must be **stop-the-world** so no fiber's RA can land in the buffer
mid-unload.

The "all symbols" check is uniform: just iterate the unit's
`Code_block` list and data block list, checking the standard color
bit. Module block reachability is subsumed by the data-block
iteration.

## H. Open issues / risks

1. **Register allocator changes for `Code_pointer` machtype** (A.5):
   the riskiest piece of plumbing in the plan. Worth a small spike
   before committing to the larger plan — confirm regalloc preserves
   the machtype through spill/reload, that we can extract per-slot
   machtype info at safepoint emission, and that the parallel
   `code_ptr_live_ofs[]` array round-trips correctly.
2. **Concurrent marker races**: uses the standard heap color CAS
   pattern in `runtime/major_gc.c` directly. *Status: should be fine
   if atomic.*
3. **Infix closures**: `Infix_tag` blocks must walk back to the master
   closure (via `Infix_offset_val`) to read closinfo. *Status:
   already handled.* Each infix entry needs its own back-pointer at
   `entry - 1`.
4. **Effect handler resumption**: when a fiber resumes into unloadable
   code, the resumption thunk holds a code pointer. Verify
   `runtime/fiber.c` stack-walks use the same frame iteration so they
   pick up the unloadable frame flag automatically. *Status: needs
   checking.*
5. **C callbacks / signal handlers**: an unloadable frame on the
   C–OCaml boundary — the boundary frame is non-unloadable but the
   next frame up is. Frame walker should handle this naturally.
   *Status: write a test.*
6. **`newer_version_of`**: ignore — we'll traverse all relevant code
   IDs anyway, so it should just work out.
7. **Reset cost**: walking every block in every unloadable unit at end
   of cycle to reset marks is O(total unloadable symbols). Bounded
   and infrequent (major GC). *Status: measure.*
8. **Multiple entry points per function**: each labeled entry that
   can be observed by the GC needs its own back-pointer at
   `entry - 1`. Verify against the current Cmm-to-asm path —
   particularly for currying and partial-application stubs.
9. **`Code_block_tag` and existing tag-dispatch sites**: marshaling,
   `Obj`, debugger, `print_value`, compactor — every site that
   pattern-matches on tag needs an arm for the new tag. Mostly fine
   to treat as a regular scannable block, but every dispatch site
   should be visited.
10. **Dynlink applicability**: same machinery should serve Dynlink
    units that opt in to unloadability. Verify nothing in the design
    depends on the JIT-specific load path. The `is_unloadable` flag
    in `Code_metadata` is the natural opt-in; the runtime
    registration path would mirror `caml_register_unloadable_unit`.

## Suggested PR breakdown

**Spike (before committing to the rest):**

- A.5 register-allocator changes: introduce `Code_pointer` machtype,
  confirm it survives spill/reload, prove per-slot machtype info is
  reachable at frame-descriptor emission, prove the parallel
  `code_ptr_live_ofs[]` round-trips. If this is intractable, the rest
  of the plan needs revisiting.

**PR 1 (compiler-side, no runtime semantic change yet):**

- Section A.0: `Clflags.unit_is_unloadable` (internal, not user-CLI);
  wire in `Eval.eval` and `expectnat` so they set it around the
  compilation call.
- Section A.1: `Code_metadata.is_unloadable` field, set from the flag
  during closure conversion, threaded through to_cmm.
- Sections A.2–A.5: closinfo bit; frame descriptor `UNLOADABLE` bit;
  `Code_pointer` machtype + parallel `code_ptr_live_ofs[]` array
  gated by `HAS_CODE_PTR_SLOTS`.
- Section B: unmarked headers + writable section for unloadable
  static data.
- Section C: `Code_block` emission from to_cmm, with C.1 dep-list
  filtering; `Code_block_tag` reservation.
- Section D: back-pointer emission for unloadable function entries.

After this PR, closures carry the closinfo bit, frames carry the
flag bits and code-ptr slot arrays, static data is
unmarked-and-writable, `Code_block`s and back-pointers are emitted
but unused at runtime.

**PR 2 (runtime changes):**

- Section E: registration in JIT loader, runtime per-unit record.
- Section F: closure-scan, RA-scan, and code-ptr-slot-scan injections
  via `darken_code_block_for`.
- Section G: end-of-cycle unload trigger + reset pass.

## Testing

Tests live in `testsuite/tests/quotation/eval/` (existing directory)
and are driven by `expectnat`
(`oxcaml/testsuite/tools/expectnat.ml`). The driver sets
`Clflags.unit_is_unloadable := true` around the compilation of each
phrase so the JIT-compiled output is eligible for unloading.

Existing tests (`eval_test.ml`, `plus.ml`, `stack_in_splice.ml`,
`type_variable.ml`, `no_stdlib.ml`) should be re-runnable unchanged
once the flag is wired in — they exercise the *correctness* axis.
Add new tests for the *unloading* axis:

1. **Smoke**: compile a small CU via `Eval.eval`, drop all
   user-visible references, force a major GC, and assert via a
   runtime hook (e.g. `Gc.unloadable_units_count` or a
   test-only callback registered with the unload pass) that the
   unit was reclaimed.
2. **Reachability via closure**: hold a reference to a closure
   produced by the CU; force GC; assert the unit is *not* unloaded.
   Drop the closure; force GC; assert it *is* unloaded.
3. **Reachability via static data**: hold a reference to a static
   string / block defined in the CU; same pattern.
4. **Reachability via code pointer in a stack slot**: arrange a call
   sequence that spills the CU's code pointer across an allocation
   safepoint (the case A.5 / F.3 specifically address); assert the
   unit stays live during execution and unloads afterward. This is
   the test most likely to catch a regression in the regalloc
   plumbing.
5. **Cross-CU**: load two unloadable CUs, where one references the
   other through a closure; verify they unload in the right order.
6. **Effect handler / fiber resumption**: an unloadable closure
   suspended via an effect handler stays live; resuming and
   completing then allows unload. (Covers H.4.)
7. **C callback**: an unloadable function reached via a C
   callback / signal context stays live during the callback.
   (Covers H.5.)

For tests 1–3 the `expectnat` driver is sufficient. Tests 4–7 may
need a more direct harness because `expectnat`'s phrase-at-a-time
model doesn't naturally exercise stack-slot timing or fibers; those
can live next to `expectnat` as a small standalone test program that
uses `Eval.eval` directly.

A test-only runtime hook (counter or callback) for "this
`schedule_unload` just fired for unit X" makes the assertions
straightforward; gate behind a debug build flag if production cost
is a concern.

## Reference: key source locations

| Concern | File | Lines |
|---|---|---|
| Closinfo layout & macros | `runtime/caml/mlvalues.h` | 463–491 |
| Object/system tags | `runtime/caml/mlvalues.h` | 449–483 |
| Code_metadata type | `middle_end/flambda2/terms/code_metadata.ml` | 17–48 |
| Backend pack_closinfo | `backend/cmm_helpers.ml` | 357–378, 3984 |
| CMM set-of-closures emit | `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml` | 249–251, 313–315, ~1170 |
| Static black header | `backend/cmm_helpers.ml` | 263, 4222–4278 |
| Frame descriptor layout | `runtime/caml/frame_descriptors.h` | 57–97, 113–140, 153–205 |
| Frame descr emit (frontend) | `backend/emitaux.ml` | 44–50, 78, 114–351 |
| Frame descr callers | `backend/amd64/emit.ml`, `backend/arm64/emit.ml` | 616, 741 |
| Function prologue emit (back-pointer site) | `backend/amd64/emit.ml`, `backend/arm64/emit.ml` | — |
| Cmm machtype | `backend/cmm.ml` (`machtype_component`) | — |
| Indirect-call code-ptr load | `backend/cmm_helpers.ml`, `middle_end/flambda2/to_cmm/to_cmm_expr.ml` | — |
| Register allocator | `backend/cfg/`, `backend/regalloc/` | — |
| Code fragment registration | `runtime/caml/codefrag.h` | 33–45 |
| JIT load path | `external/ocaml-jit/lib/jit.ml` | 76, 144–159, 168–196, 211–276 |
| JIT backend dispatch | `backend/jit_backend.ml` | 54–121 |
| Eval entry point | `otherlibs/eval/eval.ml` | 117–232 |
| `expectnat` test driver | `oxcaml/testsuite/tools/expectnat.ml` | — |
| Existing quotation tests | `testsuite/tests/quotation/eval/` | — |
| Major GC closure scan | `runtime/major_gc.c` | 931–937, 1133–1137 |
| Major GC darken loop | `runtime/major_gc.c` | 998–1044 |
| Minor GC closure scan | `runtime/minor_gc.c` | ~695 |
| Stack walk sites | `runtime/signals_nat.c`, `runtime/fiber.c` | 65, 593 |
| Heap colors | `runtime/caml/shared_heap.h` | 70–78 |
| Metaprog flag (driver) | `utils/clflags.ml` | 117–118 |
