# Dissector Implementation Comparison

Comparison between the Python prototype (`synthetic_got.py`) and the OCaml implementation (`asmcomp/dissector/`).

## Feature Comparison Table

| Aspect | Python | OCaml | Status |
|--------|--------|-------|--------|
| **File Measurement** | `read_elf_section_names_and_alloc_size` | `measure_object_files.analyze_elf_buf` | ✅ Done |
| **lib_ccobjs handling** | Not explicitly shown | ✅ Recursive via `read_cmxa` | ✅ Done |
| **Probes handling** | ✅ Files with `.probes` → main bucket | ✅ Files with `.probes` → Main partition | ✅ Done |
| **Partition threshold** | 1 GiB hardcoded | 1 GiB default, configurable | ✅ Done |
| **Partial linker** | `ld.lld` | `Config.native_pack_linker` | ✅ Done |
| **Symbol delimiter** | Snake emoji 🐍 | Snake emoji 🐍 | ✅ Done |
| **GOT section name** | `.data.got` | `.data.igot` | ✅ Done |
| **PLT section name** | `.text.plt` | `.text.iplt` | ✅ Done |
| **PLT NOP padding** | `66 90` (prefixed nop) | `90 90` (two plain nops) | ✅ OK (functionally equivalent) |
| **Symbol visibility** | GLOBAL + HIDDEN | GLOBAL + HIDDEN | ✅ Done |
| **Partition kind type** | Implicit (index 0 = main) | ✅ `Partition.kind` variant | ✅ Done |
| **Symbol prefix per partition** | ✅ "main", "1", "2", ... | ✅ "main", "p1", "p2", ... | ✅ Done |
| **Section prefix renaming** | ✅ `.text` → `.caml.p1.text` | ✅ `.text` → `.caml.p1.text` | ✅ Done |
| **Main partition (no rename)** | ✅ Main keeps original names | ✅ Main keeps original names | ✅ Done |
| **Existing linker script** | ✅ Extracted from args | ✅ Extracted from `Clflags.all_ccopts` | ✅ Done |
| **Verbose logging** | None | ✅ `-ddissector` flag | ✅ Done (OCaml bonus) |

## Detailed Differences

### 1. Probes Handling

**Python**: Files containing a `.probes` section are placed in the "main" bucket.

**OCaml**: ✅ Now implemented. Files with `.probes` sections are detected in `measure_object_files.ml`
and forced into the Main partition in `partition_object_files.ml`.

### 2. Section Prefix Renaming

**Python**: Renames sections in non-main partitions:
```python
def replace_names(names: pl.Expr, to_replace: list[str], prefix: str) -> pl.Expr:
    # .text → .caml.p1.text, .data → .caml.p1.data, etc.
```

**OCaml**: ✅ Now implemented in `form_rewrite_plan.ml`:
```ocaml
let rename_section ~partition_kind name =
  match partition_kind with
  | Partition.Main -> name  (* No renaming for main partition *)
  | Partition.Large_code _ ->
    let prefix = Partition.section_prefix partition_kind in
    (* .text -> .caml.p1.text, .rela.text -> .rela.caml.p1.text *)
```

**Status**: Both implementations now rename sections for non-main partitions.

### 3. Symbol Visibility

**Python**: Uses GLOBAL binding with HIDDEN visibility.

**OCaml**: ✅ Now matches Python - uses GLOBAL binding with HIDDEN visibility (st_other = 2).

### 4. Existing Linker Script Extraction

**Python**: Extracts `--script=` from linker arguments.

**OCaml**: ✅ Now implemented. Extracts linker script from `Clflags.all_ccopts` by looking for
`-Wl,-T,<path>` or `-Wl,--script=<path>` patterns.

### 5. PLT Entry Encoding

**Python**:
```python
plt_data = np.full((len(plt),), 0xFF25_00000000_6690, dtype=">u8")
# ff 25 00 00 00 00 66 90
```

**OCaml**:
```ocaml
X86_ast.JMP (X86_ast.Mem64_RIP (X86_ast.QWORD, "dummy", 0));
X86_ast.NOP; X86_ast.NOP
# ff 25 00 00 00 00 90 90
```

**Impact**: Both are valid 8-byte sequences. The `66 90` is a 2-byte NOP using operand size prefix, while `90 90` is two 1-byte NOPs. Functionally equivalent.

## All Features Complete

The OCaml implementation now has feature parity with the Python prototype.

## Completed Features

| Feature | Location | Notes |
|---------|----------|-------|
| File measurement | `measure_object_files.ml` | Sums allocated section sizes |
| lib_ccobjs handling | `measure_object_files.ml` | Recursive via `read_cmxa` |
| Probes handling | `measure_object_files.ml`, `partition_object_files.ml` | Files with `.probes` → Main |
| Partition by size | `partition_object_files.ml` | Configurable threshold |
| Main partition kind | `partition.ml` | `Main` vs `Large_code of int` |
| Section renaming | `form_rewrite_plan.ml` | `.text` → `.caml.p1.text` for non-main |
| Symbol prefixes | `partition.ml` | `"main"`, `"p1"`, `"p2"`, etc. |
| Symbol visibility | `rewrite_sections.ml` | GLOBAL + HIDDEN |
| Symbol delimiter | `igot.ml`, `iplt.ml` | Snake emoji 🐍 |
| IGOT/IPLT generation | `igot.ml`, `iplt.ml`, `build_igot_and_iplt.ml` | With partition-specific prefixes |
| Relocation rewriting | `form_rewrite_plan.ml`, `rewrite_sections.ml` | PLT32→PC32, GOTPCRELX→PC32 |
| Linker script generation | `linker_script.ml` | SECTIONS with INSERT AFTER .bss |
| Existing linker script | `dissector.ml` | Extracts from `-Wl,-T,<path>` in ccopts |
| Verbose logging | `-ddissector` flag | OCaml bonus |
| Configurable partition size | `-dissector-partition-size` flag | OCaml bonus |

## Non-Issues (Acceptable Differences)

| Difference | Why it's OK |
|------------|-------------|
| PLT padding (`66 90` vs `90 90`) | Functionally equivalent NOP sequences |
| Section names (`.data.got` vs `.data.igot`) | "i" prefix is clearer, avoids confusion with real GOT |
