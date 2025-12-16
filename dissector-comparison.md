# Dissector Implementation Comparison

Comparison between the Python prototype (`synthetic_got.py`) and the OCaml implementation (`asmcomp/dissector/`).

## Feature Comparison Table

| Aspect | Python | OCaml | Notes |
|--------|--------|-------|-------|
| **File Measurement** | `read_elf_section_names_and_alloc_size` | `measure_object_files.allocated_size_of_elf_buf` | Both sum allocated section sizes |
| **lib_ccobjs handling** | Not explicitly shown | ✅ Recursive via `read_cmxa` | OCaml handles .cmxa transitive deps |
| **Probes handling** | ✅ Files with `.probes` → main bucket | ❌ Not implemented | Python ensures probes stay together |
| **Partition threshold** | 1 GiB hardcoded | 1 GiB default, configurable | OCaml has `-dissector-partition-size` |
| **Partial linker** | `ld.lld` | `Config.native_pack_linker` | OCaml uses configured linker |
| **Symbol delimiter** | Snake emoji 🐍 | Lightning bolt ⚡ | Both use unlikely characters |
| **GOT section name** | `.data.got` | `.data.igot` | OCaml uses "i" for intermediate |
| **PLT section name** | `.text.plt` | `.text.iplt` | OCaml uses "i" for intermediate |
| **PLT NOP padding** | `66 90` (prefixed nop) | `90 90` (two plain nops) | Minor difference |
| **Symbol visibility** | GLOBAL + HIDDEN | LOCAL | Different approaches |
| **Partition kind type** | Implicit (index 0 = main) | ✅ `Partition.kind` variant | OCaml uses explicit variant type |
| **Symbol prefix per partition** | ✅ "main", "1", "2", ... | ✅ "main", "p1", "p2", ... | Both use unique prefixes |
| **Section prefix renaming** | ✅ `.text` → `.caml.p1.text` | ✅ `.text` → `.caml.p1.text` | Both implemented |
| **Main partition (no rename)** | ✅ Main keeps original names | ✅ Main keeps original names | Both skip renaming for Main |
| **Existing linker script** | ✅ Extracted from args | ❌ TODO, always None | Gap in OCaml |
| **Verbose logging** | None | ✅ `-ddissector` flag | OCaml has better debugging |

## Detailed Differences

### 1. Probes Handling

**Python**: Files containing a `.probes` section are placed in the "main" bucket:
```python
for archive in ocaml_archives:
    has_probes, archive_size = archive_info[archive]
    if has_probes:
        main_bucket.append(archive)
```

**OCaml**: No special handling for probes. All files are partitioned purely by size.

**Impact**: May cause issues if probes need to be in specific locations.

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

**Python**: Uses GLOBAL binding with HIDDEN visibility:
```python
st_bind=pl.lit("GLOBAL", dtype=st_bind_enum),
st_visibility=pl.lit("HIDDEN", dtype=st_visibility_enum),
```

**OCaml**: Uses LOCAL binding:
```ocaml
st_info = Rela.make_st_info ~binding:Rela.Stb.local ~typ:...
```

**Impact**: LOCAL symbols are not visible outside the object file, which should be fine since they're only referenced internally.

### 4. Existing Linker Script Extraction

**Python**: Extracts `--script=` from arguments:
```python
for i in range(len(args) - 1):
    if args[i] == "-Xlinker" and args[i + 1].startswith("--script"):
        script_index = i
if script_index is not None:
    linker_script = args[script_index + 1]
```

**OCaml**: Has a TODO comment but always passes `None`:
```ocaml
(* TODO: Extract existing_script from linker command line flags
   (--script=<path>) *)
Linker_script.write ~output_file:linker_script ~existing_script:None
```

**Impact**: If the user provides a linker script, it won't be incorporated.

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

## Missing Features in OCaml Implementation

1. ~~**Section renaming**: Sections need to be renamed with partition prefix~~ ✅ **DONE**

2. **Probes handling**: Files with `.probes` should go to main bucket

3. **Existing linker script**: Need to extract `--script=` from command line and include it

## Additional Features in OCaml Implementation

1. **Configurable partition size**: `-dissector-partition-size` flag

2. **Verbose logging**: `-ddissector` flag for debugging

3. **lib_ccobjs handling**: Recursively processes .cmxa dependencies

4. **Better integration**: Hooks into the compiler's native linking flow

## Recommendations

### High Priority

1. ~~**Implement section renaming** in `rewrite_sections.ml`~~ ✅ **DONE**
   - For non-main partitions, rename `.text` → `.caml.pN.text` etc.
   - Also rename `.rela.text` → `.rela.caml.pN.text`

2. **Extract existing linker script** in `dissector.ml` or `build_linker_args.ml`:
   - Parse `Clflags.all_ccopts` for `--script=` or `-T`
   - Pass to `Linker_script.write`

### Medium Priority

3. **Add probes handling** in `partition_object_files.ml`:
   - Check for `.probes` section in files
   - Force files with probes into partition 0 (main)

### Low Priority

4. **Consider symbol visibility**: Evaluate whether GLOBAL+HIDDEN or LOCAL is better

5. **PLT padding**: The current `90 90` is fine, but could match Python's `66 90` for consistency
