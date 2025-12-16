# Python Prototype Summary (`synthetic_got.py`)

## Overall Flow

1. **Argument Classification** (`classify_args`): Parses linker command line, categorizing args as ARCHIVE, OBJECT, LIBRARY, or FLAG
2. **Argument Mangling** (`mangle_args`): Separates "in-tree" OCaml files from system files, extracts existing linker script
3. **Archive Checking** (`check_archive`): Measures allocated section sizes, detects `.probes` sections
4. **Partitioning**: Bins archives into buckets of max 1 GiB, files with probes go to "main" bucket
5. **Partial Linking**: Runs `ld.lld --whole-archive @list --relocatable -o partition.o` for each bucket
6. **GOT/PLT Building** (`build_got_and_plt`): Analyzes .rela.text, builds synthetic GOT and PLT
7. **ELF Rewriting** (`encode_data`, `write_elf`): Rewrites sections with new symtab/strtab/rela
8. **Linker Script Generation**: Writes SECTIONS directives for non-main partitions
9. **Final Link**: Invokes gcc with patched args and `-Xlinker --script=...`

## Key Data Structures

Uses Polars DataFrames with categorical columns for symbol names (efficient string interning).

### Relocation Identification

```python
convert_to_plt = (pl.col("relocation").struct["r_type"] == "PLT32") & (
    pl.col("relocation").struct["r_section"].is_null()
)
convert_to_got = (pl.col("relocation").struct["r_type"] == "REX_GOTPCRELX") & (
    pl.col("relocation").struct["r_section"].is_null()
)
```

Only relocations for **undefined symbols** (where `r_section` is null) need synthetic GOT/PLT entries.

## Symbol Naming Convention

- GOT symbols: `got🐍{prefix}🐍{original}` (using snake emoji as delimiter)
- PLT symbols: `plt🐍{prefix}🐍{original}`

## Section Naming

- GOT: `.data.got`, `.rela.data.got`
- PLT: `.text.plt`, `.rela.text.plt`
- Partition prefixes: `.caml.{block}` for sections (e.g., `.caml.p1.text`)

## PLT Entry Format

```python
plt_data = np.full((len(plt),), 0xFF25_00000000_6690, dtype=">u8")
```

This encodes:
- `ff 25 00 00 00 00` - `jmp [rip + disp32]` (6 bytes)
- `66 90` - 2-byte nop (operand size prefix + nop)

## Relocation Rewriting

| Original | New | Target |
|----------|-----|--------|
| PLT32 | PC32 | IPLT symbol |
| REX_GOTPCRELX | PC32 | IGOT symbol |

Additionally:
- GOT entries get `R_X86_64_64` relocations (absolute 64-bit address to original symbol)
- PLT entries get `PC32` relocations to their corresponding GOT entry (with addend -4)

## Section Prefix Renaming

For non-main partitions, sections are renamed:
- `.text` → `.caml.p1.text`
- `.data` → `.caml.p1.data`
- `.rodata` → `.caml.p1.rodata`
- `.bss` → `.caml.p1.bss`
- `.eh_frame` → `.caml.p1.eh_frame`

And their `.rela.*` counterparts are similarly renamed.

## Linker Script Generation

```
SECTIONS {
  .caml.p1.text : { *(.caml.p1.text) *(.caml.p1.text.*) }
  .caml.p1.rodata : { *(.caml.p1.rodata) *(.caml.p1.rodata.*) }
  ...
} INSERT AFTER .bss
```

## Probes Handling

Files containing a `.probes` section are placed in the "main" bucket to ensure they're handled correctly.

## Existing Linker Script Handling

If `-Xlinker --script=<path>` is found in the arguments:
1. Extract the path
2. Include the contents at the beginning of the generated linker script
3. Remove the flag from the arguments passed to the final link
