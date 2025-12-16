# OCaml Implementation Summary (`asmcomp/dissector/`)

## Overall Flow (in `dissector.ml`)

1. **Measure Files** (`measure_object_files.ml`): Computes allocated section sizes for .o/.a/.cmx/.cmxa, handles lib_ccobjs recursively
2. **Partition Files** (`partition_object_files.ml`): Bins files into 1 GiB partitions
3. **Partial Link** (`partial_link.ml`): Runs `Config.native_pack_linker` with `--whole-archive`
4. **Extract Relocations** (`extract_relocations.ml`): Parses .rela.text for PLT32/GOTPCRELX on undefined symbols
5. **Build IGOT/IPLT** (`build_igot_and_iplt.ml`): Creates synthetic GOT/PLT data structures
6. **Plan Rewrite** (`form_rewrite_plan.ml`): Computes file layout, symbol mappings, relocation rewrites
7. **Rewrite Sections** (`rewrite_sections.ml`): Writes new ELF with IGOT/IPLT sections
8. **Generate Linker Script** (`linker_script.ml`): Writes SECTIONS directives
9. **Build Linker Args** (`build_linker_args.ml`): Returns partition files + linker script flag

## Module Breakdown

| Module | Purpose |
|--------|---------|
| `measure_object_files` | Measures allocated section sizes, handles .cmxa lib_ccobjs recursively |
| `partition_object_files` | Partitions by size threshold (default 1 GiB, configurable) |
| `partition` | Type definitions for partitions |
| `partial_link` | Runs `ld -r --whole-archive` via response files |
| `extract_relocations` | Finds PLT32/REX_GOTPCRELX for undefined symbols |
| `igot` | IGOT entries: 8-byte zero-filled slots + R_X86_64_64 relocs |
| `iplt` | IPLT entries: `jmp [rip+disp32]; nop; nop` + PC32 relocs |
| `build_igot_and_iplt` | Orchestrates IGOT/IPLT construction |
| `form_rewrite_plan` | Computes layout, symbol indices, rewritten .rela.text |
| `rewrite_sections` | Writes output ELF with new sections |
| `linker_script` | Generates linker script for partition sections |
| `build_linker_args` | Produces final linker args (partition files + -Wl,-T,script) |
| `dissector` | Main entry point, orchestrates the entire flow |

## Symbol Naming Convention

- IGOT symbols: `igot🐍{prefix}🐍{original}` (using snake emoji U+1F40D as delimiter)
- IPLT symbols: `iplt🐍{prefix}🐍{original}`

The delimiter is chosen to be unlikely to appear in normal symbol names.

## Section Naming

- IGOT: `.data.igot`, `.rela.data.igot`
- IPLT: `.text.iplt`, `.rela.text.iplt`

The "i" prefix stands for "intermediate" to distinguish from the real GOT/PLT.

## Relocation Identification

From `extract_relocations.ml`:

```ocaml
if Int64.equal entry.r_type Rela.r_x86_64_plt32
   || Int64.equal entry.r_type Rela.r_x86_64_rex_gotpcrelx
then
  match Rela.read_symbol_shndx ~symtab_body ~sym_index:entry.r_sym with
  | None -> ()
  | Some shndx when shndx <> Rela.shn_undef -> ()  (* skip defined symbols *)
  | Some _ -> (* process undefined symbol *)
```

Only relocations for **undefined symbols** (`st_shndx = SHN_UNDEF`) need synthetic GOT/PLT entries.

## PLT Entry Format

From `iplt.ml`, uses `X86_binary_emitter` to generate:

```ocaml
X86_ast.JMP (X86_ast.Mem64_RIP (X86_ast.QWORD, "dummy", 0));
X86_ast.NOP;
X86_ast.NOP
```

This produces: `ff 25 00 00 00 00 90 90` (8 bytes)

## Relocation Rewriting

| Original | New | Target |
|----------|-----|--------|
| PLT32 | PC32 | IPLT symbol |
| REX_GOTPCRELX | PC32 | IGOT symbol |

Additionally:
- IGOT entries get `R_X86_64_64` relocations (absolute 64-bit address to original symbol)
- IPLT entries get `PC32` relocations to their corresponding IGOT entry (with addend -4)

## File Layout Computation

From `form_rewrite_plan.ml`, the rewritten file layout is:

1. Original file data (copied verbatim)
2. `.data.igot` section (16-byte aligned)
3. `.rela.data.igot` section (8-byte aligned)
4. `.text.iplt` section (16-byte aligned)
5. `.rela.text.iplt` section (8-byte aligned)
6. `.symtab` section (8-byte aligned, includes new symbols)
7. `.strtab` section (includes new symbol names)
8. `.rela.text` section (rewritten relocations)
9. `.shstrtab` section (includes new section names)
10. Section headers (8-byte aligned)

## Symbol Table Updates

New symbols added for IGOT/IPLT entries:
- Binding: `GLOBAL`
- Type: `NOTYPE` for IGOT, `FUNC` for IPLT
- Visibility: `HIDDEN` (st_other = 2)

## Linker Script Generation

From `linker_script.ml`:

```
SECTIONS {
  .caml.p1.text : { *(.caml.p1.text) *(.caml.p1.text.*) }
  .caml.p1.rodata : { *(.caml.p1.rodata) *(.caml.p1.rodata.*) }
  .caml.p1.data : { *(.caml.p1.data) *(.caml.p1.data.*) }
  .caml.p1.bss : { *(.caml.p1.bss) *(.caml.p1.bss.*) }
  .caml.p1.eh_frame : { *(.caml.p1.eh_frame) *(.caml.p1.eh_frame.*) }
} INSERT AFTER .bss
```

## Integration with Compiler

The dissector integrates into `asmlink.ml`:
- Enabled via `-dissector` flag
- Verbose logging via `-ddissector` flag
- Partition size configurable via `-dissector-partition-size`

When enabled, `call_linker` receives the partition files and linker script flag instead of the original object files.

## Partition Kind

The OCaml implementation uses an explicit variant type to distinguish partitions:

```ocaml
type kind =
  | Main          (* Sections keep original names, placed at low addresses *)
  | Large_code of int  (* Sections renamed .caml.pN.*, placed after .bss *)
```

Helper functions:
- `Partition.symbol_prefix kind` → `"main"` or `"p1"`, `"p2"`, etc.
- `Partition.section_prefix kind` → `""` or `".caml.p1"`, `".caml.p2"`, etc.

## Section Renaming

For `Large_code` partitions, sections are renamed in `form_rewrite_plan.ml`:
- `.text` → `.caml.p1.text`
- `.data` → `.caml.p1.data`
- `.rela.text` → `.rela.caml.p1.text`
- etc.

The `Main` partition keeps original section names.

## Feature Complete

The OCaml implementation now has full feature parity with the Python prototype.
