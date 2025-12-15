# Dissector Implementation Plan

## Overview

The dissector is a new compiler pass that computes the total size of allocated
ELF sections across all object files involved in a link. This information will
be used to partition object files to prevent relocation overflow when linking
very large executables with the small code model.

## Files to Create

Directory: `asmcomp/dissector/`

1. `asmcomp/dissector/dissector.mli` - Main interface
2. `asmcomp/dissector/dissector.ml` - Main implementation
3. `asmcomp/dissector/dune` - Build configuration

All OCaml files with MIT license headers (no author attribution), following the
style in `backend/debug/dwarf/dwarf_ocaml/dwarf_inlined_frames.ml`.

## Key Requirements

### 1. Linux-only Check

Use `Target_system.system ()` to verify the target is `Linux`. If not, raise
an error when the dissector is invoked. The dissector flag can be set on any
platform, but attempting to actually run it on non-Linux should fail.

### 2. ELF Section Flags Support in Owee

Add to `external/owee/owee_elf.ml{,i}`:

```ocaml
module Section_flags : sig
  type t = int64

  val shf_write : t      (* 0x1 - Writable *)
  val shf_alloc : t      (* 0x2 - Occupies memory during execution *)
  val shf_execinstr : t  (* 0x4 - Executable *)

  val is_set : t -> flag:t -> bool
  val is_allocated : t -> bool  (* Convenience for checking SHF_ALLOC *)
end
```

Reference: [ELF man page](https://man7.org/linux/man-pages/man5/elf.5.html)

### 3. Main Interface

```ocaml
(** Result of running the dissector. The object file lists may be modified
    (e.g., partitioned) compared to the inputs. *)
type result = {
  ml_objfiles : string list;
  startup_obj : string;
}

(** Run the dissector pass.

    Analyzes all object files that will be involved in a link, computes
    the total size of allocated ELF sections, and potentially modifies
    the object file lists (e.g., for partitioning).

    The startup_obj is analyzed like all other .o files but may need to
    be handled specially during partitioning.

    @param unix First-class Unix module for file operations
    @param ml_objfiles The OCaml object files (.o, .a, .cmx, .cmxa)
    @param startup_obj The startup object file
    @param ccobjs Extra C object files from -cclib (Clflags.ccobjs)
    @param runtime_libs Runtime libraries (from runtime_lib ())
    @param cached_genfns Optional path to cached generic functions

    Raises an error if Target_system is not Linux. *)
val run
  :  unix:(module Unix_intf.S)
  -> ml_objfiles:string list
  -> startup_obj:string
  -> ccobjs:string list
  -> runtime_libs:string list
  -> cached_genfns:string option
  -> result
```

Note: For now, `run` will just compute and print the total size, returning
the inputs unchanged. The interface allows for future partitioning work.

### 4. File Type Handling

The `run` function must handle different file types:

| Extension | Action |
|-----------|--------|
| `.o` | Use `Compiler_owee.Owee_elf` to read sections, sum allocated sizes |
| `.a` | Use `Compiler_owee.Owee_archive` to iterate members, sum allocated sizes of all `.o` files |
| `.cmx` | Find associated `.o` file (same basename), analyze that |
| `.cmxa` | (a) Find `.o` for each `.cmx` in `lib_units`; (b) Analyze `lib_ccobjs` (extra C objects) |

For `.cmxa` files, need to read the library header using `Compilenv` or similar
to extract `lib_units` and `lib_ccobjs`.

### 5. Computing Allocated Section Size

For a single `.o` file:

```ocaml
let compute_allocated_size buf =
  let _header, sections = Compiler_owee.Owee_elf.read_elf buf in
  Array.fold_left (fun acc section ->
    if Section_flags.is_allocated section.sh_flags
    then Int64.add acc section.sh_size
    else acc
  ) 0L sections
```

### 6. Integration Point

In `asmcomp/asmlink.ml`, between lines 367 and 368 (after `Emitaux.reduce_heap_size`
and before `Misc.try_finally` with `call_linker`).

The module will be accessible as `Dissector` (from `asmcomp/dissector/dissector.ml`):

```ocaml
  Emitaux.reduce_heap_size ~reset:(fun () -> ());
  (* Dissector pass (may modify ml_objfiles and startup_obj) *)
  let ml_objfiles, startup_obj =
    if !Clflags.dissector then begin
      let cached_genfns =
        if !Oxcaml_flags.use_cached_generic_functions
        then Some !Oxcaml_flags.cached_generic_functions_path
        else None
      in
      let result = Profile.record_call "dissector" (fun () ->
        Dissector.run
          ~unix
          ~ml_objfiles
          ~startup_obj
          ~ccobjs:(List.rev !Clflags.ccobjs)
          ~runtime_libs:(runtime_lib ())
          ~cached_genfns
      ) in
      result.ml_objfiles, result.startup_obj
    end else
      ml_objfiles, startup_obj
  in
  Misc.try_finally
    (fun () -> call_linker ml_objfiles startup_obj output_name)
    ...
```

### 7. Owee Module Access

The owee library is accessible as `Compiler_owee`. Example usage from
`backend/internal_assembler/section_table.ml`:

```ocaml
open Compiler_owee.Owee_elf
(* or *)
let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
let header, sections = Compiler_owee.Owee_elf.read_elf buf in
```

Note: `Owee_buf.map_binary` takes a first-class module for Unix operations.
In `asmlink.ml`, the `unix` parameter is already available and can be passed
through.

### 8. Reading .cmxa Files

Look at how `Asmlink` or `Compilenv` reads `.cmxa` files to get `library_infos`.
The `lib_units` field contains `lib_unit_info list` with compilation unit names,
and `lib_ccobjs` contains extra C object file paths.

## Implementation Steps

1. Add `Section_flags` module to `owee_elf.ml{,i}`
2. Create `asmcomp/dissector/dune`
3. Create `asmcomp/dissector/dissector.mli` with `result` type and `run` signature
4. Create `asmcomp/dissector/dissector.ml` with:
   - Linux target check
   - Helper to compute allocated size for single `.o`
   - Helper to handle `.a` archives
   - Helper to handle `.cmx` (find associated `.o`)
   - Helper to handle `.cmxa` (read header, process units + ccobjs)
   - Main `run` function (for now, just computes total and returns inputs unchanged)
5. Add call site in `asmlink.ml`
6. Build and test

## Future Work

The plan will be extended later to describe the rest of the dissector
functionality (partitioning, etc.).
