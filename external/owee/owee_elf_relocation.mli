(** ELF relocation section parsing.

    This module provides support for reading RELA (relocations with addends)
    sections from ELF files. *)

(** {1 Section Types} *)

val sht_rela : int
(** Section type for RELA sections (relocations with addends). *)

val sht_symtab : int
(** Section type for symbol table sections. *)

val shn_undef : int
(** Special section index indicating an undefined symbol. *)

(** {1 x86-64 Relocation Types} *)

val r_x86_64_plt32 : int64
(** R_X86_64_PLT32 relocation type. *)

val r_x86_64_rex_gotpcrelx : int64
(** R_X86_64_REX_GOTPCRELX relocation type. *)

(** {1 RELA Entry Parsing} *)

(** A parsed RELA entry. *)
type rela_entry =
  { r_offset : int64;
    (** Offset within the section being relocated. *)
    r_sym : int;
    (** Symbol table index. *)
    r_type : int64;
    (** Relocation type. *)
    r_addend : int64
    (** Addend for the relocation. *)
  }

(** [iter_rela_entries ~rela_body ~f] iterates over all RELA entries in
    the given section body, calling [f] for each entry. *)
val iter_rela_entries : rela_body:Owee_buf.t -> f:(rela_entry -> unit) -> unit

(** {1 Symbol Name Lookup} *)

(** [read_symbol_name ~symtab_body ~strtab_body ~sym_index] reads the name
    of the symbol at the given index from the symbol table.

    Returns [None] if the index is out of bounds or the name cannot be read. *)
val read_symbol_name :
  symtab_body:Owee_buf.t -> strtab_body:Owee_buf.t -> sym_index:int -> string option

(** [read_symbol_shndx ~symtab_body ~sym_index] reads the section header index
    (st_shndx) of the symbol at the given index.

    Returns [None] if the index is out of bounds.
    A value of [shn_undef] (0) indicates an undefined symbol. *)
val read_symbol_shndx : symtab_body:Owee_buf.t -> sym_index:int -> int option
