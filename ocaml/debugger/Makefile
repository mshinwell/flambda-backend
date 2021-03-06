#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR = ..

-include $(ROOTDIR)/Makefile.config
-include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

DYNLINKDIR=$(ROOTDIR)/otherlibs/dynlink
UNIXDIR=$(ROOTDIR)/otherlibs/$(UNIXLIB)
CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/yacc/ocamlyacc$(EXE)

CAMLC=$(BEST_OCAMLC) -g -nostdlib -I $(ROOTDIR)/stdlib
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error A \
          -safe-string -strict-sequence -strict-formats
LINKFLAGS=-linkall -I $(UNIXDIR) -I $(DYNLINKDIR)
YACCFLAGS=
CAMLLEX=$(BEST_OCAMLLEX)
CAMLDEP=$(BEST_OCAMLDEP)
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

DIRECTORIES=$(UNIXDIR) $(DYNLINKDIR) $(addprefix $(ROOTDIR)/,\
  utils parsing typing bytecomp toplevel driver file_formats lambda)

INCLUDES=$(addprefix -I ,$(DIRECTORIES))

compiler_modules := $(ROOTDIR)/toplevel/genprintval

debugger_modules := \
  int64ops primitives unix_tools debugger_config parameters debugger_lexer \
  input_handling question debugcom exec source pos checkpoints events \
  program_loading symbols breakpoints trap_barrier history printval \
  show_source time_travel program_management frames eval \
  show_information loadprinter debugger_parser command_line main

all_modules := $(compiler_modules) $(debugger_modules)

all_objects := $(addsuffix .cmo,$(all_modules))

libraries = $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(UNIXDIR)/unix.cma $(DYNLINKDIR)/dynlink.cma

all: ocamldebug$(EXE)

ocamldebug$(EXE): $(libraries) $(all_objects)
	$(CAMLC) $(LINKFLAGS) -o $@ -linkall $^

install:
	$(INSTALL_PROG) ocamldebug$(EXE) "$(INSTALL_BINDIR)/ocamldebug$(EXE)"

clean::
	rm -f ocamldebug ocamldebug.exe
	rm -f *.cmo *.cmi

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) *.mli *.ml \
	| sed -e 's,$(UNIXDIR)/,$$(UNIXDIR)/,' > .depend

debugger_lexer.ml: debugger_lexer.mll
	$(CAMLLEX) $(OCAMLLEX_FLAGS) $<
clean::
	rm -f debugger_lexer.ml
beforedepend:: debugger_lexer.ml

debugger_parser.ml debugger_parser.mli: debugger_parser.mly
	$(CAMLYACC) debugger_parser.mly
clean::
	rm -f debugger_parser.ml debugger_parser.mli
beforedepend:: debugger_parser.ml debugger_parser.mli

include .depend
