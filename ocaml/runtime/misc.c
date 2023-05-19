/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#if _MSC_VER >= 1400 && _MSC_VER < 1700
/* Microsoft introduced a regression in Visual Studio 2005 (technically it's
   not present in the Windows Server 2003 SDK which has a pre-release version)
   and the abort function ceased to be declared __declspec(noreturn). This was
   fixed in Visual Studio 2012. Trick stdlib.h into not defining abort (this
   means exit and _exit are not defined either, but they aren't required). */
#define _CRT_TERMINATE_DEFINED
__declspec(noreturn) void __cdecl abort(void);
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "caml/config.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/version.h"

caml_timing_hook caml_major_slice_begin_hook = NULL;
caml_timing_hook caml_major_slice_end_hook = NULL;
caml_timing_hook caml_minor_gc_begin_hook = NULL;
caml_timing_hook caml_minor_gc_end_hook = NULL;
caml_timing_hook caml_finalise_begin_hook = NULL;
caml_timing_hook caml_finalise_end_hook = NULL;

#ifdef DEBUG

void caml_failed_assert (char * expr, char_os * file_os, int line)
{
  char* file = caml_stat_strdup_of_os(file_os);
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  caml_stat_free(file);
  abort();
}

void caml_set_fields (value v, uintnat start, uintnat filler)
{
  mlsize_t i;
  for (i = start; i < Wosize_val (v); i++){
    Field (v, i) = (value) filler;
  }
}

#endif /* DEBUG */

uintnat caml_verb_gc = 0;

void caml_gc_message (int level, char *msg, ...)
{
  if ((caml_verb_gc & level) != 0){
    va_list ap;
    va_start(ap, msg);
    if (caml_verb_gc & 0x1000) {
      caml_print_timestamp(stderr, caml_verb_gc & 0x2000);
    }
    vfprintf (stderr, msg, ap);
    va_end(ap);
    fflush (stderr);
  }
}

void (*caml_fatal_error_hook) (char *msg, va_list args) = NULL;

CAMLexport void caml_fatal_error (char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if(caml_fatal_error_hook != NULL) {
    caml_fatal_error_hook(msg, ap);
  } else {
    fprintf (stderr, "Fatal error: ");
    vfprintf (stderr, msg, ap);
    fprintf (stderr, "\n");
  }
  va_end(ap);
  abort();
}

void caml_fatal_out_of_memory(void)
{
  caml_fatal_error("Out of memory");
}

void caml_ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = caml_stat_alloc(sizeof(void *) * init_capa);
}

int caml_ext_table_add(struct ext_table * tbl, caml_stat_block data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      caml_stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void caml_ext_table_remove(struct ext_table * tbl, caml_stat_block data)
{
  int i;
  for (i = 0; i < tbl->size; i++) {
    if (tbl->contents[i] == data) {
      caml_stat_free(tbl->contents[i]);
      memmove(&tbl->contents[i], &tbl->contents[i + 1],
              (tbl->size - i - 1) * sizeof(void *));
      tbl->size--;
    }
  }
}

void caml_ext_table_clear(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries) {
    for (i = 0; i < tbl->size; i++) caml_stat_free(tbl->contents[i]);
  }
  tbl->size = 0;
}

void caml_ext_table_free(struct ext_table * tbl, int free_entries)
{
  caml_ext_table_clear(tbl, free_entries);
  caml_stat_free(tbl->contents);
}

/* Integer arithmetic with overflow detection */

#if ! (__GNUC__ >= 5 || Caml_has_builtin(__builtin_mul_overflow))
CAMLexport int caml_umul_overflow(uintnat a, uintnat b, uintnat * res)
{
#define HALF_SIZE (sizeof(uintnat) * 4)
#define HALF_MASK (((uintnat)1 << HALF_SIZE) - 1)
#define LOW_HALF(x) ((x) & HALF_MASK)
#define HIGH_HALF(x) ((x) >> HALF_SIZE)
  /* Cut in half words */
  uintnat al = LOW_HALF(a);
  uintnat ah = HIGH_HALF(a);
  uintnat bl = LOW_HALF(b);
  uintnat bh = HIGH_HALF(b);
  /* Exact product is:
              al * bl
           +  ah * bl  << HALF_SIZE
           +  al * bh  << HALF_SIZE
           +  ah * bh  << 2*HALF_SIZE
     Overflow occurs if:
        ah * bh is not 0, i.e. ah != 0 and bh != 0
     OR ah * bl has high half != 0
     OR al * bh has high half != 0
     OR the sum al * bl + LOW_HALF(ah * bl) << HALF_SIZE
                        + LOW_HALF(al * bh) << HALF_SIZE overflows.
     This sum is equal to p = (a * b) modulo word size. */
  uintnat p = a * b;
  uintnat p1 = al * bh;
  uintnat p2 = ah * bl;
  *res = p;
  if (ah == 0 && bh == 0) return 0;
  if (ah != 0 && bh != 0) return 1;
  if (HIGH_HALF(p1) != 0 || HIGH_HALF(p2) != 0) return 1;
  p1 <<= HALF_SIZE;
  p2 <<= HALF_SIZE;
  p1 += p2;
  if (p < p1 || p1 < p2) return 1; /* overflow in sums */
  return 0;
#undef HALF_SIZE
#undef HALF_MASK
#undef LOW_HALF
#undef HIGH_HALF
}
#endif

/* Runtime warnings */

uintnat caml_runtime_warnings = 0;
static int caml_runtime_warnings_first = 1;

int caml_runtime_warnings_active(void)
{
  if (!caml_runtime_warnings) return 0;
  if (caml_runtime_warnings_first) {
    fprintf(stderr, "[ocaml] (use Sys.enable_runtime_warnings to control "
                    "these warnings)\n");
    caml_runtime_warnings_first = 0;
  }
  return 1;
}

/* Flambda 2 invalid term markers */

CAMLnoreturn_start
void caml_flambda2_invalid (value message)
CAMLnoreturn_end;

void caml_flambda2_invalid (value message)
{
  fprintf (stderr, "[ocaml] [flambda2] Invalid code:\n%s\n\n",
    String_val(message));
  fprintf (stderr, "This might have arisen from a wrong use of [Obj.magic].\n");
  fprintf (stderr, "Consider using [Sys.opaque_identity].\n");
  abort ();
}

/* Helper function for copying parameters out of closures and assembling
   them into memory blocks for a variadic call in caml_curry_generic */

static value traverse_closure (value closure, uintnat* buffer,
  /* Total number unarized params: */
  uintnat num_int, uintnat num_float,
  /* Number of unarized params written to [buffer]: */
  uintnat* num_int_written, uintnat* num_float_written,
  /* Index of the current complex param being written: */
  uintnat* complex_param_index;
  /* Number of unarized params seen during the [traverse_closure] recursion
     on the way to the earliest closure: */
  uintnat num_passed_over,
  /* Function pointer of the actual closure for the function being called.
     The closure itself is the return value. */
  uintnat* func_ptr)
{
  uintnat startenv;
  uintnat num_unarized_params_this_closure;
  uintnat total_num_unarized_params;
  value parent_closure;
  value actual_closure;

  total_num_unarized_params = num_int + num_float;
  CAMLassert(num_passed_over <= total_num_unarized_params);

  /* XXX or check the num-params field, which increments by 1 in each
     linked closure */
  startenv = Start_env_closinfo(Closinfo_val(closure));
  if (num_passed_over < total_num_unarized_params) {
    num_unarized_params_this_closure = Wosize_val(closure)
      - 1 /* code pointer */
      - 1 /* arity */
      - 1 /* num complex params applied thus far */
      - 1 /* layout */
      - 1 /* parent closure link */
      ;
    num_passed_over += num_unarized_params_this_closure;

    parent_closure = Field(closure, startenv);
    CAMLassert(Is_block(parent_closure));
    /* Note that [parent_closure] should never have tag [Infix_tag]. */
    CAMLassert(Tag_val(parent_closure) == Closure_tag);

    actual_closure = traverse_closure(parent_closure, buffer,
      num_int, num_float, num_int_written, num_float_written,
      num_passed_over, func_ptr);
  }
  else {
    /* At this point we've either reached the base case of the recursion,
       i.e. the oldest closure made by caml_curry_generic. */

    /* Extract the actual closure for the function ultimately being called. */
    actual_closure = Field(closure, startenv);

    CAMLassert(Is_block(actual_closure));
    CAMLassert(Tag_val(actual_closure) == Closure_tag
      || Tag_val(actual_closure) == Infix_tag);
    CAMLassert(Arity_closinfo(Closinfo_val(actual_closure)) > 1);
    CAMLassert(Wosize_val(actual_closure)) >= 3);

    /* Extract the full application code pointer. */
    *func_ptr = (uintnat) Field(actual_closure, 2);
  }



/* extract function pointer from the actual closure (pointed to by the
   earliest closure) + return in buffer
   need to also get that actual closure + put it in the buffer as the last
   int arg (since these functions always have arity >= 2, maybe assert that)
   The num_int value is to include the closure arg
   */

  return actual_closure;
}

uintnat* caml_curry_generic_helper (value callee_closure)
{
  uintnat startenv;
  uintnat* layout;
  uintnat num_int, num_float;
  uintnat num_int_written, num_float_written;
  uintnat complex_param_index;
  uintnat* buffer;
  uintnat func_ptr;
  value actual_closure;

  /* Format of returned buffer, by word:

     function pointer to be called
     num of unarized int arguments (including the closure argument)
     num of unarized float arguments
     unarized int arguments (excluding the closure argument)
     closure argument
     unarized float arguments

     The caller is responsible for calling [free] on the buffer.
  */

  startenv = Start_env_closinfo(Closinfo_val(callee_closure));
  layout = Field(callee_closure, startenv - 1);

  num_int = 0;
  num_float = 0;

  complex_param_index = 0;

  while (layout[complex_param_index] != NULL) {

  }

  /* 4 = words for: func_ptr, num_int, num_float, closure arg */
  buffer = (uintnat*) malloc(sizeof(uintnat) * (num_int + num_float + 4));
  if (buffer == NULL) {
    caml_fatal_out_of_memory ();
  }

  num_int_written = 0;
  num_float_written = 0;
  complex_param_index = 0;

  actual_closure = traverse_closure(Field(callee_closure, startenv), buffer,
    num_int, num_float, &num_int_written, &num_float_written,
    &complex_param_index, 0, &func_ptr);

  buffer[0] = func_ptr;
  buffer[1] = num_int + 1  /* + 1 for [actual_closure] */
  buffer[2] = num_float;
  buffer[num_int + 2] = (uintnat) actual_closure;

  return buffer;
}
