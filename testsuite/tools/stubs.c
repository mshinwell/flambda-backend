/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            David Allsopp, University of Cambridge & Tarides            */
/*                                                                        */
/*   Copyright 2025 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/osdeps.h>

value caml_in_prefix_test_no_caml_executable_name(value unit)
{
  char_os *name = caml_executable_name();
  if (name)
    caml_stat_free(name);
  return Val_bool(name == NULL);
}
