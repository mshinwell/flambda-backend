#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <assert.h>

extern __attribute__((noreturn))
void caml_raise_exception(caml_domain_state*, value);

value noalloc0(value arg)
{
  assert(arg == Val_long(42));
  const value* exn_ptr = caml_named_value("noalloc_exn");
  if (!exn_ptr) {
    printf("couldn't get exception pointer\n");
    abort();
  }
  caml_raise_exception(Caml_state, *exn_ptr);
  return Val_unit;
}
