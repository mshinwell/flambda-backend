# Asynchronous exceptions model

## Overview

A means is provided of installing handlers for _async exceptions_
which is a defined set arising from safe points (allocation and polling
points).  These async exceptions encompass predefined exceptions
used by the GC (`Stack_overflow` and `Out_of_memory`) together with
exceptions arising from user code in signal handlers and finalisers (in
particular, `Sys.Break`).

A similar stack discipline to normal exception handlers applies for
async exception handlers.  An exception will only be delivered to
an async exception handler, as the user sees it, if and only if
such exception is async.  Async exceptions are never delivered to any
other handlers.

## Front end

A new primitive is provided, to be exposed in the stdlib:

```
external with_async_exns : (unit -> 'a) -> (exn -> 'a) -> 'a
  = "%with_async_exns"
```

This behaves like `try ... with` except that the supplied handler only
receives async exceptions.

There is also a new predefined exn constructor `Async_exn` taking an argument of
type `exn`.  User code must not raise this.  There is a new constraint that
user code must not raise `Stack_overflow` or `Out_of_memory`.

## Middle end

In Lambda, there would be a new primitive corresponding to `%with_async_exns`.

The semantics of `[[ with_async_exns e1 e2 ]]` is:
```
  let body = [[ e1 ]] in
  let handler = [[ e2 ]] in
  try_async body [@inlined never] ()
  with
  | ( Stack_overflow | Out_of_memory | Async_exn _ ) as exn ->
    (* This pattern must cover all async exception constructors.  Owing
       to the constraints above we know that these can only have been raised
       by the GC. *)
    unwind_normal_exception_stack ();  (* see "Scenarios", below *)
    handler [@inlined never] exn
  | exn ->
    (* This cannot be an async exception. *)
    unwind_async_exception_stack ();  (* likewise *)
    reraise exn
```
where `try_async` is like normal `try` but with a flag saying that it
binds an async exception handler.

### Closure and Flambda 1

These middle ends will need to annotate their `try ... with` constructs as to
whether they are binding normal or async exception handlers, but that should be
all.  This information will need to be propagated to the generated Clambda
code.

### Flambda 2

Trap actions will need to be augmented to say whether they are normal
or async.  This information will need to be propagated to the generated
Cmm code.

In Flambda 2, an async exception handler never has _extra arguments_,
so its Cmm translation will not involve mutable variables.  (Flambda 2
only emits mutable Cmm variables for this case and this case alone.)
This means it can be invoked directly from the GC at a safe point.

Flambda 2 will have to mark continuations that are async exception
handlers (by augmenting the flag used at present for exception handlers).
For normal exception handlers, Flambda 2 segregates the occurrences of
exception continuation identifiers (`Continuation.t`) according to whether
they occur in trap actions or not: for deletion of dead code, occurrences only
in trap actions do not count.  Furthermore normal exception handlers appear
in the double-barrelled CPS style, although this is strictly speaking
redundant with the trap actions, which are only really kept to ensure that
the user's placement of `try ... with` blocks remains as-is.  For async
exception continuations, of which there are expected to be very few, it
seems reasonable to always count the `Continuation.t` names as free in the
normal sense even if they occur in trap actions.  This will sometimes inhibit
deletion of dead async exception handlers but such cases seem rare.  It also
seems reasonable not to identify the current async exception continuation, to
avoid triple-barrelled CPS.

## Backend

The backend push/pop annotations are enhanced with the `Normal` or
`Async` specifications, for translating `try` and `try_async` respectively.

**Key point:** Whenever an async exception handler is installed, a normal
exception handler is installed too.  This ensures that management of the async
exception stack can be kept in sync with that of the normal exception stack.
(See "Scenarios", below.)

During a normal exception raise an async handler may be called.  In
this circumstance it may be necessary to unwind the async exception
stack.

### Liveness analysis

The liveness analysis passes in `Mach` and `Cfg` will need to be enhanced to
understand that:

1. exceptions arising from safe points only get delivered to handlers marked
`Async`;

2. exceptions arising from anywhere else may get delivered to both handlers
marked `Normal` and those marked `Async`.

We ensure that we don't fall into the trap (no pun intended) fixed by upstream
PR10523 as follows:
- for non-Flambda 2, the fix in that PR suffices;
- for Flambda 2, point 1. above in conjunction with the fact that
  async handlers never have extra arguments.  (PR10523 does not
  suffice for Flambda 2 since no suitable reordering of passes is available:
  Flambda 2 has already happened by the time the backend is reached.)

It follows that it is not conservative to have a single set of exceptions
tracked by liveness analysis; they must be segregated into the ones coming
from safe points, and all of the others.

### Inline code to raise exceptions (in the emitters)

This won't need to be changed, since this code never raises async
exceptions.

## Runtime

There is a new linked list on the stack for async exception trap
frames.  The head of the list (newest entry) is given by a new
domain state entry `caml_async_exception_pointer` (analogously to the
existing `caml_exception_pointer`).

Async exception trap frames contain analogous information to the
normal exception trap frames: the pointer to the previous async
exception handler code; and the pointer to the associated stack pointer for
execution of such code.

The existing toplevel uncaught exception handler code can also be used to
catch async exceptions, although an async trap frame will
also be required.

An alternative to `caml_async_exception_pointer` would be to mark trap frames as
to whether they are `Normal` or `Async` at runtime.  One way this can be done is
by using the top bit of the handler code pointer (the bottom bit cannot be used
without alterations to the backend to align exception handler entry points,
which is a bit tricky and potentially wasteful of space).  However this
increases the length of the code sequences required for raising exceptions,
which doesn't seem like a good trade-off, given that async exception handlers
will be relatively rare (non-existent in many programs).

The runtime needs altering (as has already been done in PR194) so there is
a separate code path for raising of async exceptions called from the
appropriate places.  We will need to check that the logic here is
reasonable.

## Scenarios

### Raising an async exception

The same procedure is followed as for normal exceptions, including
collection of backtraces into the same backtrace buffer, with the
exception that `caml_async_exception_pointer` is used as the starting point.

### Catching of a normal exception inside an async exception handler

When this happens it is necessary to unwind the `caml_async_exception_pointer`
stack until the stack pointers therein are at or past the stack pointer in
the previous (earlier) trap frame upon entry to the handler.

### Catching of an async exception inside an async exception handler

Prior to executing the handler it is necessary to unwind the normal
exception stack, similarly to what happens to the async exception
stack when a normal exception is caught inside an async exception
handler.

### Catching of a normal exception inside a normal exception handler

Nothing needs doing here.  If the exception ends up crossing the point at
which an async handler was installed, such async handler
will be executed, as normal trap frames corresponding to them are always
placed too.  This will cause the async exception stack to be correctly
unwound.

### Catching of an async exception inside a normal exception handler

This case is impossible, since the starting point for an async raise in the GC
will be `caml_async_exception_pointer`.
