(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Classification of application expressions. *)

module Function_call : sig
  type t = private
    | Direct of Code_id.t
        (** The [code_id] uniquely determines the function symbol to call. *)
    | Indirect_full_application
    | Indirect
        (** [Indirect] can be used even if the application is known to be a
            full application. *)
end

module Method_kind : sig
  type t = private
    | Self
    | Public
    | Cached

  val from_lambda : Lambda.meth_kind -> t

  val to_lambda : t -> Lambda.meth_kind
end

(** Whether an application expression corresponds to an OCaml function
    invocation, an OCaml method invocation, or an external call. *)
type t = private
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_types.t;
        return_arity : Flambda_arity.With_subkinds.t
            (** [return_arity] describes what the callee returns. It matches
                up with the arity of [continuation] in the enclosing [Apply.t]
                record. *)
      }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t;
        alloc_mode : Alloc_mode.For_types.t;
        return_arity : Flambda_arity.With_subkinds.t
      }
  | C_call of
      { alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
        is_c_builtin : bool
            (* CR mshinwell: This should have the effects and coeffects
               fields *)
      }

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val direct_function_call :
  Code_id.t ->
  return_arity:Flambda_arity.With_subkinds.t ->
  Alloc_mode.For_types.t ->
  t

val indirect_function_call :
  return_arity:Flambda_arity.With_subkinds.t -> Alloc_mode.For_types.t -> t

val indirect_function_call_full_application :
  return_arity:Flambda_arity.With_subkinds.t -> Alloc_mode.For_types.t -> t

val method_call :
  Method_kind.t ->
  obj:Simple.t ->
  return_arity:Flambda_arity.With_subkinds.t ->
  Alloc_mode.For_types.t ->
  t

val c_call :
  alloc:bool ->
  param_arity:Flambda_arity.t ->
  return_arity:Flambda_arity.t ->
  is_c_builtin:bool ->
  t

val return_arity : t -> Flambda_arity.With_subkinds.t
