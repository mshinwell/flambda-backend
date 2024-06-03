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

type t =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    toplevel_my_region : Variable.t;
    body : Flambda.Expr.t;
    module_symbols : Symbol.t list;
    used_value_slots : Value_slot.Set.t Or_unknown.t
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_region ~body
    ~module_symbols ~used_value_slots =
  { return_continuation;
    exn_continuation;
    toplevel_my_region;
    body;
    module_symbols;
    used_value_slots
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let toplevel_my_region t = t.toplevel_my_region

let body t = t.body

let module_symbols t = t.module_symbols

let used_value_slots t = t.used_value_slots

let [@ocamlformat "disable"] print ppf
      { return_continuation; exn_continuation; toplevel_my_region; body;
        module_symbols; used_value_slots;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbols@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(toplevel_my_region@ %a)@]@ \
        @[<hov 1>(used_value_slots@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
    module_symbols
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Variable.print toplevel_my_region
    (Or_unknown.print Value_slot.Set.print) used_value_slots
    Flambda.Expr.print body
