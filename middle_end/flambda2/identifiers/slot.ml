(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  include Container_types.S

  module Lmap : Lmap.S with type key = t

  val create : Compilation_unit.t -> name:string -> t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val to_string : t -> string

  val name : t -> string

  val rename : t -> t
end

module Make (P : sig
  val colour : unit -> string
end) : S = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      name_stamp : int
          (** [name_stamp]s are unique within any given compilation unit. *)
    }

  module Self = Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      if t1 == t2
      then 0
      else
        let c = t1.name_stamp - t2.name_stamp in
        if c <> 0
        then c
        else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

    let equal t1 t2 = compare t1 t2 = 0

    let hash t =
      Hashtbl.hash (t.name_stamp, Compilation_unit.hash t.compilation_unit)

    let print ppf t =
      Format.fprintf ppf "@<0>%s" (P.colour ());
      if Compilation_unit.equal t.compilation_unit
           (Compilation_unit.get_current_exn ())
      then Format.fprintf ppf "%s/%d" t.name t.name_stamp
      else
        Format.fprintf ppf "%a.%s/%d" Compilation_unit.print t.compilation_unit
          t.name t.name_stamp;
      Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())
  end)

  include Self

  module Lmap = Lmap.Make (struct
    type nonrec t = t

    include Self
  end)

  let next_stamp = ref 0

  let get_next_stamp () =
    let stamp = !next_stamp in
    incr next_stamp;
    stamp

  let create compilation_unit ~name =
    { compilation_unit; name; name_stamp = get_next_stamp () }

  let get_compilation_unit t = t.compilation_unit

  let in_compilation_unit t compilation_unit =
    Compilation_unit.equal compilation_unit t.compilation_unit

  let is_imported t = not (Compilation_unit.is_current t.compilation_unit)

  let to_string t = t.name ^ "_" ^ string_of_int t.name_stamp

  let name t = t.name

  let rename t = { t with name_stamp = get_next_stamp () }
end
