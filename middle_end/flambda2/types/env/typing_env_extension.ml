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

[@@@ocaml.warning "+a-30-40-41-42"]

module Make (Type_grammar : sig
  type t

  include Contains_ids.S with type t := t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val check_equation : Name.t -> t -> unit
end) =
struct
  type t = { equations : Type_grammar.t Name.Map.t } [@@unboxed]

  let fold ~equation t acc = Name.Map.fold equation t.equations acc

  let invariant { equations } =
    if Flambda_features.check_invariants ()
    then Name.Map.iter Type_grammar.check_equation equations

  let empty = { equations = Name.Map.empty }

  let is_empty { equations } = Name.Map.is_empty equations

  let from_map equations =
    let t = { equations } in
    invariant t;
    t

  let to_map { equations } = equations

  let one_equation name ty =
    Type_grammar.check_equation name ty;
    { equations = Name.Map.singleton name ty }

  let add_or_replace_equation t name ty =
    Type_grammar.check_equation name ty;
    if Flambda_features.check_invariants () && Name.Map.mem name t.equations
    then
      Format.eprintf
        "Warning: Overriding equation for name %a@\n\
         Old equation is@ @[%a@]@\n\
         New equation is@ @[%a@]@." Name.print name Type_grammar.print
        (Name.Map.find name t.equations)
        Type_grammar.print ty;
    { equations = Name.Map.add name ty t.equations }

  let replace_equation t name ty =
    { equations = Name.Map.add (* replace *) name ty t.equations }

  let all_ids_for_export { equations } =
    Name.Map.fold
      (fun name ty acc ->
        let acc =
          Ids_for_export.union (Type_grammar.all_ids_for_export ty) acc
        in
        Ids_for_export.add_name acc name)
      equations Ids_for_export.empty

  module With_extra_variables = struct
    type t =
      { existential_vars : Flambda_kind.t Variable.Map.t;
        equations : Type_grammar.t Name.Map.t
      }

    let [@ocamlformat "disable"] print ppf { existential_vars; equations; } =
      Format.fprintf ppf
        "@[<hov 1>(\
         @[<hov 1>(variables@ @[<hov 1>%a@])@]\
         @[<hov 1>(equations@ @[<v 1>%a@])@])@ \
         @]"
        (Variable.Map.print Flambda_kind.print) existential_vars
        print_equations equations

    let fold ~variable ~equation t acc =
      let acc = Variable.Map.fold variable t.existential_vars acc in
      Name.Map.fold equation t.equations acc

    let empty () =
      { existential_vars = Variable.Map.empty; equations = Name.Map.empty }

    let add_definition t var kind ty =
      let existential_vars = Variable.Map.add var kind t.existential_vars in
      let equations = Name.Map.add (Name.var var) ty t.equations in
      { existential_vars; equations }

    let add_or_replace_equation t name ty =
      Type_grammar.check_equation name ty;
      { existential_vars = t.existential_vars;
        equations = Name.Map.add name ty t.equations
      }
  end
end
