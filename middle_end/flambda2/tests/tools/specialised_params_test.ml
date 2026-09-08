(* Checks that [Compare] takes specialised parameters, synthetic value slots and
   specialisation sites into account, and that the approximants it returns
   preserve them. *)

open Import

let base =
  {|let code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let $f = closure f &toplevel.alloc_region synthetic { sx = 0; sy = 1 } in
let $camlCompare = Block 0 ($f) in
cont done ($camlCompare)
|}

let occurrences text pattern =
  let n = String.length pattern in
  let rec go i acc =
    if i + n > String.length text
    then List.rev acc
    else if String.equal (String.sub text i n) pattern
    then go (i + n) (i :: acc)
    else go (i + 1) acc
  in
  go 0 []

let replace_at text ~pattern ~with_ i =
  let n = String.length pattern in
  String.sub text 0 i ^ with_
  ^ String.sub text (i + n) (String.length text - i - n)

(* Replace the unique occurrence of [pattern]. *)
let replace text ~pattern ~with_ =
  match occurrences text pattern with
  | [i] -> replace_at text ~pattern ~with_ i
  | [] | _ :: _ :: _ ->
    Misc.fatal_errorf "Expected exactly one occurrence of %S" pattern

let replace_all text ~pattern ~with_ =
  match occurrences text pattern with
  | [] -> Misc.fatal_errorf "Expected an occurrence of %S" pattern
  | occurrences ->
    List.fold_left
      (fun text i -> replace_at text ~pattern ~with_ i)
      text (List.rev occurrences)

let delete_line text ~containing =
  let lines = String.split_on_char '\n' text in
  let lines' =
    List.filter
      (fun line ->
        match occurrences line containing with [] -> true | _ -> false)
      lines
  in
  if List.compare_lengths lines lines' = 0
  then Misc.fatal_errorf "No line contains %S" containing;
  String.concat "\n" lines'

(* Named variants of [base]; see the checks below for what they exercise. *)
let missing = delete_line base ~containing:"specialised {"

let missing_one = replace base ~pattern:"x = sx; y = sy" ~with_:"x = sx"

let swapped = replace base ~pattern:"x = sx; y = sy" ~with_:"x = sy; y = sx"

let ordinary = replace missing ~pattern:" synthetic {" ~with_:" with {"

let renamed =
  base
  |> replace ~pattern:"(x : val, y : val)" ~with_:"(u : val, v : val)"
  |> replace ~pattern:"x = sx; y = sy" ~with_:"u = su; v = sv"
  |> replace ~pattern:"cont k (x)" ~with_:"cont k (u)"
  |> replace ~pattern:"sx = 0; sy = 1" ~with_:"su = 0; sv = 1"

let reordered = replace base ~pattern:"x = sx; y = sy" ~with_:"y = sy; x = sx"

let different_body = replace base ~pattern:"cont k (x)" ~with_:"cont k (y)"

let renamed_different_body =
  replace renamed ~pattern:"cont k (u)" ~with_:"cont k (v)"

let mark_site text =
  replace text ~pattern:"closure f" ~with_:"closure specialisation_site f"

let site = mark_site base

let site_different_body = replace site ~pattern:"cont k (x)" ~with_:"cont k (y)"

let empty = replace missing ~pattern:" synthetic { sx = 0; sy = 1 }" ~with_:""

let empty_site = mark_site empty

let empty_site_different_body =
  replace empty_site ~pattern:"cont k (x)" ~with_:"cont k (y)"

let malformed_site = mark_site ordinary

let duplicate_values = replace base ~pattern:"sy = 1" ~with_:"sy = 0"

let duplicate_values_reordered =
  replace duplicate_values ~pattern:"x = sx; y = sy" ~with_:"y = sy; x = sx"

let mixed_duplicate_values =
  replace duplicate_values ~pattern:"sx = 0; sy = 0"
    ~with_:"sx = 0; sy = 0; sz = 0"

let mixed_duplicate_values_reordered =
  replace mixed_duplicate_values ~pattern:"x = sx; y = sy"
    ~with_:"y = sy; x = sx"

let mixed_duplicate_values_changed =
  replace mixed_duplicate_values_reordered ~pattern:"sz = 0" ~with_:"sz = 1"

let missing_sy =
  replace duplicate_values ~pattern:"sx = 0; sy = 0" ~with_:"sx = 0"

let missing_sx =
  replace duplicate_values ~pattern:"sx = 0; sy = 0" ~with_:"sy = 0"

let mixed_kinds =
  base
  |> replace ~pattern:"y : val" ~with_:"y : float"
  |> replace ~pattern:"sy = 1" ~with_:"sy : float = 1.0"

let mixed_kinds_different_body =
  replace mixed_kinds ~pattern:"cont k (x)" ~with_:"cont k (0)"

let cyclic = replace base ~pattern:"sx = 0; sy = 1" ~with_:"sx = $f; sy = $f"

let cyclic_renamed =
  cyclic
  |> replace_all ~pattern:"$f" ~with_:"$g"
  |> replace_all ~pattern:"sx" ~with_:"su"
  |> replace_all ~pattern:"sy =" ~with_:"sv ="
  |> replace_all ~pattern:"= sy" ~with_:"= sv"

(* The units are all parsed from the same file name, so that they belong to the
   same compilation unit, [Compare]. *)
let filename =
  let dir = Filename.temp_dir "specialised_params_test" "" in
  Filename.concat dir "compare.fl"

let () = Env.set_current_unit (Parse_flambda.make_unit_info ~filename)

let with_text text ~f =
  Out_channel.with_open_text filename (fun out -> output_string out text);
  f filename

let parse text =
  with_text text ~f:(fun filename ->
      match Parse_flambda.parse filename with
      | Ok unit -> unit
      | Error _ -> Misc.fatal_errorf "Could not parse:@ %s" text)

let parse_fexpr text =
  with_text text ~f:(fun filename ->
      match Parse_flambda.parse_fexpr filename with
      | Ok unit -> unit
      | Error _ -> Misc.fatal_errorf "Could not parse:@ %s" text)

(* The annotations found in a unit. *)
type summary =
  { sites : int;
    specialised_params : int;
    synthetic_value_slots : int
  }

let add_fun_decl summary (decl : Fexpr.fun_decl) =
  { summary with
    sites = (summary.sites + if decl.is_specialisation_site then 1 else 0);
    synthetic_value_slots =
      (summary.synthetic_value_slots
      +
      match decl.synthetic_value_slots with
      | None -> 0
      | Some slots -> List.length slots)
  }

let rec summarise summary (expr : Fexpr.expr) =
  match expr with
  | Let { bindings; value_slots = _; body } ->
    let summary =
      List.fold_left
        (fun summary ({ defining_expr; var = _ } : Fexpr.let_binding) ->
          match defining_expr with
          | Closure decl -> add_fun_decl summary decl
          | Simple _ | Prim _ | Rec_info _ -> summary)
        summary bindings
    in
    summarise summary body
  | Let_cont { recursive = _; body; bindings } ->
    List.fold_left
      (fun summary ({ handler; _ } : Fexpr.continuation_binding) ->
        summarise summary handler)
      (summarise summary body) bindings
  | Let_symbol { bindings; value_slots = _; body } ->
    let summary =
      List.fold_left
        (fun summary (binding : Fexpr.symbol_binding) ->
          match binding with
          | Code code ->
            let summary =
              { summary with
                specialised_params =
                  summary.specialised_params
                  + List.length code.params_and_body.specialised_params
              }
            in
            summarise summary code.params_and_body.body
          | Closure { fun_decl; symbol = _ } -> add_fun_decl summary fun_decl
          | Set_of_closures { bindings; elements = _ } ->
            List.fold_left
              (fun summary ({ fun_decl; _ } : Fexpr.static_closure_binding) ->
                add_fun_decl summary fun_decl)
              summary bindings
          | Data _ | Deleted_code _ -> summary)
        summary bindings
    in
    summarise summary body
  | Switch { scrutinee = _; cases } ->
    List.fold_left
      (fun summary (_, (cont : Fexpr.apply_or_inlined_cont)) ->
        match cont with
        | Inlined_goto expr -> summarise summary expr
        | Named_cont _ -> summary)
      summary cases
  | Apply _ | Apply_cont _ | Invalid _ -> summary

let summary_of_unit (unit : Fexpr.flambda_unit) =
  summarise
    { sites = 0; specialised_params = 0; synthetic_value_slots = 0 }
    unit.body

let failures = ref 0

let fail fmt =
  Format.kfprintf
    (fun ppf ->
      Format.fprintf ppf "@.";
      incr failures)
    Format.err_formatter ("FAIL " ^^ fmt)

(* As for [fldiff], the approximant returned is that of the second unit. *)
let compare ~left ~right = Compare.flambda_units (parse right) (parse left)

let check_equivalent name ~left ~right =
  match compare ~left ~right with
  | Equivalent -> ()
  | Different _ -> fail "%s: expected equivalent" name

let check_different name ~left ~right =
  match compare ~left ~right with
  | Different _ -> ()
  | Equivalent -> fail "%s: expected different" name

let check_both_directions ~expected name left right =
  let check = if expected then check_equivalent else check_different in
  check name ~left ~right;
  check (name ^ " (reverse)") ~left:right ~right:left

let equivalent = check_both_directions ~expected:true

let different = check_both_directions ~expected:false

let () =
  different "missing specialised parameters" base missing;
  different "missing one specialised parameter" base missing_one;
  different "swapped specialised parameters" base swapped;
  different "ordinary versus synthetic slots" ordinary missing;
  equivalent "alpha-renamed parameters and slots" base renamed;
  equivalent "reordered annotation entries" base reordered;
  different "specialisation-site marker" base site;
  different "empty specialisation-site marker" empty empty_site;
  equivalent "reordered equal-valued slots" duplicate_values
    duplicate_values_reordered;
  equivalent "mapped and unmapped equal-valued slots" mixed_duplicate_values
    mixed_duplicate_values_reordered;
  different "changed unmapped slot alongside equal-valued slots"
    mixed_duplicate_values mixed_duplicate_values_changed;
  different "mapped slots missing from the opposite set" missing_sy missing_sx;
  equivalent "alpha-renamed cyclic synthetic slots" cyclic cyclic_renamed

(* The approximant of [changed] must carry the same annotations as [changed] and
   must be equivalent to it. *)
let check_approximant name ~original ~changed =
  match compare ~left:original ~right:changed with
  | Equivalent -> fail "%s: expected different" name
  | Different { approximant } -> (
    let expected = summary_of_unit (parse_fexpr changed) in
    let actual = summary_of_unit (Flambda_to_fexpr.conv approximant) in
    if expected.sites <> actual.sites
    then fail "%s: approximant changed the specialisation-site markers" name;
    if expected.specialised_params <> actual.specialised_params
    then fail "%s: approximant changed the specialised parameters" name;
    if expected.synthetic_value_slots <> actual.synthetic_value_slots
    then fail "%s: approximant changed the synthetic value slots" name;
    match Compare.flambda_units approximant (parse changed) with
    | Equivalent -> ()
    | Different _ ->
      fail "%s: approximant is not equivalent to the second unit" name)

let check_approximant_both_directions name original changed =
  check_approximant name ~original ~changed;
  check_approximant (name ^ " (reverse)") ~original:changed ~changed:original

let () =
  check_approximant_both_directions "different body" base different_body;
  check_approximant_both_directions "alpha-renamed different body" base
    renamed_different_body;
  check_approximant_both_directions "specialisation-site different body" site
    site_different_body;
  check_approximant_both_directions "empty specialisation-site roundtrip"
    empty_site empty_site_different_body;
  check_approximant_both_directions "missing annotation" missing base;
  check_approximant_both_directions "missing one annotation" missing_one base;
  check_approximant_both_directions "swapped annotations" base swapped;
  check_approximant_both_directions "marker difference" base site;
  check_approximant_both_directions "empty marker difference" empty empty_site;
  check_approximant_both_directions "equal-valued slot approximant" missing_one
    duplicate_values_reordered;
  check_approximant_both_directions "changed unmapped slot approximant"
    mixed_duplicate_values mixed_duplicate_values_changed;
  check_approximant_both_directions "missing mapped slot approximant" missing_sy
    missing_sx;
  check_approximant_both_directions "mixed value/float slot roundtrip"
    mixed_kinds mixed_kinds_different_body

(* A specialisation site cannot have ordinary value slots. The error message is
   not printed, since it is expected. *)
let () =
  let out_functions =
    Format.pp_get_formatter_out_functions Format.err_formatter ()
  in
  Format.pp_set_formatter_out_functions Format.err_formatter
    { out_functions with
      out_string = (fun _ _ _ -> ());
      out_flush = (fun () -> ())
    };
  let accepted =
    match parse malformed_site with
    | exception Misc.Fatal_error -> false
    | _ -> true
  in
  Format.pp_set_formatter_out_functions Format.err_formatter out_functions;
  if accepted then fail "marked site with ordinary value slots was accepted"

let () =
  Misc.remove_file filename;
  if !failures > 0
  then (
    Format.eprintf "%d comparison checks failed@." !failures;
    exit 1)
  else Format.printf "Specialised-parameter comparison checks passed@."
