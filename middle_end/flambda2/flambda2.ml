(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Unlike most of the rest of Flambda 2, this file depends on ocamloptcomp,
   meaning it can call [Compilenv]. *)

let symbol_for_global id =
  Compilenv.symbol_for_global' id |> Flambda2_term_basics.Symbol.create_wrapped

let get_global_info comp_unit =
  (* Typing information for predefined exceptions should be populated directly
     by the callee. *)
  if Compilation_unit.equal comp_unit Compilation_unit.predef_exn
  then
    Misc.fatal_error
      "get_global_info is not for use with predefined exception compilation \
       units";
  if Compilation_unit.equal comp_unit
       (Flambda2_term_basics.Symbol.external_symbols_compilation_unit ())
  then None
  else
    (* CR lmaurer: It feels like there should be a
       [Compilenv.get_global_info_for_unit] here, but I'm not quite sure how to
       implement it. *)
    let id =
      Compilation_unit.name comp_unit
      |> Compilation_unit.Name.to_string |> Ident.create_persistent
    in
    match Compilenv.get_global_export_info id with
    | None | Some (Flambda2 None) -> None
    | Some (Flambda2 (Some info)) -> Some info
    | Some (Clambda _) ->
      (* CR mshinwell: This should be a user error, not a fatal error. Same
         below. *)
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Closure middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit
    | Some (Flambda1 _) ->
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Flambda 1 middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit

let print_rawflambda ppf unit =
  if Flambda_features.dump_rawflambda ()
  then
    Format.fprintf ppf "\n%tAfter CPS conversion:%t@ %a@."
      Flambda_colours.each_file Flambda_colours.pop Flambda_unit.print unit;
  if Flambda_features.dump_rawfexpr ()
  then
    Format.fprintf ppf "\n%tAfter CPS conversion:%t@ %a@."
      Flambda_colours.each_file Flambda_colours.pop Print_fexpr.flambda_unit
      (unit |> Flambda_to_fexpr.conv)

let print_flambda name ppf unit =
  if Flambda_features.dump_flambda ()
  then
    Format.fprintf ppf "\n%tAfter %s:%t@ %a@." Flambda_colours.each_file name
      Flambda_colours.pop Flambda_unit.print unit;
  if Flambda_features.dump_fexpr ()
  then
    Format.fprintf ppf "\n%tAfter %s:%t@ %a@." Flambda_colours.each_file name
      Flambda_colours.pop Print_fexpr.flambda_unit
      (unit |> Flambda_to_fexpr.conv)

let output_flexpect ~ml_filename ~raw_flambda:old_unit new_unit =
  if Flambda_features.dump_flexpect ()
  then
    let basename = Filename.chop_suffix ml_filename ".ml" in
    let filename = basename ^ ".flt" in
    let before = old_unit |> Flambda_to_fexpr.conv in
    let after = new_unit |> Flambda_to_fexpr.conv in
    let test : Fexpr.expect_test_spec = { before; after } in
    let out = open_out filename in
    Misc.try_finally
      ~always:(fun () -> close_out out)
      (fun () ->
        let ppf = out |> Format.formatter_of_out_channel in
        Print_fexpr.expect_test_spec ppf test;
        Format.pp_print_flush ppf ())

let lambda_to_cmm ~ppf_dump:ppf ~prefixname ~filename ~module_ident
    ~module_block_size_in_words ~module_initializer ~keep_symbol_tables =
  (* Make sure -linscan is enabled in classic mode. Doing this here to be sure
     it happens exactly when -Oclassic is in effect, which we don't know at CLI
     processing time because there may be an [@@@flambda_oclassic] or
     [@@@flambda_o3] attribute. *)
  if Flambda_features.classic_mode () then Clflags.use_linscan := true;
  Misc.Color.setup (Flambda_features.colour ());
  (* CR-someday mshinwell: Note for future WebAssembly work: this thing about
     the length of arrays will need fixing, I don't think it only applies to the
     Cmm translation. *)
  (* The Flambda 2 code won't currently operate on 32-bit hosts; see
     [Name_occurrences]. *)
  if Sys.word_size <> 64
  then Misc.fatal_error "Flambda 2 can only run on 64-bit hosts at present";
  (* When the float array optimisation is enabled, the length of an array needs
     to be computed differently according to the array kind, in the case where
     the width of a float is not equal to the machine word width (at present,
     this happens only on 32-bit targets). *)
  if Cmm_helpers.wordsize_shift <> Cmm_helpers.numfloat_shift
     && Flambda_features.flat_float_array ()
  then
    Misc.fatal_error
      "Cannot compile on targets where floats are not word-width when the \
       float array optimisation is enabled";
  let run () =
    let cmx_loader =
      Flambda_cmx.create_loader ~get_global_info ~symbol_for_global
    in
    let (Mode mode) = Flambda_features.mode () in
    let raw_flambda, close_program_metadata =
      Profile.record_call "lambda_to_flambda" (fun () ->
          Lambda_to_flambda.lambda_to_flambda ~mode ~symbol_for_global
            ~big_endian:Arch.big_endian ~cmx_loader ~module_ident
            ~module_block_size_in_words module_initializer)
    in
    Compiler_hooks.execute Raw_flambda2 raw_flambda;
    print_rawflambda ppf raw_flambda;
    let flambda, offsets, cmx, all_code =
      match mode, close_program_metadata with
      | Classic, Classic (code, cmx, offsets) ->
        (if Flambda_features.inlining_report ()
        then
          let output_prefix = prefixname ^ ".cps_conv" in
          let inlining_tree =
            Inlining_report.output_then_forget_decisions ~output_prefix
          in
          Compiler_hooks.execute Inlining_tree inlining_tree);
        raw_flambda, offsets, cmx, code
      | Normal, Normal ->
        let round = 0 in
        let { Simplify.unit = flambda; exported_offsets; cmx; all_code } =
          Profile.record_call ~accumulate:true "simplify" (fun () ->
              Simplify.run ~cmx_loader ~round raw_flambda)
        in
        (if Flambda_features.inlining_report ()
        then
          let output_prefix = Printf.sprintf "%s.%d" prefixname round in
          let inlining_tree =
            Inlining_report.output_then_forget_decisions ~output_prefix
          in
          Compiler_hooks.execute Inlining_tree inlining_tree);
        Compiler_hooks.execute Flambda2 flambda;
        print_flambda "simplify" ppf flambda;
        output_flexpect ~ml_filename:filename ~raw_flambda flambda;
        flambda, exported_offsets, cmx, all_code
    in
    (match cmx with
    | None ->
      () (* Either opaque was passed, or there is no need to export offsets *)
    | Some cmx -> Compilenv.flambda2_set_export_info cmx);
    let cmm = Flambda2_to_cmm.To_cmm.unit flambda ~all_code ~offsets in
    if not keep_symbol_tables
    then (
      Compilenv.reset_info_tables ();
      Flambda2_term_basics.Continuation.reset ();
      Flambda2_term_basics.Int_ids.reset ());
    cmm
  in
  Profile.record_call "flambda2" run
