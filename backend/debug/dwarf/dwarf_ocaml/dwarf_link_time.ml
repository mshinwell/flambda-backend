(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Dwarf_high

let keep_dwp_dot_s = true (* XXX move to a command-line flag *)

let generate_dwarf_dwp ~asm_directives ~basic_block_sections
    ~binary_backend_available ~output_name ~units_to_link ~get_dwarf_from_unit =
  if not !Dwarf_flags.split_dwarf
  then ()
  else
    let asm_file =
      if keep_dwp_dot_s
      then output_name ^ ".dwp" ^ Config.ext_asm
      else Filename.temp_file "camlstartup" Config.ext_asm
    in
    let dwp_file = output_name ^ ".dwp" in
    let remove_asm_file () =
      if not keep_dwp_dot_s then Misc.remove_file asm_file
    in
    Misc.try_finally
      ~exceptionally:(fun () -> Misc.remove_file dwp_file)
      (fun () ->
        Emitaux.output_channel := open_out asm_file;
        Misc.try_finally
          (fun () ->
            let dwarf_states =
              List.fold_left
                (fun dwarf_states unit ->
                  match get_dwarf_from_unit unit with
                  | None -> dwarf_states
                  | Some serialized_dwarf ->
                    let dwarf_state =
                      Dwarf_state.Serialized.to_dwarf_state serialized_dwarf
                      |> Dwarf_state.get_dwarf_world_state
                    in
                    dwarf_state :: dwarf_states)
                [] units_to_link
            in
            match dwarf_states with
            | [] -> ()
            | _ :: _ ->
              let dwarf_world = Dwarf_world.create dwarf_states in
              Dwarf_world.emit dwarf_world ~asm_directives Normal
                ~basic_block_sections ~binary_backend_available)
          ~always:(fun () -> close_out !Emitaux.output_channel)
          ~exceptionally:remove_asm_file)
