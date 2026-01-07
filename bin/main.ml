(* Copyright (c) 2025 Hubert Gruniaux
 *
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to <https://unlicense.org/>
 *)

open Minisync

let () = Printexc.record_backtrace true

type action =
  | Check_only
      (** Parses and typechecks the input program but do not emits anything.
          Just prints errors and warnings. *)
  | Print_tast
      (** Parses and typechecks the input program and prints the typed abstract
          syntax tree (TAST). Stops just after it. *)
  | Print_interface
      (** Parses and typechecks the input program and prints its interface (the
          list of global variables and functions with their types). Stops just
          after it. *)
  | Print_nast
      (** Parses, typechecks and normalizes the input program and prints the
          normalized abstract syntax tree (NAST). Stops just after it. *)
  | Print_ir
      (** Parses, typechecks, normalizes and codegens the input program and
          prints the intermediate representation (IR). Stops just after it. *)
  | Compile
      (** Full compilation: parses, typechecks, normalizes, codegens and emits
          the final code. *)

let colors_supported = Out_channel.isatty stdout || Out_channel.isatty stderr
let enable_colors = ref colors_supported
let action = ref Compile
let usage = "Usage: minisync [options] <filename>"

let spec =
  Arg.align
    [
      ( "-i",
        Arg.Unit (fun _ -> action := Print_interface),
        " Print the interface of the checked program." );
      ( "--check",
        Arg.Unit (fun _ -> action := Check_only),
        " Only typecheck the input program." );
      ( "--color",
        Arg.Symbol
          ( [ "always"; "never"; "auto" ],
            function
            | "always" -> enable_colors := true
            | "never" -> enable_colors := false
            | "auto" -> enable_colors := colors_supported
            | _ -> assert false ),
        " Set if colors are used in output." );
      ( "--error-format",
        Arg.Symbol
          ( [ "ocaml"; "gcc" ],
            function
            | "ocaml" -> Error.format := Error.OCaml
            | "gcc" -> Error.format := Error.GCC
            | _ -> assert false ),
        " Set the error message format." );
      ( "--error-lines",
        Arg.Set Error.show_lines,
        " Show source code lines in error messages." );
      ( "--no-error-lines",
        Arg.Clear Error.show_lines,
        " Do not show source code lines in error messages." );
      ("--no-warnings", Arg.Clear Error.show_warnings, " Do not show warnings.");
      ( "--warnings-as-errors",
        Arg.Set Error.warnings_as_errors,
        " Treat warnings as errors." );
      ( "--dump-tast",
        Arg.Unit (fun _ -> action := Print_tast),
        " Print the typed abstract syntax tree (TAST) of the checked program."
      );
      ( "--dump-nast",
        Arg.Unit (fun _ -> action := Print_nast),
        " Print the normalized abstract syntax tree (NAST) of the checked \
         program." );
      ( "--dump-ir",
        Arg.Unit (fun _ -> action := Print_ir),
        " Print the intermediate representation (IR) of the compiled program."
      );
      ( "--print-types",
        Arg.Set Printer_utils.print_types,
        " Print types in the dumps (tast, nast, ir, ...)." );
      ( "--print-clocks",
        Arg.Set Printer_utils.print_clocks,
        " Print clocks in the dumps (tast, nast, ir, ...)." );
      ( "--print-locations",
        Arg.Set Printer_utils.print_locations,
        " Print source code locations in the dumps (tast, nast, ir, ...)." );
      ( "--print-var-types-ids",
        Arg.Set Printer_utils.print_var_types_ids,
        " When dumping the TAST, NAST or IR, also print the unique ids of \
         variable types/clocks." );
      ( "--optimize-ir",
        Arg.Set Codegen.optimize_ir,
        " Enable or disable IR optimizations." );
    ]

let filename = ref ""

(* Parse arguments. *)
let () = Arg.parse spec (fun f -> filename := f) usage

(* Handle the --color option. *)
let () =
  if !enable_colors then begin
    Printer_utils.enable_colors Format.err_formatter;
    Printer_utils.enable_colors Format.std_formatter
  end

let filename =
  if Filename.check_suffix !filename ".sync" then !filename
  else begin
    Error.print_error "Input file must have .sync extension." Location.dummy [];
    exit 1
  end

let input = open_in filename

let handle_error ?(notes = []) msg pos =
  Error.print_error msg pos notes;
  exit 1

let () =
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  try
    let ast = Parser.program Lexer.next_token lexbuf in
    close_in input;
    let ctx = Typechecker.create_context () in
    let tast = Typechecker.check_program ctx ast in
    if !action = Print_tast then (
      Tast_printer.pp_program Format.std_formatter tast;
      if !Typechecker.had_error then exit 1 else exit 0)
    else if !action = Print_interface then (
      if !Typechecker.had_error then exit 1;
      Print_interface.print_interface tast;
      exit 0)
    else if !action = Check_only then
      if !Typechecker.had_error then exit 1 else exit 0;

    if !Typechecker.had_error then exit 1;

    let nast = Normalizer.normalize_program tast in
    if !action = Print_nast then (
      Format.printf "%a@." Nast_printer.pp_program nast;
      exit 0);

    let ir = Codegen.codegen_program nast in
    if !action = Print_ir then (
      Format.printf "%a@." Ir_printer.pp_program ir;
      exit 0);

    let ocaml = Gen_ml.emit_program ir in
    Format.printf "%a@." Ocaml_printer.pp_module ocaml
    (* Format.printf "%a@." Backend_ml.emit_program ir *)
  with
  | Lexer.Error msg ->
      close_in input;
      handle_error msg (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)
  | Parser.Error ->
      close_in input;
      handle_error "Syntax error."
        (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)
  | Typechecker.Error (msg, pos, notes) ->
      close_in input;
      handle_error ~notes msg pos
