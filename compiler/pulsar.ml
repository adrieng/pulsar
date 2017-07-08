open Types
open Parser

let handle_internal_errors f x =
  try f x
  with
  | Lexer.Lexing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Lexer.print_lexing_error err
  | Parser_error.Parsing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Parser_error.print_parsing_error err

let parse_pulsar_file filename =
  let ic = open_in filename in
  let ctx = Lexer.ctx_from_utf8_channel ~filename ic in
  let supplier () =
    let tok, start, stop = Lexer.next_token_pos ctx in
    if !Options.debug_lexing
    then Format.eprintf "%a @?" Lexer.print_token tok;
    tok, start, stop
  in
  let chk = Parser.Incremental.file Lexing.dummy_pos in
  let file = Parser.MenhirInterpreter.loop supplier chk in
  close_in ic;
  file

let process_pulsar_file filename =
  let file = parse_pulsar_file filename in
  Format.printf "%a@?" Raw_tree.T.print_file file

let args =
    [
      "-utf8",
      Arg.Symbol (["yes"; "no"], Options.yes_no Options.pp_utf8),
      " use UTF-8 pretty-printing (default: yes)";
      "-debug",
      Arg.Symbol (Options.debug_options, Options.set_debug),
      " display debugging information";
    ]

let usage = "pulsar <options> <files>"

let files = ref []

let process filename =
  match Clock.Utils.file_extension filename with
  | ".pul" ->
    handle_internal_errors process_pulsar_file filename
  | _ ->
    Printf.eprintf "%s: don't know what to do with %s\n"
      Sys.argv.(0)
      filename;
    Arg.usage args usage

let _ =
  Arg.parse args (fun s -> files := s :: !files) usage;
  List.iter process (List.rev !files)
