open Parse
open Parser
open Pass

let handle_internal_errors f x =
  try f x
  with
  | Lexer.Lexing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Lexer.print_lexing_error err
  | Parser_error.Parsing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Parser_error.print_parsing_error err
  | Scoping.Scoping_error err ->
    Format.eprintf "%a@." Scoping.print_scoping_error err
  | Typing.Typing_error err ->
    Format.eprintf "%a@." Typing.print_typing_error err

let frontend =
  Parse.pass
  >>> Scoping.pass
  >>> Typing.pass
  >>> Typing.serialize

let compiler =
  frontend

let process_pulsar_file filename =
  let ctx = Pass.make_default ~filename in
  let ic = open_in filename in
  ignore @@ Pass.run ~ctx compiler ic

let args =
    [
      "-utf8",
      Arg.Symbol (["yes"; "no"], Options.yes_no Options.pp_utf8),
      " use UTF-8 pretty-printing (default: yes)";
    ]
    @ Pass.command_line_arguments compiler

let usage = "pulsar <options> <files>"

let files = ref []

let process filename =
  match Warp.Utils.file_extension filename with
  | ".pul" ->
    handle_internal_errors process_pulsar_file filename
  | _ ->
    Printf.eprintf "%s: don't know what to do with %s\n"
      Sys.argv.(0)
      filename;
    Arg.usage args usage;
    exit 1

let _ =
  Arg.parse args (fun s -> files := s :: !files) usage;
  List.iter process (List.rev !files)
