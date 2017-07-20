let parse_pulsar_file ctx ic =
  let filename = Pass.current_file ctx in
  let lex = Lexer.ctx_from_utf8_channel ~filename ic in
  let supplier () =
    let tok, start, stop = Lexer.next_token_pos lex in
    if !Options.debug_lexing
    then Format.eprintf "%a @?" Lexer.print_token tok;
    tok, start, stop
  in
  let chk = Parser.Incremental.file Lexing.dummy_pos in
  let file = Parser.MenhirInterpreter.loop supplier chk in
  close_in ic;
  file

let pass =
  Pass.atomic
    ~pp_out:Raw_tree.T.print_file
    ~name:"parsing"
    parse_pulsar_file
