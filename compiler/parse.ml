let parse_pulsar_file ctx ic =
  let filename = Pass.current_file ctx in
  let lex = Lexer.ctx_from_utf8_channel ~filename ic in
  let supplier () =
    let tok, start, stop = Lexer.next_token_pos lex in
    if !Options.debug_lexing
    then Format.eprintf "%a @?" Lexer.print_token tok;
    tok, start, stop
  in
  let chk =
    let initial_pos =
      Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 0; }
    in
    Parser.Incremental.file initial_pos
  in
  let file = Parser.MenhirInterpreter.loop supplier chk in
  close_in ic;
  file

let pass =
  Pass.atomic
    ~pp_out:Raw_tree.T.print_file
    ~name:"parsing"
    parse_pulsar_file
