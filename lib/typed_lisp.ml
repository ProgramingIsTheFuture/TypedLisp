let parse_code s =
  let buf = Lexing.from_string s in
  Parser.main Lexer.token buf

