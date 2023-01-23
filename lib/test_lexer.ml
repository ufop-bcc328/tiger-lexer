(* inline expect tests for the scanner *)

let scan_string s =
  let lexbuf = Lexing.from_string s in
  let rec loop () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location
      (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  try loop ()
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg

let%expect_test "spaces" =
  scan_string " \n\t   \n  ";
  [%expect{| :3.2-3.2 Parser.EOF |}]

let%expect_test "comments" =
  scan_string "/* this is a comment */";
  [%expect{| :1.23-1.23 Parser.EOF |}];

  scan_string "/* this /* is */ /* a /* deep nested */ */ comment */";
  [%expect{| :1.53-1.53 Parser.EOF |}];

  scan_string {|
               /* a
                  multiline
                  comment
               */
               |};
  [%expect{| :6.15-6.15 Parser.EOF |}];

  scan_string "/* this comment /* is unclosed */ here";
  [%expect{| :1.0-1.38 error: unterminated comment |}]

let%expect_test "integer literals" =
  scan_string "27348";
  [%expect{|
    :1.0-1.5 (Parser.INT 27348)
    :1.5-1.5 Parser.EOF |}];

  scan_string "-27348";
  [%expect{|
    :1.0-1.1 Parser.MINUS
    :1.1-1.6 (Parser.INT 27348)
    :1.6-1.6 Parser.EOF |}];

  scan_string "+27348";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.1-1.6 (Parser.INT 27348)
    :1.6-1.6 Parser.EOF |}]

let%expect_test "string literals" =
  scan_string {|
               "true"
               "good\tmorning"
               "good\nafternoon"
               "name: \"Tom\"."
               "2\\3"
               "first vowel: \065"
               "tab|\^I|"
               |};
  [%expect{|
    :2.15-2.21 (Parser.STR "true")
    :3.15-3.30 (Parser.STR "good\tmorning")
    :4.15-4.32 (Parser.STR "good\nafternoon")
    :5.15-5.31 (Parser.STR "name: \"Tom\".")
    :6.15-6.21 (Parser.STR "2\\3")
    :7.15-7.34 (Parser.STR "first vowel: A")
    :8.15-8.25 (Parser.STR "tab|\t|")
    :9.15-9.15 Parser.EOF |}];

  scan_string {|"ignore \     		     \ inner spaces"|};
  [%expect{|
    :1.0-1.36 (Parser.STR "ignore  inner spaces")
    :1.36-1.36 Parser.EOF |}];

  scan_string {|"ignore\
     	       \ inner spaces"|};
  [%expect{|
    :1.0-2.14 (Parser.STR "ignore inner spaces")
    :2.14-2.14 Parser.EOF |}];

  scan_string {|"bad \s!"|};
  [%expect{| :1.5-1.7 error: illegal escape sequence: \s |}];

  scan_string {|"forget to close the string|};
  [%expect{| :1.0-1.27 error: unterminated string |}]

let%expect_test "reserved words" =
  scan_string "array";
  [%expect{|
    :1.0-1.5 Parser.ARRAY
    :1.5-1.5 Parser.EOF |}];

  scan_string "break";
  [%expect{|
    :1.0-1.5 Parser.BREAK
    :1.5-1.5 Parser.EOF |}];

  scan_string "do";
  [%expect{|
    :1.0-1.2 Parser.DO
    :1.2-1.2 Parser.EOF |}];

  scan_string "else";
  [%expect{|
    :1.0-1.4 Parser.ELSE
    :1.4-1.4 Parser.EOF |}];

  scan_string "end";
  [%expect{|
    :1.0-1.3 Parser.END
    :1.3-1.3 Parser.EOF |}];

  scan_string "for";
  [%expect{|
    :1.0-1.3 Parser.FOR
    :1.3-1.3 Parser.EOF |}];

  scan_string "function";
  [%expect{|
    :1.0-1.8 Parser.FUNCTION
    :1.8-1.8 Parser.EOF |}];

  scan_string "if";
  [%expect{|
    :1.0-1.2 Parser.IF
    :1.2-1.2 Parser.EOF |}];

  scan_string "in";
  [%expect{|
    :1.0-1.2 Parser.IN
    :1.2-1.2 Parser.EOF |}];

  scan_string "let";
  [%expect{|
    :1.0-1.3 Parser.LET
    :1.3-1.3 Parser.EOF |}];

  scan_string "nil";
  [%expect{|
    :1.0-1.3 Parser.NIL
    :1.3-1.3 Parser.EOF |}];

  scan_string "of";
  [%expect{|
    :1.0-1.2 Parser.OF
    :1.2-1.2 Parser.EOF |}];

  scan_string "then";
  [%expect{|
    :1.0-1.4 Parser.THEN
    :1.4-1.4 Parser.EOF |}];

  scan_string "to";
  [%expect{|
    :1.0-1.2 Parser.TO
    :1.2-1.2 Parser.EOF |}];

  scan_string "type";
  [%expect{|
    :1.0-1.4 Parser.TYPE
    :1.4-1.4 Parser.EOF |}];

  scan_string "var";
  [%expect{|
    :1.0-1.3 Parser.VAR
    :1.3-1.3 Parser.EOF |}];

  scan_string "while";
  [%expect{|
    :1.0-1.5 Parser.WHILE
    :1.5-1.5 Parser.EOF |}]

let%expect_test "punctuation" =
  scan_string ", : ;";
  [%expect{|
    :1.0-1.1 Parser.COMMA
    :1.2-1.3 Parser.COLON
    :1.4-1.5 Parser.SEMICOLON
    :1.5-1.5 Parser.EOF |}];

  scan_string "()";
  [%expect{|
    :1.0-1.1 Parser.LPAREN
    :1.1-1.2 Parser.RPAREN
    :1.2-1.2 Parser.EOF |}];

  scan_string "[]";
  [%expect{|
    :1.0-1.1 Parser.LBRACK
    :1.1-1.2 Parser.RBRACK
    :1.2-1.2 Parser.EOF |}];

  scan_string "{}";
  [%expect{|
    :1.0-1.1 Parser.LBRACE
    :1.1-1.2 Parser.RBRACE
    :1.2-1.2 Parser.EOF |}];

  scan_string ".";
  [%expect{|
    :1.0-1.1 Parser.DOT
    :1.1-1.1 Parser.EOF |}];

  scan_string "+ - * /";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.2-1.3 Parser.MINUS
    :1.4-1.5 Parser.TIMES
    :1.6-1.7 Parser.DIV
    :1.7-1.7 Parser.EOF |}];

  scan_string "= <> < <= > >=";
  [%expect{|
    :1.0-1.1 Parser.EQ
    :1.2-1.4 Parser.NE
    :1.5-1.6 Parser.LT
    :1.7-1.9 Parser.LE
    :1.10-1.11 Parser.GT
    :1.12-1.14 Parser.GE
    :1.14-1.14 Parser.EOF |}];

  scan_string "& |";
  [%expect{|
    :1.0-1.1 Parser.AND
    :1.2-1.3 Parser.OR
    :1.3-1.3 Parser.EOF |}];

  scan_string ":=";
  [%expect{|
    :1.0-1.2 Parser.ASSIGN
    :1.2-1.2 Parser.EOF |}]

let%expect_test "identifiers" =
  scan_string "Idade alfa15 beta_2";
  [%expect{|
    :1.0-1.5 (Parser.ID "Idade")
    :1.6-1.12 (Parser.ID "alfa15")
    :1.13-1.19 (Parser.ID "beta_2")
    :1.19-1.19 Parser.EOF |}];

  scan_string "_altura";
  [%expect{| :1.0-1.1 error: illegal character '_' |}];

  scan_string "5x";
  [%expect{|
    :1.0-1.1 (Parser.INT 5)
    :1.1-1.2 (Parser.ID "x")
    :1.2-1.2 Parser.EOF |}]
