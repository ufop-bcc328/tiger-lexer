{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence: %s" sequence

  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str

  let append_char str ch =
    str ^ (String.make 1 (Char.chr ch))
}

let spaces = [' ' '\t']+
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let litint = digit+
let id = alpha+ (alpha | digit | '_')*

rule token = parse
  | spaces { token lexbuf }
  | "array" { ARRAY }

  (* WRITE THE MISSING LEXICAL RULES HERE *)
