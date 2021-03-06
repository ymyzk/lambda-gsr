{

let reservedWords = [
  ("fun", Parser.FUN);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("int", Parser.INT);
  ("bool", Parser.BOOL);
  ("unit", Parser.UNIT);
  ("shift", Parser.SHIFT);
  ("reset", Parser.RESET);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
]

}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+ { main lexbuf }
| ['0'-'9']+ { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ":" { Parser.COLON }
| ";" { Parser.SEMI }
| ";;" { Parser.SEMISEMI }
| "->" { Parser.RARROW }
| "-" { Parser.MINUS }
| "/" { Parser.SLASH }
| "+" { Parser.PLUS }
| "*" { Parser.STAR }
| "?" { Parser.QUESTION }
| "^" { Parser.CARET }
| "#" { Parser.SHARP }
| "=" { Parser.EQUAL }
| ">" { Parser.GT }
| "<" { Parser.LT }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with
    _ -> Parser.ID id
  }
| eof { exit 0 }
