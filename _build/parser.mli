type token =
  | INT of (int)
  | BOOL of (bool)
  | LOCATION of (int)
  | ADD
  | GTE
  | SEMICOLON
  | SKIP
  | DEREF
  | ASSIGN
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
