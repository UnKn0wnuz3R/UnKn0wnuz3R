// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | LPAR
  | RPAR
  | ARROW
  | ASSIGN
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | REC
  | ID of (string)
  | NUM of (int)
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | EQUAL
  | NOTEQ
  | LESS
  | GREATER
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_ARROW
    | TOKEN_ASSIGN
    | TOKEN_SEMICOLON
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_FUN
    | TOKEN_REC
    | TOKEN_ID
    | TOKEN_NUM
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_EQUAL
    | TOKEN_NOTEQ
    | TOKEN_LESS
    | TOKEN_GREATER
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_expr
    | NONTERM_exp
    | NONTERM_exps
    | NONTERM_prog
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val prog : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (AST.Exp) 