{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

let open_comment  = "(*"

let close_comment = "*)"

rule token = parse
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { NUM (int_of_string x) }
  | "def" { DEF }
  | "add1" { ADD1 }
  | "sub1" { SUB1 }
  | "print" { PRINT }
  | "begin" { BEGIN }
  | "end" { END }
  | "if" { IF }
  | "true" { TRUE }
  | "false" { FALSE }
  | "isbool" { ISBOOL }
  | "ispair" { ISPAIR }
  | "isnum" { ISNUM }
  | "fst" { FST }
  | "snd" { SND }
  | "lambda" { LAMBDA }
  | "setfst" { SETFST }
  | "setsnd" { SETSND }
  | "input" { INPUT }
  | ":" { COLON }
  | ";" { SEMI }
  | "else:" { ELSECOLON }
  | "end" { END }
  | "let" { LET }
  | "in" { IN }
  | "==" { EQEQ }
  | "=" { EQUAL }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "<" { LESS }
  | ">" { GREATER }
  | ident as x { ID x }
  | open_comment { comment 1 lexbuf }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }

and comment depth = parse
  | open_comment  { comment (depth+1) lexbuf }
  | close_comment { if depth = 1
                    then token lexbuf
                    else comment (depth-1) lexbuf }
  | _             { comment depth lexbuf }
