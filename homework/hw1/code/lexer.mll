{
  open Parser
  open Lexing
  let drop_quotes s =
    String.sub s 1 (String.length s - 2)
}

let id = ['A'-'Z''a'-'z'](['_''A'-'Z''a'-'z''0'-'9']*)
let str = '"' ['_''A'-'Z''a'-'z''0'-'9'','';''-''?''.'' ''!']* '"'
let comment = '/''/' ['_''A'-'Z''a'-'z''0'-'9'','';''-''?''.'' ''!']* ['\r''\n']
  
rule token = parse
  [' '] { token lexbuf }
| comment {
  let pos = Lexing.lexeme_start_p lexbuf in
  (* Printf.printf "comment position bol=%d, cnum=%d\n" pos.pos_bol pos.pos_cnum; *)
  if pos.pos_bol = pos.pos_cnum then (new_line lexbuf; token lexbuf)
  else (new_line lexbuf; EOL) }
| ['\n'] { new_line lexbuf; EOL }
| "***" { EOP }
| "as" { AS }
| "principal" { PRINCIPAL }
| "return" { RETURN }
| "do" { DO }
| "to" { TO }
| "exit" { EXIT }
| "equal" { EQUAL }
| "notequal" { NOTEQUAL }
| "foreach" { FOREACH }
| "filtereach" { FILTEREACH }
| "change" { CHANGE }
| "password" { PASSWORD }
| "create" { CREATE }
| "in" { IN }
| "replacewith" { REPLACEWITH }
| "with" { WITH }
| "append" { APPEND } 
| "set" { SET }
| "read" { READ }
| "write" { WRITE }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "true" { TRUE }
| "false" { FALSE }
| "all" { ALL }
| "[]" { EMPTYLIST }
| "=" { EQUALS }
| "->" { ARROW }
| "{" { LBRACE }
| "}" { RBRACE }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| '.' { DOT }
| id as i  { if String.length i > 255 then failwith "Identifier > 255 characters" else ID i }
| str as s { let s' = drop_quotes s in if String.length s' > 65535 then failwith "String > 65535 characters" else STR s' }

