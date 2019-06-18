{

open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

(* l2ud does not support unicode *)
let d = ['0'-'9']
let l = ['a'-'z' 'A'-'Z']
let c = ['A'-'Z']
let s = ['a'-'z']
let i = (l|d)
let whitespace = [' ' '\t']+
let identifier = (l|'_')(i|'_'|'\'')*

(* l2ud does not support integers *)
rule read = parse
  | whitespace { read lexbuf }
  | '\n'       { next_line lexbuf; read lexbuf }
  | "--"       { read_single_line_comment lexbuf }
  | "{-"       { read_multiline_comment lexbuf }
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | '!'        { EXCLMARK }
  | '('        { OPAREN }
  | ')'        { CPAREN }
  | "++"       { PLUSPLUS }
  | "||"       { BARBAR } (*INTERLEAVE*) (* IDL operators *)
  | '`'        { LOCK }
  | "\/"       { DISJUNCTION }
  | '.'        { DOT }
  | ':'        { COLON }
  | ';'        { SEMICOLON }
  | '='        { EQUAL }
  | "\\\\"     { LAMLAM }
  | "=>"       { BIG_RARROW }
  | '{'        { OCURLY }
  | '}'        { CCURLY }
  | '['        { OBRACK }
  | ']'        { CBRACK }
  | '|'        { BAR }
  | "<>"       { GFLOCK }
  | "and"      { AND }
  | "or"       { OR }
  | "not"      { NOT }
  | "True"     { TRUE }
  | "False"    { FALSE }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "skip"     { SKIP }
  | "Set"      { SET }
  | "concrete" { CONCRETE } (* No abstract syntax, no flags, no printname *)
  | "lin"      { LIN }
  | "lincat"   { LINCAT }
  | "param"    { PARAM }
  | "table"    { TABLE }
  | "includecc"{ INCLUDECC }
  | identifier as ident
               { IDENT (ident) }
  | eof        { EOF }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | '\n'       { next_line lexbuf; read lexbuf }
  | eof        { EOF }
  | _          { read_single_line_comment lexbuf }

and read_multiline_comment = parse
  | "-}"       { read lexbuf }
  | '\n'       { next_line lexbuf; read lexbuf }
  | eof        { EOF }
  | _          { read_multiline_comment lexbuf }

(* copy-pasted from the OCaml tutorial *)
(* forbid multiline strings ? *)
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '\\']  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                read_string buf lexbuf
              }
  | eof { raise (SyntaxError ("String is not terminated")) }        
