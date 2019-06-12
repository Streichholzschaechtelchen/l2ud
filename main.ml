open Printf
open Stream
open Lexer
open Lexing

let print_position_ outx pos =
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  print_position_ outx pos
  
let parse_with_error lexbuf =
  try Parser.file Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let type_with_error gpfm_file =
  try Typing.type_file gpfm_file with
    Typing.TypingError (loc, msg) -> 
    fprintf stderr "%a: %s\n" print_position_ loc msg;
    exit (-1)

let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let gfpm_file = parse_with_error lexbuf in
  let gfpm_file = Includecc.includecc_file gfpm_file in
  let tgfpm_file = type_with_error gfpm_file in
  let tl1_file = Finfun.tl1_file tgfpm_file in
  let tl2_file = Rec.tl2_file tl1_file in
  let grammar = Convert.convert_file tl2_file in
  Gfpm.print_file gfpm_file;
  print_newline ();
  Tgfpm.print_file tgfpm_file;
  print_newline ();
  Tl1.print_file tl1_file;
  print_newline ();
  Tl2.print_file tl2_file;
  close_in inx

let _ =
  loop "test.gfpm";
  print_string "ok!\n"
    
