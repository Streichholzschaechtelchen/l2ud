open Gf
open Printf
open Lexing

let get_tables filename =
  let com = Com.create filename in
  let funs = Com.cmd_fold com "pg -funs" [] (fun a s -> match parse_pgfun s
                                                        with Some f -> f::a
                                                           | None  -> a) in
  let tables = List.fold_left (fun acc (f, t) ->
                   let cmdstr = "cc -unqual " ^ f in
                   let table  = Com.cmd com cmdstr in
                   let table' = f ^ " : " ^ t ^ " = " ^ table ^ ";" in
                   Com.concat_ acc table') "lin\n" funs in
  Com.close com;
  tables

(*Code copied from main*)
let print_position_ outx pos =
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  print_position_ outx pos
                  
let parse_with_error lexbuf =
  try Parser.standalone_lins Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
     fprintf stderr "%a: %s\n" print_position lexbuf msg;
     exit (-1)
  | Parser.Error ->
     fprintf stderr "%a: syntax error\n" print_position lexbuf;
     exit (-1)

let get_lins filename =
  let tables = get_tables filename in
  let lexbuf = Lexing.from_string tables in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_with_error lexbuf

let update_lins =
  List.fold_left (fun lins fn -> (get_lins fn)@lins)

let includecc_file (file: Gfpm.file) : Gfpm.file =
  let lins = update_lins file.lins file.includeccs in
  { file with lins }
  
(*let filename = "/home/franz/GU/gf/gf-rgl/src/latin/LexiconLat.gf"*)
