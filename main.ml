open Printf
open Stream
open Lexer
open Lexing
open ParsingTools
open Flags

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

let open_grammar filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Printing.printi 1 7 "Parsing... ";
  let gfpm_file = parse_with_error lexbuf in
  Printing.printi_after_done 2 7 "Processing includecc... ";
  let gfpm_file = Includecc.includecc_file gfpm_file in
  Printing.printi_after_done 3 7 ("Typing: " ^ (Gfpm.summary gfpm_file) ^ "... ");
  let tgfpm_file = type_with_error gfpm_file in
  Printing.printi_after_done 4 7 ("Computing finite functions: " ^ (Tgfpm.summary tgfpm_file) ^ "... ");
  let tl1_file = Finfun.tl1_file tgfpm_file in
  Printing.printi_after_done 5 7 ("Processing records: " ^ (Tl1.summary tl1_file) ^ "... ");
  let tl2_file = Rec.tl2_file tl1_file in
  Printing.printi_after_done 6 7 ("Converting into EIDLPMCFG: " ^ (Tl2.summary tl2_file) ^ "... ");
  let grammar = Convert.convert_file tl2_file in
  (*Formal.G.print_grammar grammar;*)
  Printing.printi_after_done 7 7 ("Developing conditions: " ^ (Formal.G.summary grammar) ^ "... ");
  let grammar' = Formal.G.develop_grammar grammar in
  Printing.print_done ();
  (*Gfpm.print_file gfpm_file;
  print_newline ();
  Tgfpm.print_file tgfpm_file;
  print_newline ();
  Tl1.print_file tl1_file;
  print_newline ();
  Tl2.print_file tl2_file;
  print_newline ();
  Formal.G.print_grammar grammar;
  print_newline ();
  close_in inx;*)
  grammar'

let loop_test grammar_fn ts =
  let grammar' = open_grammar grammar_fn in
  let accepted = Parsing.P.parse grammar' ts in
  match accepted with
    true -> print_string "Accepted!\n"
  | _    -> print_string "Rejected!\n"

let loop_compile grammar_fn =
  let grammar' = open_grammar grammar_fn in
  Formal.G.print_grammar grammar'
       
let _ =
  Arg.parse [("-g", Set_string grammar_fn, "COMPA grammar to use");
             ("-t", Set_string text_to_parse, "Text to parse");
             ("-v", Set verbose, "Verbose");
             ("-s", Set statistics, "Show parsing statistics");
             ("-p", Set parse_trees, "Show parse trees");
             ("-d", Set_string png_to_draw, "Draw syntax tree into PNG file (if unique)");
             ("-dep", Set draw_ud, "Draw dependency tree instead of AST");
             ("-c", Set compile_only, "Compile only (do not parse)")]
    (fun _ -> ())
    "COMPAges Grammaticalis v0";
  let time = -. Unix.gettimeofday () in
  (match !compile_only with
     true  -> loop_compile !grammar_fn
   | false -> loop_test !grammar_fn (String.split_on_char ' ' !text_to_parse));
  let time = time +. Unix.gettimeofday () in
  print_string "(";
  print_float time;
  print_string "s)\n"
    
