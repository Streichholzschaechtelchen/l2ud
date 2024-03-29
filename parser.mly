%{
  open Gfpm
%}

%token EOF
%token <string> STRING
%token <string> IDENT

(* Keywords *)                  
%token SET
%token CONCRETE INCLUDECC
%token LIN LINCAT PARAM
%token TABLE
%token TRUE FALSE
%token IF THEN ELSE SKIP
%token FOR DO

(* Symbols *)
%token OPAREN CPAREN OCURLY CCURLY OBRACK CBRACK
%token AND OR NOT
%token EQUAL
%token BIG_RARROW
%token GFLOCK
%token EPSILON EMPTY
       
(* Operators *)
%token LOCK
%token LAMLAM
%token PLUSPLUS
%token BARBAR
%token DISJUNCTION
%token EXCLMARK
%token DOT
%token COLON SEMICOLON BAR

(* Precedence *)
%left BIG_RARROW
%nonassoc DO
%nonassoc ELSE
%left DISJUNCTION
%left BARBAR
%left PLUSPLUS
%right LOCK
%left OR
%left AND
%left NOT
%left EXCLMARK
%left DOT

%start file standalone_lins
%type <Gfpm.file> file
%type <Gfpm.lin list> standalone_lins

%%
                    
file:
    | CONCRETE; i = ident; EQUAL; OCURLY; ic = includeccs; p = params; lc = lincats; l = lins; CCURLY; EOF
      { { name = i; includeccs = ic; lincats = lc; params = p; lins = l } }

includeccs:
    | (* empty *)
      { [] }
    | INCLUDECC; i = list(includecc)
      { i }

includecc:
    | s = STRING; SEMICOLON
      { s }

lincats:
    | (* empty *)      
      { [] }
    | LINCAT; lc = list(lincat)
      { lc }

lincat:
    | i = ident; EQUAL; t = typ; SEMICOLON
      { { lincat_name = i; lincat_type = t } }
                              
params:
    | (* empty *)      
      { [] }
    | PARAM; p = list(param)
      { p }

param:
    | i = ident; EQUAL; e = param_enum; SEMICOLON
      { { param_name = i; param_values = e } }

param_enum:
    | i = ident
      { [i] }
    | i = ident; BAR; e = param_enum
      { i::e }

standalone_lins:
    | l = lins; EOF
      { l }
      
lins:
    | (* empty *)      
      { [] }
    | LIN; l = list(lin)
      { l }

lin:
    | i1 = ident; a = list(lin_arg); COLON; i2 = ident; EQUAL; e = expr; SEMICOLON
      { { lin_name = i1; lin_outc = i2; lin_args = a; lin_expr = e } }

lin_arg:
    | OPAREN; i1 = ident; COLON; i2 = ident; CPAREN
      { (i1, i2) }

typ:
    | i = ident
      { Tident i }
    | SET
      { Tset }
    | i = ident; BIG_RARROW; t = typ
      { Ttable (i, t) }                                    
    | OCURLY; r = record_decl_fields; CCURLY
      { Trecord r } 

expr:
    | e = expr_node
      { { expr_node = e;
          expr_loc  = $startpos } }
      
expr_node:
    | EPSILON
      { Eepsilon }
    | EMPTY
      { Eempty }    
    | s = STRING
      { Estring s }
    | i = ident
      { Eident i }
    | e1 = expr; EXCLMARK; e2 = expr
      { Eselect (e1, e2) }
    | OPAREN; e = expr; CPAREN
      { Eblock e }
    | e1 = expr; PLUSPLUS; e2 = expr
      { Econcat (e1, e2) }
    | e1 = expr; BARBAR; e2 = expr
      { Einterl (e1, e2) }
    | LOCK; e = expr;
      { Elock e }
    | e1 = expr; DISJUNCTION; e2 = expr
      { Edisj (e1, e2) }
    | e = expr; DOT; i = ident
      { Eproject (e, i) }
    | LAMLAM; i1 = ident; COLON; i2 = ident; BIG_RARROW; e = expr
      { Elambda (i1,i2,e) }
    | FOR; i1 = ident; COLON; i2 = ident; DO; e = expr
      { Efor (i1,i2,e) }
    | OCURLY; r = record_fields; CCURLY
      { Erecord r }
    | TABLE; OCURLY; t = table_fields; CCURLY
      { Etable t }
    | TABLE; i = ident; OBRACK; c = covtable_fields; CBRACK
      { Ecovtable (i, c) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
      { Eif (e1, e2, e3) }
    | e1 = expr; AND; e2 = expr
      { Eand (e1, e2) }
    | e1 = expr; OR; e2 = expr
      { Eor (e1, e2) }
    | NOT; e1 = expr
      { Enot e1 }
    | SKIP
      { Eskip }
    | TRUE
      { Etrue }
    | FALSE
      { Efalse }

record_fields:
    | (* empty *)
      { [] }
    | r = nonempty_record_fields
      { r }
            
nonempty_record_fields:
    | r = record_field; SEMICOLON?
      { [r] }
    | lock_field; SEMICOLON?
      { [] }
    | r = record_field; SEMICOLON; re = nonempty_record_fields
      { r::re }
    | lock_field; SEMICOLON; re = nonempty_record_fields
      { re }

record_field:
    | i = ident; EQUAL; e = expr
      { (i,e) }

lock_field:
    | i = ident; EQUAL; GFLOCK
      { [] }

record_decl_fields:
    | (* empty *)
      { [] }
    | r = nonempty_record_decl_fields
      { r }
            
nonempty_record_decl_fields:
    | r = record_decl_field; SEMICOLON?
      { [r] }
    | r = record_decl_field; SEMICOLON; re = nonempty_record_decl_fields
      { r::re }

record_decl_field:
    | i = ident; COLON; t = typ
      { (i,t) }
                                               
table_fields:
    | t = table_field; SEMICOLON?
      { [t] }
    | t = table_field; SEMICOLON; ta = table_fields
      { t::ta }

table_field:
    | i = ident; BIG_RARROW; e = expr
      { (i,e) }

covtable_fields:
    | e = expr; SEMICOLON?
      { [e] }
    | e = expr; SEMICOLON; c = covtable_fields
      { e::c }
                              
ident:
    | i = IDENT
      { { id = i; id_loc = $startpos } }
                                                   

