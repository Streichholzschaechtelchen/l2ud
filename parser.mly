%{
  open Gfpm
%}

%token EOF
%token <string> STRING
%token <string> IDENT

(* Keywords *)                  
%token SET
%token CONCRETE INCLUDECC AS
%token LIN LINCAT PARAM
%token TABLE

(* Symbols *)
%token OPAREN CPAREN OCURLY CCURLY
%token COLON SEMICOLON BAR
%token EQUAL
%token BIG_RARROW
       
(* Operators *)
%token LOCK
%token LAMLAM
%token PLUSPLUS
%token BARBAR
%token DISJUNCTION
%token EXCLMARK
%token DOT

(* Precedence *)       
%right LOCK
%left BIG_RARROW
%left PLUSPLUS
%left BARBAR
%left DISJUNCTION
%left EXCLMARK
%left DOT

%start file
%type <Gfpm.file> file

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
    | s = STRING; OCURLY; a = aliases; CCURLY
      { { filename = s; aliases = a } }

aliases:
    | a = alias
      { [a] }
    | al = aliases; SEMICOLON; a = alias
      { a::al }

alias:
    | i1 = ident; AS; i2 = ident
      { { oldident = i1; newident = i2 } }

lincats:
    | (* empty *)      
      { [] }
    | LINCAT; lc = list(lincat)
      { lc }

lincat:
    | i = ident; EQUAL; t = typ
      { { lincat_name = i; lincat_type = t } }
                              
params:
    | (* empty *)      
      { [] }
    | PARAM; p = list(param)
      { p }

param:
    | i = ident; EQUAL; e = param_enum
      { { param_name = i; param_values = e } }

param_enum:
    | i = ident
      { [i] }
    | e = param_enum; BAR; i = ident
      { i::e }

lins:
    | (* empty *)      
      { [] }
    | LIN; l = list(lin)
      { l }

lin:
    | i1 = ident; a = list(lin_arg); COLON; i2 = ident; EQUAL; e = expr
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
    | OCURLY; r = record_fields; CCURLY
      { Erecord r }
    | TABLE; OCURLY; t = table_fields; CCURLY
      { Etable t }

record_fields:
    | (* empty *)
      { [] }
    | r = nonempty_record_fields
      { r }
            
nonempty_record_fields:
    | r = record_field
      { [r] }
    | re = nonempty_record_fields; SEMICOLON; r = record_field
      { r::re }

record_field:
    | i = ident; EQUAL; e = expr
      { (i,e) }

record_decl_fields:
    | (* empty *)
      { [] }
    | r = nonempty_record_decl_fields
      { r }
            
nonempty_record_decl_fields:
    | r = record_decl_field
      { [r] }
    | re = nonempty_record_decl_fields; SEMICOLON; r = record_decl_field
      { r::re }

record_decl_field:
    | i = ident; COLON; t = typ
      { (i,t) }
                                               
table_fields:
    | t = table_field
      { [t] }
    | ta = table_fields; SEMICOLON; t = table_field
      { t::ta }

table_field:
    | i = ident; BIG_RARROW; e = expr
      { (i,e) }
                              
ident:
    | i = IDENT
      { { id = i; id_loc = $startpos } }
                                                   

