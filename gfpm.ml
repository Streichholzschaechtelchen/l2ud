type ident = { id    : string;
               id_loc: Lexing.position }

type alias = { oldident: ident;
               newident: ident }

type typ = Tset
         | Tident of ident
         | Trecord of record_decl
         | Ttable of ident * typ
                                    
and record_decl = (ident * typ) list

type expr_nod = Estring of string
              | Eident of ident
              | Eselect of expr * expr
              | Eproject of expr * ident
              | Eblock of expr
              | Econcat of expr * expr
              | Einterl of expr * expr
              | Edisj of expr * expr
              | Elock of expr
              | Elambda of ident * ident * expr
              | Erecord of record
              | Etable of table
              | Ecovtable of ident * expr list

and table = (ident * expr) list
and record = (ident * expr) list

and expr = { expr_node: expr_nod;
             expr_loc : Lexing.position }
                            
type includecc = string

type param = { param_name  : ident;
               param_values: ident list }
  
type lincat = { lincat_name: ident;
                lincat_type: typ }

type lin = { lin_name: ident;
             lin_outc: ident;
             lin_args: (ident * ident) list;
             lin_expr: expr }
                
type file = { name      : ident;
              includeccs: includecc list;
              lincats   : lincat list;
              params    : param list;
              lins      : lin list }

let print_ident = print_string

let print_typ t =
  let rec string_of_typ = function
    | Tset -> "Set"
    | Tident i -> i.id
    | Ttable (i,t) -> i.id ^ " => " ^ (string_of_typ t)
    | Trecord l -> (List.fold_left (fun s (i,t) -> s ^ i.id ^ " : " ^ (string_of_typ t) ^ "; ") "{ " l) ^ " }"
  in print_string (string_of_typ t)

let print_expr e =
  let rec string_of_expr e = match e.expr_node with
      Estring s -> "\"" ^ s ^ "\""
    | Eident i  -> i.id
    | Eselect (e1, e2) -> "Select(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Eproject (e, i)  -> "Project(" ^ (string_of_expr e) ^ ", "
                          ^ i.id ^ ")"
    | Eblock e  -> "(" ^ (string_of_expr e) ^ ")"
    | Econcat (e1, e2) -> "Concat(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Einterl (e1, e2) -> "Interl(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Edisj (e1, e2)   -> "Disj(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Elock e          -> "Lock(" ^ (string_of_expr e) ^ ")"
    | Elambda (i1, i2, e) -> "Lambda(" ^ i1.id ^ ":" ^ i2.id ^ ", "
                             ^ (string_of_expr e) ^ ")"
    | Erecord r        -> (List.fold_left (fun s rr -> s ^ (string_of_record rr) ^ "; ") "Record(" r) ^ ")"
    | Etable t         -> (List.fold_left (fun s tt -> s ^ (string_of_table tt) ^ "; ") "Table(" t) ^ ")"
    | Ecovtable (p, es)-> (List.fold_left (fun s ee -> s ^ (string_of_expr ee) ^ "; ") ("COVTable:" ^ p.id ^ "(") es) ^ ")"
  and string_of_record r = 
    (fst r).id ^ " = " ^ (string_of_expr (snd r))
  and string_of_table t =
    (fst t).id ^ " => " ^ (string_of_expr (snd t))
  in print_string (string_of_expr e) 

let print_lin l =
  List.iter (fun (a, t) -> print_string ("(" ^ a ^ " : " ^ t ^ ") "))
    (List.map (fun (a,b) -> a.id, b.id) l.lin_args);
  print_string (": " ^ l.lin_outc.id ^ " = ");
  print_expr l.lin_expr

let print_values p =
  print_string (List.hd p).id;
  List.iter (fun v -> print_string (" | " ^ v.id)) (List.tl p)
                  
let print_file f =
  print_string ("<Gfpm file " ^ f.name.id ^ ">\n");
  print_string "param\n";
  List.iter (fun p -> print_string ("  " ^ p.param_name.id ^ " = "); print_values p.param_values; print_newline ()) f.params;
  print_string "lincat\n";
  List.iter (fun l -> print_string ("  " ^ l.lincat_name.id ^ " = "); print_typ l.lincat_type; print_newline ()) f.lincats;
  print_string "lin\n";
  List.iter (fun l -> print_string ("  " ^ l.lin_name.id ^ " "); print_lin l; print_newline ()) f.lins
