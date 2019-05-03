type ident = string

module T = struct
  type t = ident
  let compare = compare
end
                             
module M = Map.Make(T)

type alias = { oldident: ident;
               newident: ident }
  
type typ = Ttyp  (* the type of finite types *)
         | Tset
         | Tparam of ident (* the type of param values *)
         | Ttable of ident * typ
         | Trecord of (ident * typ) list
  
type expr = { expr_node: expr_nod;
              expr_type: typ }
               
and expr_nod = Estring of string 
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

and table = (ident * expr) list
and record = (ident * expr) list

type param_map = ident list M.t

type lincat_map = typ M.t
       
type lin = { lin_outc: ident;
             lin_args: (ident * ident) list;
             lin_expr: expr }

type lin_map = lin M.t
                
type file = { name      : ident;
              lincats   : lincat_map;
              params    : param_map;
              lins      : lin_map }

let print_ident = print_string

let print_values p =
  print_string (List.hd p);
  List.iter (fun v -> print_string (" | " ^ v)) (List.tl p)
  
let print_typ t =
  let rec string_of_typ = function
      Ttyp -> "Typ"
    | Tset -> "Set"
    | Tparam i -> i
    | Ttable (i,t) -> i ^ " => " ^ (string_of_typ t)
    | Trecord l -> (List.fold_left (fun s (i,t) -> s ^ i ^ " : " ^ (string_of_typ t) ^ "; ") "{ " l) ^ "}"
  in print_string (string_of_typ t)

let print_expr_node e =
  let rec string_of_expr_node = function
      Estring s -> "\"" ^ s ^ "\""
    | Eident i  -> i
    | Eselect (e1, e2) -> "Select(" ^ (string_of_expr_node e1.expr_node) ^ ", "
                          ^ (string_of_expr_node e2.expr_node) ^ ")"
    | Eproject (e, i)  -> "Project(" ^ (string_of_expr_node e.expr_node) ^ ", "
                          ^ i ^ ")"
    | Eblock e  -> "(" ^ (string_of_expr_node e.expr_node) ^ ")"
    | Econcat (e1, e2) -> "Concat(" ^ (string_of_expr_node e1.expr_node) ^ ", "
                          ^ (string_of_expr_node e2.expr_node) ^ ")"
    | Einterl (e1, e2) -> "Interl(" ^ (string_of_expr_node e1.expr_node) ^ ", "
                          ^ (string_of_expr_node e2.expr_node) ^ ")"
    | Edisj (e1, e2)   -> "Disj(" ^ (string_of_expr_node e1.expr_node) ^ ", "
                          ^ (string_of_expr_node e2.expr_node) ^ ")"
    | Elock e          -> "Lock(" ^ (string_of_expr_node e.expr_node) ^ ")"
    | Elambda (i1, i2, e) -> "Lambda(" ^ i1 ^ ":" ^ i2 ^ ", "
                             ^ (string_of_expr_node e.expr_node) ^ ")"
    | Erecord r        -> (List.fold_left (fun s rr -> s ^ (string_of_record rr) ^ "; ") "Record(" r) ^ ")"
    | Etable t         -> (List.fold_left (fun s tt -> s ^ (string_of_table tt) ^ "; ") "Table(" t) ^ ")"
  and string_of_record r =
    (fst r) ^ " = " ^ (string_of_expr_node (snd r).expr_node)
  and string_of_table t =
    (fst t) ^ " => " ^ (string_of_expr_node (snd t).expr_node)
  in print_string (string_of_expr_node e) 
    
let print_lin l =
  List.iter (fun (a, t) -> print_string ("(" ^ a ^ " : " ^ t ^ ") ")) l.lin_args;
  print_string (": " ^ l.lin_outc ^ " = ");
  print_expr_node l.lin_expr.expr_node

let print_file f =
  print_string ("<Tgfpm file " ^ f.name ^ ">\n");
  print_string "param\n";
  M.iter (fun i v -> print_string ("  " ^ i ^ " = "); print_values v; print_newline ()) f.params;
  print_string "lincat\n";
  M.iter (fun i t -> print_string ("  " ^ i ^ " = "); print_typ t; print_newline ()) f.lincats;
  print_string "lin\n";
  M.iter (fun i l -> print_string ("  " ^ i ^ " "); print_lin l; print_newline ()) f.lins
