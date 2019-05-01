type ident = Tl1.ident
                             
module M = Tl1.M

type alias = { oldident: ident;
               newident: ident }
  
type expr = Estring of string
          | Eident of ident
          | Eproject of ident * int
          | Econcat of expr * expr
          | Einterl of expr * expr
          | Edisj of expr * expr
          | Elock of expr

type record = expr array
                            
type includecc = Gfpm.includecc (*{ filename: string;
                   aliases : alias list }*)

type lincat_map = int M.t
       
type lin = { lin_outc: ident;
             lin_args: (ident * ident) list;
             lin_rcrd: record }

type lin_map = lin M.t
                
type file = { name      : ident;
              includeccs: includecc list;
              lincats   : lincat_map;
              lins      : lin_map }

let print_ident = print_string

let print_expr e =
  let rec string_of_expr = function
      Estring s -> "\"" ^ s ^ "\""
    | Eident i  -> i
    | Eproject (i1, i2)-> i1 ^ "[" ^ (string_of_int i2) ^ "]"
    | Econcat (e1, e2) -> "Concat(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Einterl (e1, e2) -> "Interl(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Edisj (e1, e2)   -> "Disj(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Elock e          -> "Lock(" ^ (string_of_expr e) ^ ")"
  in print_string (string_of_expr e) 
    
let print_rcrd r =
  let print_field e =
    print_expr e; print_string "; "
  in print_string "[ "; Array.iter print_field r; print_string "]"

let print_lin l =
  List.iter (fun (a, t) -> print_string ("(" ^ a ^ " : " ^ t ^ ") ")) l.lin_args;
  print_string (": " ^ l.lin_outc ^ " = ");
  print_rcrd l.lin_rcrd

let print_file f =
  print_string ("<Tl2 file " ^ f.name ^ ">\n");
  print_string "lincat\n";
  M.iter (fun i a -> print_string ("  " ^ i ^ "(" ^ (string_of_int a) ^ ")\n")) f.lincats;
  print_string "lin\n";
  M.iter (fun i l -> print_string ("  " ^ i ^ " "); print_lin l; print_newline ()) f.lins
