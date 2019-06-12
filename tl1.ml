type ident = string

module T = struct
  type t = ident
  let compare = compare
end
                             
module M = Map.Make(T)

type alias = { oldident: ident;
               newident: ident }
  
type typ = ident list

type log = Land of log * log
         | Lor of log * log
         | Lnot of log
         | Lproject of ident * ident
  
type expr = Estring of string
          | Eident of ident
          | Eproject of ident * ident
          | Econcat of expr * expr
          | Einterl of expr * expr
          | Edisj of expr * expr
          | Elock of expr

type record = (ident * expr) list
                            
type lincat_map = typ M.t
       
type lin = { lin_outc: ident;
             lin_args: (ident * ident) list;
             lin_logc: log option;
             lin_rcrd: record }

type lin_map = lin M.t
                
type file = { name      : ident;
              lincats   : lincat_map;
              lins      : lin_map }

let print_ident = print_string

let print_typ t =
  print_string ((List.fold_left (fun s i -> s ^ i ^ " : Set; ") "{ " t ) ^ "}")

let print_expr e =
  let rec string_of_expr = function
      Estring s -> "\"" ^ s ^ "\""
    | Eident i  -> i
    | Eproject (i1, i2)-> "Project(" ^ i1 ^ ", " ^ i2 ^ ")"
    | Econcat (e1, e2) -> "Concat(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Einterl (e1, e2) -> "Interl(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Edisj (e1, e2)   -> "Disj(" ^ (string_of_expr e1) ^ ", "
                          ^ (string_of_expr e2) ^ ")"
    | Elock e          -> "Lock(" ^ (string_of_expr e) ^ ")"
  in print_string (string_of_expr e) 
    
let print_rcrd r =
  let print_field (i, e) =
    print_string i; print_string " = "; print_expr e; print_string "; "
  in print_string "{ "; List.iter print_field r; print_string "}"

let print_logc l =
  let rec string_of_logc = function
      Land (l1, l2)    -> "And(" ^ (string_of_logc l1) ^ ", "
                         ^ (string_of_logc l2) ^ ")"
    | Lor (l1, l2)     -> "Or(" ^ (string_of_logc l1) ^ ", "
                         ^ (string_of_logc l2) ^ ")"
    | Lnot l'          -> "Not(" ^ (string_of_logc l') ^ ")"
    | Lproject (i1, i2)-> "Project(" ^ i1 ^ ", " ^ i2 ^ ")"
  in match l with
       None -> ()
     | Some l' -> print_string (" if " ^ (string_of_logc l'))

let print_lin l =
  List.iter (fun (a, t) -> print_string ("(" ^ a ^ " : " ^ t ^ ") ")) l.lin_args;
  print_string (": " ^ l.lin_outc ^ " = ");
  print_rcrd l.lin_rcrd;
  print_logc l.lin_logc

let print_file f =
  print_string ("<Tl1 file " ^ f.name ^ ">\n");
  print_string "lincat\n";
  M.iter (fun i t -> print_string ("  " ^ i ^ " = "); print_typ t; print_newline ()) f.lincats;
  print_string "lin\n";
  M.iter (fun i l -> print_string ("  " ^ i ^ " "); print_lin l; print_newline ()) f.lins
