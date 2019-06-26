exception CannotParseToUD

let nummerlappar =
  object (self)
    val mutable c = 0
    method reset () =
      c <- 0
    method next () =
      let d = c in
      c <- c + 1;
      "l" ^ (string_of_int d)
    method all i =
      if i >= c then []
      else ("l" ^ (string_of_int i))::(self#all (i+1))
                                        
  end
            
module AST (G: ParsingTools.Grammar) = struct

  type t = Token of G.T.t  * int * G.N.t
         | Leaf of G.N.t
         | Node of G.N.t * ((G.N.t * t) list)
      
  let rec print_indent = function
      i when i <= 0 -> ()
    | i when i > 0  -> print_string " "; print_indent (i-1)
    | _             -> assert false
                                          
  let rec print ?(indent=0) =
    function
      Token (t, _, n) -> begin G.T.print t;
                               print_string " : ";
                               G.N.print n
                         end
    | Leaf n          -> G.N.print n
    | Node (n, [])    -> G.N.print n
    | Node (n, l)     -> let indent = indent - (pos_out stdout) in
                         print_string "(";
                         G.N.print n;
                         print_string " ";
                         let indent = indent + (pos_out stdout) in
                         List.iter (fun (lbl, a) -> print_newline ();
                                                    print_indent indent;
                                                    let indent = indent - (pos_out stdout) in
                                                    G.N.print lbl;
                                                    print_string " = ";
                                                    let indent = indent + (pos_out stdout) in
                                                    print ~indent a) l;
                         print_string ")"

    
  let to_dot a =
    nummerlappar#reset ();
    let add_vertex_t t s =
      let l = nummerlappar#next () in
      l, s ^ l ^ "[label=\"" ^ (G.T.to_string t) ^ "\", shape=box];\n" in
    let add_vertex_n n s =
      let l = nummerlappar#next () in
      l, s ^ l ^ "[label=\"" ^ (G.N.to_string n) ^ "\"];\n" in
    let add_edge a b s =
      s ^ a ^ " -> " ^ b ^ ";\n" in
    let rec aux ?(nohead=false) head s = function
        Token (t, _, n) -> let ln, s = add_vertex_n n s in
                           let lt, s = add_vertex_t t s in
                           let s = if nohead then s else add_edge head ln s in
                           add_edge ln lt s
      | Leaf n          -> let ln, s = add_vertex_n n s in
                           (if nohead then s else add_edge head ln s)
      | Node (n, l)     -> let ln, s = add_vertex_n n s in
                           let s = if nohead then s else add_edge head ln s in
                           List.fold_left (fun s (_, a) -> aux ln s a) s l
    in (aux ~nohead:true "" "digraph G {\n" a) ^ "}"


  exception ConnotConvertToUD
              
  let to_dot_dep a =
    let lbl_of_int i =
      "l" ^ (string_of_int i) in
    let add_vertex t i s =
      let x = string_of_float ((float_of_int i) *. 1.5) in
      s ^ (match t with
             None   -> ("root[style=invis,pos=\"" ^ x ^ ",1!\"]\n")
           | Some t -> ((lbl_of_int i) ^ "[style=rounded,shape=rectangle,label=" ^ (G.T.to_string t)
                        ^ ",pos=\"" ^ x ^ ",0!\"];\n")) in
    let add_edge i lbl j s =
      s ^ (lbl_of_int i) ^ " -> " ^ (lbl_of_int j) ^ "[label=" ^ (G.N.to_string lbl) ^ "];\n" in
    let rec aux s = function
        Token (t, i, _) -> i, add_vertex (Some t) i s
      | Leaf n          -> raise CannotParseToUD
      | Node (_, l)     -> let heads = List.filter (fun (n, a) -> G.N.to_string n = "head") l in
                           let other = List.filter (fun (n, a) -> G.N.to_string n <> "head") l in
                           match heads with
                           | [(h , a)] -> let i, s = aux s a in
                                          let is, ns, s = List.fold_left (fun (is, ns, s) (n, a) ->
                                                               let i', s = aux s a in
                                                               (i'::is, n::ns, s)) ([], [], s) other in
                                          (i, List.fold_right2 (add_edge i) ns is s)
                           | _         -> raise CannotParseToUD
    in let root, s = aux "digraph G {\nsplines=true;\n" a in
       (add_vertex None root s) ^ "root -> " ^ (lbl_of_int root) ^ "[label=root];\n}"                        

  let compare = compare
                                  
end

