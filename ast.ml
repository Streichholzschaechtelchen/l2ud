module AST (G: ParsingTools.Grammar) = struct

  type t = Token of G.T.t  * G.N.t
         | Leaf of G.N.t
         | Node of G.N.t * ((G.N.t * t) list)
      
  let rec print_indent = function
      i when i <= 0 -> ()
    | i when i > 0  -> print_string " "; print_indent (i-1)
    | _             -> assert false
                                          
  let rec print ?(indent=0) =
    function
      Token (t, n) -> begin G.T.print t;
                            print_string " : ";
                            G.N.print n
                      end
    | Leaf n       -> G.N.print n
    | Node (n, []) -> G.N.print n
    | Node (n, l)  -> let indent = indent - (pos_out stdout) in
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
    let c = ref 0 in
    let next () =
      let d = !c in
      incr c;
      "l" ^ (string_of_int d) in
    let add_vertex_t t s =
      let l = next () in
      l, s ^ l ^ "[label=\"" ^ (G.T.to_string t) ^ "\", shape=box];\n" in
    let add_vertex_n n s =
      let l = next () in
      l, s ^ l ^ "[label=\"" ^ (G.N.to_string n) ^ "\"];\n" in
    let add_edge a b s =
      s ^ a ^ " -> " ^ b ^ ";\n" in
    let rec aux ?(nohead=false) head s = function
        Token (t, n) -> let ln, s = add_vertex_n n s in
                        let lt, s = add_vertex_t t s in
                        let s = if nohead then s else add_edge head ln s in
                        add_edge ln lt s
      | Leaf n       -> let ln, s = add_vertex_n n s in
                        (if nohead then s else add_edge head ln s)
      | Node (n, l)  -> let ln, s = add_vertex_n n s in
                        let s = if nohead then s else add_edge head ln s in
                        List.fold_left (fun s (_, a) -> aux ln s a) s l
    in (aux ~nohead:true "" "digraph G {\n" a) ^ "}"

  let compare = compare
                                  
end
