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

  let compare = compare
                                  
end
