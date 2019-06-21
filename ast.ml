module AST (G: ParsingTools.Grammar) = struct

  type t = Leaf of G.T.t
         | Node of G.N.t * ((G.N.t * t) list)

  let rec print = function
      Leaf t         -> G.T.print t
    | Node (n, [])   -> G.N.print n
    | Node (n, h::t) -> print_string "(";
                        G.N.print n;
                        print_string " ";
                        let lbl, a = h in
                        G.N.print lbl;
                        print_string " = ";
                        print a;
                        List.iter (fun (lbl, a) -> print_string ", ";
                                                   G.N.print lbl;
                                                   print_string " = ";
                                                   print a) t;
                        print_string ")"

  let compare = compare
                                  
end
