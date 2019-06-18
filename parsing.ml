module Parser (G: ParsingTools.Grammar) = struct

  (* 1. DEFINITIONS *)
  
  module Graph = ParsingTools.Graph(G)
  module Context = ParsingTools.Context(G)
                                       
  type actItem = { n    : G.nonterm;
                   i    : int;
                   expr : G.idlexpr;
                   cut  : Graph.cut;
                   cntxt: Context.t }

  type passItem = { n : G.nonterm;
                    i : int;
                    ws: ParsingTools.WordSet.t }

  module ActItem = struct
    type t = actItem
    let compare = compare
  end

  module PassItem = struct
    type t = passItem
    let compare = compare
  end

  module SActItem = Set.Make(ActItem)

  module SPassItem = Set.Make(PassItem)

  type env = { gram: G.grammar;
               ts  : G.term list;
               acts: SActItem.t;
               pass: SPassItem.t }

  let empty_env gram ts = { gram; ts;
                            acts = SActItem.empty;
                            pass = SPassItem.empty }


  (* 2. PRINTING FUNCTIONS *)

  let print_actItem (it: actItem) =
    print_string "<active item> = {\n";
    print_string "n = ";
    G.N.print it.n;
    print_string "\ni = ";
    print_int it.i;
    print_string "\nexpr = ";
    G.print_idlexpr it.expr;
    print_string "\ncut = ";
    Graph.print_cut it.cut;
    print_string "\ncntxt = ";
    Context.print it.cntxt;
    print_string "\n}"

  let print_passItem (it: passItem) =
    print_string "<passive item> = {\n";
    print_string "n = ";
    G.N.print it.n;
    print_string "\ni = ";
    print_int it.i;
    print_string "\nws = ";
    ParsingTools.WordSet.print it.ws;
    print_string "\n}"

  let print_env (e: env) =
    print_string "<ENVIRONMENT>\n\n";
    print_string "***GRAMMAR***\n\n";
    G.print_grammar e.gram;
    print_string "\n\n***STRING TO PARSE***\n\n";
    List.iter (fun s -> G.T.print s; print_string " ") e.ts;
    print_string "\n\n***ACTIVE ITEMS***\n\n";
    SActItem.iter (fun it -> print_actItem it; print_string "\n\n") e.acts;
    print_string "***PASSIVE ITEMS***\n\n";
    SPassItem.iter (fun it -> print_passItem it; print_string "\n\n") e.pass;
    print_string "</ENVIRONMENT>\n\n"

  let print_env_stats (e: env) =
    print_string "Rules in grammar: ";
    print_int (G.M.cardinal e.gram.rules);
    print_string "\nLength of token list: ";
    print_int (List.length e.ts);
    print_string "\nActive items: ";
    print_int (SActItem.cardinal e.acts);
    print_string "\nPassive items: ";
    print_int (SPassItem.cardinal e.pass);
    print_newline ()
                            
  (* 3. PARSING ALGORITHM *)
    
  let rec parse gram ts =
    let e = predict_all (empty_env gram ts) in
    let tsi = List.mapi (fun i w -> (i,w)) ts in
    let ws' = ParsingTools.WordSet.total ts in
    let e = List.fold_left (fun e (i,w) ->
                let e = SActItem.fold (try_scan w i) e.acts e in
                if !Flags.verbose then print_env e;
                if !Flags.statistics then (print_string "Iter ";
                                           print_int (i+1);
                                           print_newline ();
                                           print_env_stats e;
                                           print_newline ());
                e) e tsi in
    SPassItem.exists (fun pas -> pas.n = gram.start && pas.ws = ws') e.pass

  and predict_all e =
    let process_rule' n (r: G.rule) e =
      match r with
        ARule a_r -> begin
          let exprsi = Array.mapi (fun i x -> (i,x)) a_r.exprs in
          Array.fold_left (fun e (i,expr) ->
              let it = { n; i; expr;
                         cut = Graph.init expr;
                         cntxt = Context.create a_r.incats;
                       } in
              let acts = SActItem.add it e.acts in
              try_rec it { e with acts }) e exprsi
        end
      | ERule e_r -> assert false
    in
    let process_rules' n =
      List.fold_right (process_rule' n) 
    in G.M.fold process_rules' e.gram.rules e
                
  and try_scan a j act e =
    let sing  = ParsingTools.WordSet.singleton j in
    let trans = Graph.next act.cut in
    let cuts  = Graph.apply trans (Graph.Term a) sing in
    let process_cut cut e =
      let it = { act with cut } in
      let acts = SActItem.add it e.acts in
      try_rec it { e with acts }
    in List.fold_right process_cut cuts e

  and try_save act e =
    match Graph.CutM.cardinal act.cut with
      1 -> begin match Graph.CutM.bindings act.cut with
             [Final, wss] -> begin let it = { n  = act.n;
                                              i  = act.i;
                                              ws = fst (ParsingTools.WordSetStack.pop wss) } in
                                   let pass = SPassItem.add it e.pass in
                                   try_combine_RHS it { e with pass }
                             end
           | [_]         -> e
           | _           -> assert false
           end
    | _ -> e

  and try_step act e =
    let trans = Graph.next act.cut in
    let cuts = Graph.apply trans Graph.Epsilon ParsingTools.WordSet.empty in
    let process_cut cut e =
      let it = { act with cut } in
      let acts = SActItem.add it e.acts in
      try_rec it { e with acts }
    in List.fold_right process_cut cuts e

  and try_combine_LHS act e =
    let trans = Graph.next act.cut in
    let process_cut pas n cut e =
      let cntxt = Context.update act.cntxt pas.n pas.i n pas.ws in
      let pas = { act with cut; cntxt } in
      let acts = SActItem.add pas e.acts in
      try_rec pas { e with acts } in
    let process_nonterm pas n e =
      let cuts = Graph.apply trans (Graph.Nonterm (n, pas.i)) pas.ws in
      List.fold_right (process_cut pas n) cuts e in
    let process_item pas e =
      let ns = Context.compatible act.cntxt e.ts pas.n pas.i pas.ws in
      List.fold_right (process_nonterm pas) ns e
    in SPassItem.fold process_item e.pass e

  and try_combine_RHS pas e =
    let process_cut act n cut e =
      let cntxt = Context.update act.cntxt pas.n pas.i n pas.ws in
      let pas = { act with cut; cntxt } in
      let acts = SActItem.add pas e.acts in
      try_rec pas { e with acts } in
    let process_nonterm act n e =
      let trans = Graph.next act.cut in
      let cuts = Graph.apply trans (Graph.Nonterm (n, pas.i)) pas.ws in
      List.fold_right (process_cut act n) cuts e in
    let process_item act e =
      let ns = Context.compatible act.cntxt e.ts pas.n pas.i pas.ws in
      List.fold_right (process_nonterm act) ns e
    in SActItem.fold process_item e.acts e

  and try_rec act e =
    e
    |> try_save act
    |> try_step act
    |> try_combine_LHS act

end

module P = Parser(Formal.G)
