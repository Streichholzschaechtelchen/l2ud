module Parser (G: ParsingTools.Grammar) = struct

  module Graph = ParsingTools.Graph(G)
  module Context = ParsingTools.Context(G)
                                       
  type actItem = { n    : G.nonterm;
                   i    : int;
                   expr : G.idlexpr';
                   cut  : Graph.cut;
                   cntxt: Context.t;
                   lockd: bool }

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

  type env = { gram: G.grammar';
               str : G.term list;
               acts: SActItem.t;
               pass: SPassItem.t }

  let empty_env gram str = { gram; str;
                             acts = SActItem.empty;
                             pass = SPassItem.empty }

  let rec parse gram str =
    let e = predict_all (empty_env gram str) in
    let stri = List.mapi (fun i w -> (i,w)) str in
    List.fold_left (fun e (i,w) ->
        SActItem.fold (try_scan w i) e.acts e) e stri

  and predict_all e =
    let process_rule' n (r: G.rule') e =
      let exprsi = Array.mapi (fun i x -> (i,x)) r.exprs' in
      Array.fold_left (fun e (i,expr') ->
          let expr, lockd = expr' in
          let it = { n; i; expr; lockd;
                     cut = Graph.init expr;
                     cntxt = Context.create r.incats';
                   } in
          let acts = SActItem.add it e.acts in
          try_rec it { e with acts })
        e exprsi in
    let process_rules' n =
      List.fold_right (process_rule' n) 
    in G.M.fold process_rules' e.gram.rules' e
                
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
             [Final, ws] -> begin if not act.lockd || ParsingTools.WordSet.locked ws
                                  then let it = { n  = act.n;
                                                  i  = act.i;
                                                  ws } in
                                       let pass = SPassItem.add it e.pass in
                                       try_combine_RHS it { e with pass }
                                  else e
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
      let ns = Context.compatible act.cntxt e.str pas.n pas.i pas.ws in
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
      let ns = Context.compatible act.cntxt e.str pas.n pas.i pas.ws in
      List.fold_right (process_nonterm act) ns e
    in SActItem.fold process_item e.acts e

  and try_rec act e =
    e
    |> try_save act
    |> try_step act
    |> try_combine_LHS act

end
