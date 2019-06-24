module Parser (G: ParsingTools.Grammar) = struct

  (* 0. RULE MEMO *)

  let rules = ref ParsingTools.I.empty

  (* 1. DEFINITIONS *)
  
  module Graph = ParsingTools.Graph(G)
  module Context = ParsingTools.Context(G)
  module AST = Ast.AST(G)

  module SAST = Set.Make(AST)

  module ContextT = struct
    type t = Context.t
    let compare = compare
  end

  module SContext = Set.Make(ContextT)
                                       
  type actItem = { rulid: int;
                   cat  : G.nonterm;
                   i    : int;
                   expr : G.idlexpr;
                   cut  : Graph.cut;
                   cntxt: Context.t }

  type pasItem = { rulid: int;
                   cat  : G.nonterm;
                   i    : int;
                   ws   : ParsingTools.WordSet.t;
                   cntxt: Context.t }

  type cmpItem = { rulid: int;
                   cat  : G.nonterm;
                   wsm  : ParsingTools.WordSet.t ParsingTools.I.t;
                   cntxt: Context.t }
                   

  module ActItem = struct
    type t = actItem
    let compare = compare
  end

  module PasItem = struct
    type t = pasItem
    let compare = compare
  end

  module CmpItem = struct
    type t = cmpItem
    let compare = compare
  end

  module SActItem = Set.Make(ActItem)
  module SPasItem = Set.Make(PasItem)
  module SCmpItem = Set.Make(CmpItem)

  type env = { gram: G.grammar;
               ts  : G.term list;
               acts: SActItem.t;
               pass: SPasItem.t;
               cmps: SCmpItem.t }

  let empty_env gram ts = { gram; ts;
                            acts = SActItem.empty;
                            pass = SPasItem.empty;
                            cmps = SCmpItem.empty}

  (* 2. PRINTING FUNCTIONS *)

  let print_actItem (it: actItem) =
    print_string "<active item> = {\n";
    print_string "rule id = ";
    print_int it.rulid;
    print_string "\ncat = ";
    G.N.print it.cat;
    print_string "\ni = ";
    print_int it.i;
    print_string "\nIDL expression = ";
    G.print_idlexpr it.expr;
    print_string "\ncut = ";
    Graph.print_cut it.cut;
    print_string "\ncontext = ";
    Context.print it.cntxt;
    print_string "\n}"

  let print_pasItem (it: pasItem) =
    print_string "<passive item> = {\n";
    print_string "rule id = ";
    print_int it.rulid;
    print_string "\ncat = ";
    G.N.print it.cat;
    print_string "\ni = ";
    print_int it.i;
    print_string "\nword set = ";
    ParsingTools.WordSet.print it.ws;
    print_string "\ncontext = ";
    Context.print it.cntxt;
    print_string "\n}"

  let print_cmpItem (it: cmpItem) =
    print_string "<complete item> = {\n";
    print_string "rule id = ";
    print_int it.rulid;
    print_string "\ncat = ";
    G.N.print it.cat;
    print_string "\nword sets = {\n";
    ParsingTools.I.iter (fun i ws -> print_string "#";
                                     print_int i;
                                     print_string " ";
                                     ParsingTools.WordSet.print ws;
                                     print_newline ()) it.wsm;
    print_string "}\ncontext = ";
    Context.print it.cntxt;
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
    SPasItem.iter (fun it -> print_pasItem it; print_string "\n\n") e.pass;
    print_string "***COMPLETE ITEMS***\n\n";
    SCmpItem.iter (fun it -> print_cmpItem it; print_string "\n\n") e.cmps;
    print_string "</ENVIRONMENT>\n\n"

  let print_env_stats (e: env) =
    print_string "Rules in grammar: ";
    print_int (G.M.cardinal e.gram.rules);
    print_string "\nLength of token list: ";
    print_int (List.length e.ts);
    print_string "\nActive items: ";
    print_int (SActItem.cardinal e.acts);
    print_string "\nPassive items: ";
    print_int (SPasItem.cardinal e.pass);
    print_string "\nComplete items: ";
    print_int (SCmpItem.cardinal e.cmps);
    print_newline ()

  (* 3. ABSTRACT SYNTAX TREES *)

  let rec asts_of_contextEntry (en: env) (cmps: SCmpItem.t) (e: Context.E.t): SAST.t =
    let matches_contextEntry cmp =
      (cmp.cat = e.cat) &&
        (match e.wsm with None   -> false
                        | Some w -> cmp.wsm = w) in
    SCmpItem.fold (fun cmp as_ -> SAST.union (asts_of_cmpItem en cmps cmp) as_)
      (SCmpItem.filter matches_contextEntry cmps)
      SAST.empty

  and asts_of_cmpItem (en: env) (cmps: SCmpItem.t) (cmp: cmpItem): SAST.t =
    let rec cartesian (asm: SAST.t Context.M.t): AST.t Context.M.t list =
      match Context.M.min_binding_opt asm with
        None          -> [Context.M.empty]
      | Some (n, as_) -> let asm = Context.M.remove n asm in
                         let acc = cartesian asm in
                         SAST.fold (fun a acc ->
                             List.fold_left (fun bdd am ->
                                 (Context.M.add n a am)::bdd) [] acc)
                           as_ acc in
    let cntxt = Context.M.filter (fun n (e: Context.E.t) ->
                    match e.wsm with None   -> false
                                   | Some w -> true) cmp.cntxt in
    let asm    = Context.M.map (function e -> asts_of_contextEntry en cmps e) cntxt in
    let label  = ParsingTools.I.find cmp.rulid !rules in
    let onetok = match ParsingTools.I.cardinal cmp.wsm with
        1 -> begin match Context.substr en.ts (snd (ParsingTools.I.min_binding cmp.wsm)) with
               [w] -> Some w
             | _   -> None
             end
      | _ -> None in
    match cartesian asm with
      []                           -> assert false
    | [a] when a = Context.M.empty -> begin match onetok with
                                        None   -> SAST.singleton (AST.Leaf label)
                                      | Some w -> SAST.singleton (AST.Token (w, label))
                                      end
    | ams                          -> List.fold_left (fun as_ am ->
                                          SAST.add (AST.Node (label, Context.M.bindings am)) as_)
                                        SAST.empty ams
           
  let all_asts (en: env): SAST.t =
    let ws_tot = ParsingTools.WordSet.total en.ts in
    let cmps   = SCmpItem.filter (fun cmp -> cmp.cat = en.gram.start
                                             && (match ParsingTools.I.find_opt 0 cmp.wsm with
                                                   None    -> false
                                                 | Some w  -> w = ws_tot)) en.cmps in
    SCmpItem.fold (fun cmp -> SAST.union (asts_of_cmpItem en en.cmps cmp)) cmps SAST.empty
      
  (* 4. PARSING ALGORITHM *)
    
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
    let asts = all_asts e in
    if !Flags.parse_trees then
            SAST.iter (fun a -> AST.print a; print_newline()) asts;
    (if !Flags.png_to_draw <> "" && SAST.cardinal asts = 1 then
            let oc = open_out "compatmp.dot" in
            let dot = AST.to_dot (SAST.choose asts) in
            output_string oc dot;
            close_out oc;
            ignore (Sys.command ("dot -Tpng compatmp.dot -o " ^ !Flags.png_to_draw));
            Sys.remove "compatmp.dot");
    SCmpItem.exists (fun cmp -> cmp.cat = gram.start && (match ParsingTools.I.find_opt 0 cmp.wsm with
                                                           None -> false
                                                         | Some ws -> ws = ws')) e.cmps

  and predict_all (e: env) =
    let i = ref 0 in
    let new_rulid cat =
      let j = !i in
      i := !i + 1;
      rules := ParsingTools.I.add j cat !rules;
      j in
    let process_rule' cat (r: G.rule) e =
      match r with
        ARule a_r -> begin
          let exprsi = Array.mapi (fun i x -> (i,x)) a_r.exprs in
          Array.fold_left (fun e (i,expr) ->
              let it = { rulid = new_rulid cat;
                         cut   = Graph.init expr;
                         cntxt = Context.create a_r.incats;
                         cat; i; expr;
                       } in
              let acts = SActItem.add it e.acts in
              try_rec it { e with acts }) e exprsi
        end
      | ERule e_r -> assert false
    in
    let process_rules' n =
      List.fold_right (process_rule' n) 
    in G.M.fold process_rules' e.gram.rules e
                
  and try_scan a j (act: actItem) (e: env) =
    let sing  = ParsingTools.WordSet.singleton j in
    let trans = Graph.next act.cut in
    let cuts  = Graph.apply trans (Graph.Term a) sing in
    let process_cut cut e =
      let it = { act with cut } in
      let acts = SActItem.add it e.acts in
      try_rec it { e with acts }
    in List.fold_right process_cut cuts e

  and try_save (act: actItem) (e: env) =
    match Graph.CutM.cardinal act.cut with
      1 -> begin match Graph.CutM.bindings act.cut with
             [Final, wss] -> begin let it = { rulid = act.rulid;
                                              cat   = act.cat;
                                              i     = act.i;
                                              ws    = fst (ParsingTools.WordSetStack.pop wss);
                                              cntxt = act.cntxt } in
                                   let pass = SPasItem.add it e.pass in
                                   let e = try_unify it { e with pass } in
                                   let ju = { rulid = act.rulid;
                                              cat   = act.cat;
                                              wsm   = ParsingTools.I.singleton act.i it.ws;
                                              cntxt = act.cntxt } in
                                   let cmps = SCmpItem.add ju e.cmps in
                                   try_combine_RHS ju { e with cmps }
                             end
           | [_]         -> e
           | _           -> assert false
           end
    | _ -> e

  and try_step (act: actItem) (e: env) =
    let trans = Graph.next act.cut in
    let cuts = Graph.apply trans Graph.Epsilon ParsingTools.WordSet.empty in
    let process_cut cut e =
      let it = { act with cut } in
      let acts = SActItem.add it e.acts in
      try_rec it { e with acts }
    in List.fold_right process_cut cuts e

  and try_combine (act: actItem) (cmp: cmpItem) (e: env) =
    let process_cut cut n i e cmp =
      let cntxt = Context.reserve act.cntxt n i cmp.wsm in
      let it = { act with cut; cntxt } in
      let acts = SActItem.add it e.acts in
      try_rec it { e with acts } in
    let process_tran cmp e (tran: Graph.transition) =
      let n, i = match tran.lbl with
          Graph.Nonterm (n, i) -> n, i
        | _                    -> assert false in
      match Graph.apply_one tran (ParsingTools.I.find i cmp.wsm) with
        Some cut -> begin match Context.compat act.cntxt e.ts n i cmp.wsm with
                      true  -> process_cut cut n i e cmp
                    | false -> e
                    end
      | None     -> e in
    let trans = List.filter (fun (tran: Graph.transition) ->
                    match tran.lbl with
                      Graph.Nonterm (n, i) ->
                       let cat = (Context.M.find n act.cntxt).cat in
                       (cat = cmp.cat) &&
                         begin match ParsingTools.I.find_opt i cmp.wsm with
                           Some _ -> true
                         | _      -> false
                         end
                    | _ -> false) (Graph.next act.cut) in
    List.fold_left (process_tran cmp) e trans

  and try_combine_LHS (act: actItem) (e: env) =
    SCmpItem.fold (try_combine act) e.cmps e

  and try_combine_RHS (cmp: cmpItem) (e: env) =
    SActItem.fold (fun act -> try_combine act cmp) e.acts e

  and try_unify (pas: pasItem) (e: env) =
    let process_item cmp e =
      match ParsingTools.I.find_opt pas.i cmp.wsm with
        None -> begin match Context.unify pas.cntxt cmp.cntxt with
                  None       -> e
                | Some cntxt -> let wsm = ParsingTools.I.add pas.i pas.ws cmp.wsm in
                                let it = { cmp with wsm; cntxt } in
                                let cmps = SCmpItem.add it e.cmps in
                                try_combine_RHS it { e with cmps }
                end
      | _    -> e
    in SCmpItem.fold process_item (SCmpItem.filter (fun it -> it.rulid = pas.rulid) e.cmps) e

  and try_rec act e =
    e
    |> try_save act
    |> try_step act
    |> try_combine_LHS act

end

module P = Parser(Formal.G)
