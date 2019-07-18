module Parser (G: ParsingTools.Grammar) = struct

  (* 0. RULE MEMO *)

  let rules = ref ParsingTools.I.empty

  (* 1. DEFINITIONS *)
  
  module Graph = ParsingTools.Graph(G)
  module Context = ParsingTools.Context(G)
  module AST = Ast.AST(G)
  module SAST = Set.Make(AST)
  module E = ParsingEnv.Env(G)

  open E

  module CmpItem = struct
    type t = cmpItem
    let compare = compare
  end

  module SCmpItem = Set.Make(CmpItem)
         
  (* 2. ABSTRACT SYNTAX TREES *)

  let rec asts_of_contextEntry (en: env) (djavu: SCmpItem.t) (cmps: cmpstbl) (e: Context.E.t): SAST.t =
    let candidates = match e.wsm with
        None -> []
      | Some w -> List.filter (fun cmp -> cmp.wsm = w) (Hashtbl.find_all cmps e.cat) in
    List.fold_right (fun cmp as_ -> SAST.union (asts_of_cmpItem en djavu cmps cmp) as_)
      candidates SAST.empty

  and asts_of_cmpItem (en: env) (djavu: SCmpItem.t) (cmps: cmpstbl) (cmp: cmpItem): SAST.t =
    if SCmpItem.mem cmp djavu
    then SAST.empty
    else
      begin
        let rec cartesian (asm: SAST.t Context.M.t): AST.t Context.M.t list =
          match Context.M.min_binding_opt asm with
            None          -> [Context.M.empty]
          | Some (n, as_) -> let asm = Context.M.remove n asm in
                             let acc = cartesian asm in
                             SAST.fold (fun a bdd ->
                                 List.fold_left (fun cee am ->
                                     (Context.M.add n a am)::cee)
                                   bdd acc )
                               as_ [] in
        let cntxt  = Context.M.filter (fun n (e: Context.E.t) ->
                         match e.wsm with None   -> false
                                        | Some w -> true) cmp.cntxt in
        let asm    = let djavu = SCmpItem.add cmp djavu in
                     Context.M.map (function e -> asts_of_contextEntry en djavu cmps e) cntxt in
        let label  = ParsingTools.I.find cmp.rulid !rules in
        let onetok = match ParsingTools.I.cardinal cmp.wsm with
            1 -> let ws = snd (ParsingTools.I.min_binding cmp.wsm) in
                 begin match Context.substr en.ts ws with
                   [w] -> Some (w, List.hd (List.tl ws))
                 | _   -> None
                 end
          | _ -> None in
        match cartesian asm with
          []                           -> assert false
        | [a] when a = Context.M.empty -> begin match onetok with
                                            None        -> SAST.singleton (AST.Leaf label)
                                          | Some (w, i) -> SAST.singleton (AST.Token (w, i, label))
                                          end
        | ams                          -> List.fold_left (fun as_ am ->
                                              SAST.add (AST.Node (label, Context.M.bindings am)) as_)
                                            SAST.empty ams
      end
                                             
  let all_asts (en: env): SAST.t =
    let ws_tot = ParsingTools.WordSet.total en.ts in
    let cmps   = Hashtbl.find_all en.cmps en.gram.start in
    let cmps   = List.filter (fun cmp -> match ParsingTools.I.find_opt 0 cmp.wsm with
                                           None    -> false
                                         | Some w  -> w = ws_tot) cmps in
    List.fold_right (fun cmp -> SAST.union (asts_of_cmpItem en SCmpItem.empty en.cmps cmp)) cmps SAST.empty
      
  (* 4. PARSING ALGORITHM *)

  let clean_env e =
    Hashtbl.filter_map_inplace (fun lbl dtrans -> match dtrans.trans.lst with
                                                    None   -> None
                                                  | Some x -> Some dtrans)
      e.acts;
    e

  let rec parse gram ts =
    let e = predict_all (empty_env gram ts) in
    let tsi = List.mapi (fun i w -> (i,w)) ts in
    let ws' = ParsingTools.WordSet.total ts in
    let e = List.fold_left (fun e (i,w) ->
                let e = fold_actItem (Graph.Term w) (try_scan w i) e e in
                (*let e = clean_env e in*)
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
    (if !Flags.png_to_draw <> ""
     then (if SAST.cardinal asts = 1 then
            let oc = open_out "compatmp.dot" in
            let dot = if !Flags.draw_ud
                      then AST.to_dot_dep (SAST.choose asts)
                      else AST.to_dot (SAST.choose asts) in
            (output_string oc dot;
            close_out oc;
            ignore (Sys.command ("dot " ^ (if !Flags.draw_ud then "-Kfdp -n " else "")
                                 ^ "-Tpng compatmp.dot -o " ^ !Flags.png_to_draw));
            Sys.remove "compatmp.dot")
           else print_string "Syntax tree could not be drawn (not unique)!\n"));
    Hashtbl.filter_map_inplace (fun cat cmp -> if cat = gram.start
                                               then
                                                 (match ParsingTools.I.find_opt 0 cmp.wsm with
                                                    Some ws when ws = ws' -> Some cmp
                                                  | _                     -> None)
                                               else None) e.cmps;
    Hashtbl.length e.cmps > 0

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
              let cut = Graph.init expr in
              let it = { rulid = new_rulid cat; cut;
                         cntxt = Context.create a_r.incats;
                         cat; i; expr;
                         trnsl = Graph.next cut
                       } in
              (*print_int it.rulid; print_newline ();*)
              add_actItem e it;
              try_rec it e) e exprsi
        end
      | ERule e_r -> assert false
    in
    let process_rules' n =
      List.fold_right (process_rule' n) 
    in G.M.fold process_rules' e.gram.rules e
                
  and try_scan a j (e: env) (dtrans: dtransition) : env =
    let sing  = ParsingTools.WordSet.singleton j in
    let trans = dtrans.act.trnsl in
    let cuts  = Graph.apply trans (Graph.Term a) sing in
    let process_cut e (b, cut) =
      let trnsl = Graph.next cut in
      let it = { dtrans.act with cut; trnsl } in
      add_actItem e it;
      try_rec it e
    in List.fold_left process_cut e cuts

  and try_save (act: actItem) (e: env) =
    match Graph.CutM.cardinal act.cut with
      1 -> begin match Graph.CutM.bindings act.cut with
             [Final, wss] -> begin let it = { rulid = act.rulid;
                                              cat   = act.cat;
                                              i     = act.i;
                                              ws    = fst (ParsingTools.WordSetStack.pop wss);
                                              cntxt = act.cntxt } in
                                   let e = try_unify it e in
                                   let ju = { rulid = act.rulid;
                                              cat   = act.cat;
                                              wsm   = ParsingTools.I.singleton act.i it.ws;
                                              cntxt = act.cntxt } in
                                   if not (mem_cmpItem e ju)
                                   then (add_cmpItem e ju; try_combine_RHS ju e)
                                   else e
                             end
           | [_]         -> e
           | _           -> assert false
           end
    | _ -> e

  and try_step (act: actItem) (e: env) =
    let trans = Graph.next act.cut in
    let cuts = Graph.apply trans Graph.Epsilon ParsingTools.WordSet.empty in
    let process_cut (b, cut) e =
      let trnsl = Graph.next cut in
      let it = { act with cut; trnsl } in
      add_actItem e it;
      try_rec it e in
    List.fold_right process_cut cuts e

  and try_combine (dtrans: dtransition) (cmp: cmpItem) (e: env) =
    let process_cut (b, cut) n i e cmp =
      let cntxt = Context.reserve dtrans.act.cntxt n i cmp.wsm in
      let trnsl = Graph.next cut in
      let it = { dtrans.act with cut; cntxt; trnsl } in
      add_actItem e it;
      try_rec it e in
    let n, i = match dtrans.trans.lbl with
        Graph.Nonterm (n, i) -> n, i
      | _                    -> assert false in
    match Graph.apply_one dtrans.trans (ParsingTools.I.find i cmp.wsm) with
      Some cut -> begin match Context.compat dtrans.act.cntxt e.ts n i cmp.wsm with
                    true  -> process_cut cut n i e cmp
                  | false -> e
                  end
    | None     -> e

  and try_combine_LHS (act: actItem) (e: env) =
    if (Hashtbl.length e.cmps > 0)
    then (
      let trnsl = act.trnsl in
      List.fold_left (fun e (trans: Graph.transition) ->
          match trans.lbl with
            Graph.Nonterm (n, i) ->
             let cat = (Context.M.find n act.cntxt).cat in
             let candidates = Hashtbl.find_all e.cmps cat in
             let candidates = List.filter (fun cmp ->
                                  begin match ParsingTools.I.find_opt i cmp.wsm with
                                    Some _ -> true
                                  | _      -> false
                                  end) candidates in
             List.fold_right (try_combine { trans; act } ) candidates e
          | _ -> e) e trnsl
    ) else e
                   
  and try_combine_RHS (cmp: cmpItem) (e: env) =
    let indices = fst (List.split (ParsingTools.I.bindings cmp.wsm)) in
    List.fold_left (fun e index ->
        let trnsl = Hashtbl.find_all e.acts (Graph.Nonterm (cmp.cat, index)) in
        print_int (List.length trnsl); print_newline ();
        List.fold_right (fun trans -> try_combine trans cmp) trnsl e)
                   e indices

  and try_unify (pas: pasItem) (e: env) =
    let process_item cmp e =
      match ParsingTools.I.find_opt pas.i cmp.wsm with
        None -> begin match Context.unify pas.cntxt cmp.cntxt with
                  None       -> e
                | Some cntxt -> let wsm = ParsingTools.I.add pas.i pas.ws cmp.wsm in
                                let it = { cmp with wsm; cntxt } in
                                add_cmpItem e it;
                                try_combine_RHS it e
                end
      | _    -> e
    in Hashtbl.fold (fun _ cmp e -> if cmp.rulid = pas.rulid
                                    then process_item cmp e
                                    else e) e.cmps e

  and try_rec act e =
    e
    |> try_save act
    |> try_step act
    |> try_combine_LHS act

end

module P = Parser(Formal.G)
