module Env (G: ParsingTools.Grammar) = struct

  (* 1. DEFINITIONS *)
  
  module Graph = ParsingTools.Graph(G)
  module Context = ParsingTools.Context(G)

  module ContextT = struct
    type t = Context.t
    let compare = compare
  end

  module SContext = Set.Make(ContextT)

  type label      = Graph.label
  type transition = Graph.transition
                                       
  type actItem = { rulid: int;
                   cat  : G.nonterm;
                   i    : int;
                   expr : G.idlexpr;
                   cut  : Graph.cut;
                   cntxt: Context.t;
                   trnsl: transition list }

  type pasItem = { rulid: int;
                   cat  : G.nonterm;
                   i    : int;
                   ws   : ParsingTools.WordSet.t;
                   cntxt: Context.t }

  type cmpItem = { rulid: int;
                   cat  : G.nonterm;
                   wsm  : ParsingTools.WordSet.t ParsingTools.I.t;
                   cntxt: Context.t }

  type dtransition = { trans: transition;
                       act  : actItem }

  type actstbl = (label, dtransition) Hashtbl.t
  type cmpstbl = (G.nonterm, cmpItem) Hashtbl.t
                         
  type env = { gram: G.grammar;
               ts  : G.term list;
               acts: actstbl;
               cmps: cmpstbl }

  (* 2. CONSTRUCTORS / MODIFIERS *)
               
  let empty_env gram ts = { gram; ts;
                            acts = Hashtbl.create 17;
                            cmps = Hashtbl.create 17 }

  let add_actItem e (act: actItem) =
    List.iter (fun (trans: Graph.transition) ->
        let lbl = trans.lbl in
        let lbl' = match lbl with
            Term t         -> Graph.Term t
          | Nonterm (n, i) -> let cat = (Context.M.find n act.cntxt).cat in
                              Graph.Nonterm (cat, i)
          | Epsilon        -> Graph.Epsilon in
        let dtrans = { trans; act } in
        Hashtbl.add e.acts lbl' dtrans)
      act.trnsl

  (*let remove_actItem e (act: actItem) =
    List.iter (fun (trans: Graph.transition) ->
        let lbl = trans.lbl in
        Hashtbl.filter_map_inplace (fun lbl' dtrans' ->
            if lbl' = lbl && dtrans'.trans = trans
            then None
            else Some dtrans') e.acts)
      act.trnsl*)

  let add_cmpItem e cmp =
    Hashtbl.add e.cmps cmp.cat cmp

  let mem_cmpItem e cmp =
    let candidates = Hashtbl.find_all e.cmps cmp.cat in
    List.mem cmp candidates

  let fold_actItem (lbl: label) (f: env -> dtransition -> env) (e: env) (x0: 'a) : 'a =
    let items = Hashtbl.find_all e.acts lbl in
    List.fold_left f x0 items

  (* 3. PRINTING FUNCTIONS *)

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
    Hashtbl.iter (fun _ it -> print_actItem it.act; print_string "\n\n") e.acts;
    print_string "***COMPLETE ITEMS***\n\n";
    Hashtbl.iter (fun _ it -> print_cmpItem it; print_string "\n\n") e.cmps;
    print_string "</ENVIRONMENT>\n\n"

  let print_env_stats (e: env) =
    print_string "Rules in grammar: ";
    print_int (G.M.cardinal e.gram.rules);
    print_string "\nLength of token list: ";
    print_int (List.length e.ts);
    print_string "\nActive transitions: ";
    print_int (Hashtbl.length e.acts);
    print_string "\nComplete items: ";
    print_int (Hashtbl.length e.cmps);
    print_newline ()


end
