(* A word set is encoded as a list of positions: [[1;2[[ [[3;4[[ [[7;9[[ => [9;7;4;3;2;1] *)
module WordSet = struct

  type t = int list

  exception Invalid
  exception Incompatible

  (* return true iff the argument encodes a valid word set *)
  let rec check = function
      []                             -> true
    | [y; x] when y > x              -> true
    | y::x::z::t when y > x && x > z -> check (z::t)
    | _                              -> false

  (* empty word set *)
  let empty = []

  (* create a word set from an interval [[x; y[[ *)
  let interval x y =
    if y > x then [y; x] else raise Invalid

  (* merge two word sets that are assumed to be valid *)
  let rec merge a b = match a, b with
      [], _  -> b
    | _ , [] -> a
    | y::x::t, y'::x'::t' when x > y' -> y::x::(merge t b)
    | y::x::t, y'::x'::t' when x = y' -> y::x'::(merge t t')
    | y::x::t, y'::x'::t' when y > x' -> y'::x'::(merge t' a)
    | y::x::t, y'::x'::t' when y = x' -> y'::x::(merge t t')
    | _ -> raise Incompatible
                 
  (* append an interval to a word set *)
  let append a b = match a, List.rev b with
      [], _  -> b
    | _ , [] -> a
    | x::t, y::u when y > x -> b@a
    | x::t, y::u when y = x -> (List.tl b)@t
    | _ , _  -> raise Incompatible                      
                      
end

module type Grammar = sig

  type term
  type nonterm
  type idlexpr' = E'
                | T' of term
                | N' of nonterm * int
                | C' of idlexpr' list
                | I' of idlexpr' list
                | D' of idlexpr' list
                                 
end

module Graph (G: Grammar) = struct

  type label = Term    of G.term
             | Nonterm of G.nonterm * int
             | Epsilon

  type graph = Split of graph list
             | Merge of graph * int * int
             | Node  of edge list
             | Final

  and edge = graph * label

  module Cut = struct
    type t = graph
    let compare = compare
  end

  module CutS = Set.Make(Cut)
  module CutM = Map.Make(Cut)

  type transition = { lbl: label; lst: graph option; cut: WordSet.t CutM.t }

  (* return the list of transitions available from a valid cut *)
  let next (cut: WordSet.t CutM.t): transition list =
    let process_edge part_cut s trans (g, l) =
      { lbl = l;
        lst = Some g;
        cut = CutM.add g s part_cut }::trans
    in
    let process_node g s (trans, merges) = match g with
        Split gs         -> ({ lbl = Epsilon;
                               lst = None;
                               cut = List.fold_right (fun g' -> CutM.add g' s) gs (CutM.remove g cut) }::trans,
                             merges)
      | Merge (g', j, i) -> (trans, CutM.update g'
                                      (function None ->
                                                 Some ([(g,s)], i) | Some (gs, i) -> Some ((g,s)::gs, i)) merges)
      | Node  es         -> (List.fold_left (process_edge (CutM.remove g cut) s) trans es, merges)
      | Final            -> (trans, merges)
    in
    let process_merge g (gs, i) trans =
      if List.length gs = i
      then let g_, s_ = List.split gs in
           try let merged = List.fold_left WordSet.merge WordSet.empty s_ in
               { lbl = Epsilon;
                 lst = None;
                 cut = CutM.add g merged (List.fold_right CutM.remove g_ cut) }::trans
           with WordSet.Incompatible -> trans
      else trans
    in
    let trans, merges = CutM.fold process_node cut ([], CutM.empty) in
    CutM.fold process_merge merges trans

  (* apply all possible transitions from a list, given a label and its WordSet *)
  let apply (ts: transition list) (l: label) (s: WordSet.t): WordSet.t CutM.t list =
    let ts = List.filter (fun t -> t.lbl = l) ts in
    let process_transition cuts t = match t.lst with
        None   -> t.cut::cuts
      | Some g -> try let s' = CutM.find g t.cut in
                      let cut = CutM.update g (fun _ -> Some (WordSet.append s' s)) t.cut in
                      cut::cuts
                  with WordSet.Incompatible -> cuts
    in List.fold_left process_transition [] ts

  (* generate graph from IDL expression *)
  let of_idlexpr expr =
    let rec aux succ = function
        G.E'       -> Final
      | G.T' t     -> Node ([succ, Term t])
      | G.N' (n,i) -> Node ([succ, Nonterm (n,i)])
      | G.C' ies'  -> List.fold_left aux succ ies'
      | G.I' ies'  -> let l = List.length ies' in
                      let gs = List.mapi (fun i e' -> aux (Merge (succ, i, l)) e') ies' in
                      Split gs
      | G.D' ies'  -> let gs = List.map (fun e' -> (aux (Node [succ, Epsilon]) e', Epsilon)) ies' in
                      Node gs
    in aux Final expr
    
end

module Context (G: Grammar) = struct

  module Nonterm = struct
    type t = G.nonterm
    let compare = compare
  end
                         
  module M = Map.Make(Nonterm)

  type t = WordSet.t option M.t M.t

  (* create initial context from incats *)
  let create incats =
    List.fold_right (fun (n, cat) m -> M.update cat (function None   -> Some (M.add n None M.empty)
                                                            | Some l -> Some (M.add n None l)) m)
      [] M.empty

  (* extract substring matching positions from WordSet *)
  (* string is encoded as a list of terms *)
  let substr ts ws =
    let rec aux o ws ts = match ws, ts with
        []     , _               -> []
      | x::y::t, h::u when y = o -> aux (o+1) t u
      | x::y::t, h::u when x = o -> h::(aux (o+1) (y::(x+1)::t) u)
      | x::y::t, h::u when o < x -> aux (o+1) ws u
      | _      , _               -> assert false
    in aux 0 (List.rev ws) ts
        
  (* return context-compatible nonterminals of given category that may cover a given WordSet in text *)
  let compatible cntxt s cat ws =
    match M.find_opt cat cntxt with
      None    -> []
    | Some nl -> List.map fst (M.bindings (M.filter (fun n wss
                                                     -> match wss with
                                                          None   -> true
                                                        | Some h -> substr s h = substr s ws) nl))

  (* update context adding nonterminal of given category on given WordSet *)
  let update cntxt cat n ws =
    M.update cat (function None    -> assert false
                         | Some nl -> Some (M.update n (function  None    -> Some ws
                                                                | Some ws -> Some ws) nl))
      cntxt

end

(*TEST*)

(* to test, first type
                    #directory "_build";;
                    #load "formal.cmo";;
   in the toplevel *)
                              
open Formal

module TestGraph = Graph(G)

open TestGraph

let f = Final
          
let g = Split ([Node ([(Merge (f, 0, 2), Term "a")]); Node ([(Merge (f, 1, 2), Term "b")])])

let cut = CutM.add g WordSet.empty CutM.empty

let transitions = next cut

let cuts_2 = apply transitions Epsilon WordSet.empty

let cut_2 = List.hd cuts_2

let cut_2_b = CutM.bindings cut_2
                     
let a = WordSet.interval 0 1

let transitions_2 = next cut_2

let cuts_3 = apply transitions_2 (Term "a") a

let cut_3 = List.hd cuts_3

let cut_3_b = CutM.bindings cut_3

let b = WordSet.interval 1 2

let transitions_3 = next cut_3

let cuts_4 = apply transitions_3 (Term "b") b

let cut_4 = List.hd cuts_4

let cut_4_b = CutM.bindings cut_4

let transitions_5 = next cut_4

let cuts_5 = apply transitions_5 Epsilon WordSet.empty

let cut_5 = List.hd cuts_5

let cut_5_b = CutM.bindings cut_5

let transitions_6 = next cut_5

let ie' = G.C' ([G.T' "a"; G.T' "b"])

let ie'_g = of_idlexpr ie'

