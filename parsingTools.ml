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

  (* create a word set from a singleton {x} *)
  let singleton x =
    interval x (x + 1)

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

  (* return true iff the word set is locked, ie represents an interval *)
  let locked = function
      []     -> true
    | [y; x] -> true
    | _      -> false
                      
end

module type Grammar = sig

  module T: Formal.Symbol
  module N: Formal.Symbol
              
  module M : sig
    (* Dirty copying of Map.S with overriden key *)
    type key = N.t
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
                     
  type term = T.t
  type nonterm = N.t
  type idlexpr' = E'
                | T' of term
                | N' of nonterm * int
                | C' of idlexpr' list
                | I' of idlexpr' list
                | D' of idlexpr' list
  type rule' = { incats': (nonterm * nonterm) list;
                 exprs': (idlexpr' * bool) array }
  type grammar' = { rules': (rule' list) M.t;
                    start': nonterm }
                                 
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

  type cut = WordSet.t CutM.t
                       
  type transition = { lbl: label; lst: graph option; cut: cut }

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
  let apply (ts: transition list) (l: label) (s: WordSet.t): cut list =
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

  (* generate initial cut from IDL expression *)
  let init expr =
    let g = of_idlexpr expr in
     CutM.add g WordSet.empty CutM.empty
    
end

module Context (G: Grammar) = struct

  module Nonterm = struct
    type t = G.nonterm
    let compare = compare
  end
                     
  module Int = struct
    type t = int
    let compare = compare
  end
                         
  module M = Map.Make(Nonterm)
  module I = Map.Make(Int)

  type nonterm = G.nonterm
  type term    = G.term

  type t = WordSet.t I.t option M.t M.t

  (* create initial context from incats *)
  let create (incats: (nonterm * nonterm) list): t =
    List.fold_right (fun (n, cat) m -> M.update cat (function None   -> Some (M.add n None M.empty)
                                                            | Some l -> Some (M.add n None l)) m)
      incats M.empty

  (* extract substring matching positions from WordSet *)
  (* string is encoded as a list of terms *)
  let substr (ts: term list) (ws: WordSet.t) : term list =
    let rec aux o ws ts = match ws, ts with
        []     , _               -> []
      | x::y::t, h::u when y = o -> aux (o+1) t u
      | x::y::t, h::u when x = o -> h::(aux (o+1) (y::(x+1)::t) u)
      | x::y::t, h::u when o < x -> aux (o+1) ws u
      | _      , _               -> assert false
    in aux 0 (List.rev ws) ts
        
  (* return context-compatible nonterminals of given category that may cover a given WordSet in text *)
  let compatible (cntxt: t) (ts: term list) (cat: nonterm) (i: int) (ws: WordSet.t): nonterm list =
    match M.find_opt cat cntxt with
      None    -> []
    | Some nl -> List.map fst (M.bindings (M.filter (fun n wss
                                                     -> match wss with
                                                          None   -> true
                                                        | Some h ->
                                                           match I.find_opt i h with
                                                             None -> true
                                                           | Some xt ->
                                                              substr ts xt = substr ts ws) nl))

  (* update context adding nonterminal of given category on given WordSet *)
  let update (cntxt: t) (cat: nonterm) (i: int) (n: nonterm) (ws: WordSet.t): t =
    M.update cat (function None    -> assert false
                         | Some nl -> Some (M.update n (function  None
                                                                  -> assert false
                                                                | Some None
                                                                  -> Some (Some (I.add i ws I.empty))
                                                                | Some (Some m)
                                                                  -> Some (Some (I.update i
                                                                                   (function None -> Some ws
                                                                                           | Some ws -> Some ws)
                                                                              m))
                                              ) nl))
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

