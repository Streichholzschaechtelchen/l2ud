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

  (* create a word set matching a complete terminal list *)
  let total ts =
    interval 0 (List.length ts)

  (* merge two word sets that are assumed to be valid *)
  let rec merge a b =
    let rec join x y u v = match u, v with
        r::s::t, l::m::n when s >= l && r = x  -> join s y t v
      | r::s::t, l::m::n when m >= r && l = x  -> join m y u n
      | r::s::t, []      when r = x            -> join s y t []
      | []     , l::m::n when l = x            -> join m y [] n
      | _      , _                             -> (interval x y, u, v) in
    let rec aux a b = match a, b with
        []     , _                   -> b
      | _      , []                  -> a
      | y::x::t, g::f::e when x >= g -> let i, a, b = join x y t b in
                                        i@(aux a b)
      | y::x::t, g::f::e when f >= y -> let i, a, b = join f g a e in
                                        i@(aux a b)
      | _      , _                   -> raise Incompatible in
    aux a b

  (* return true iff the two (valid) word sets intersect *)
  let rec intersect a b = match a, b with
      []     , _
    | _      , []                  -> false
    | y::x::t, g::f::e when x >= g -> intersect t b
    | y::x::t, g::f::e when f >= y -> intersect a e
    | _      , _                   -> true
      
  (* append a word set to another word set *)
  let append a b = match a, List.rev b with
      [], _  -> b
    | _ , [] -> a
    | x::t, y::u when y > x -> b@a
    | x::t, y::u when y = x -> (List.rev u)@t
    | _ , _  -> raise Incompatible

  (* return true iff the word set is locked, ie represents an interval *)
  let locked = function
      []     -> true
    | [y; x] -> true
    | _      -> false

  (* print word set *)
  let print a =
    print_string "(";
    let rec loop = function
        []      -> ()
      | x::y::t -> begin print_string "[";
                         print_int x;
                         print_string ";";
                         print_int y;
                         print_string "[";
                         loop t
                   end
      | _       -> assert false
    in loop (List.rev a);
       print_string ")"
                  
end

module WordSetStack = struct

  type t = WordSet.t list

  exception Empty_WordSetStack
  exception Singleton_WordSetStack

  let push (ws: WordSet.t) (wss: t) : t =
    ws::wss

  let pop : t -> (WordSet.t * t) = function
      []   -> raise Empty_WordSetStack
    | h::t -> h, t

  let merge : t list -> t = function
      []   -> raise Empty_WordSetStack
    | h::t -> let _, t'  = pop h in
              let y, t'' = pop t' in
              push (List.fold_left (fun acc ws -> WordSet.merge (fst (pop ws)) acc) y (h::t)) t''

  let lock (wss: t) : t =
    push WordSet.empty wss
      
  let unlock : t -> t = function
      []                            -> raise Empty_WordSetStack
    | [a]                           -> raise Singleton_WordSetStack
    | a::b::t when WordSet.locked a -> push (WordSet.merge a b) t
    | a::b::t                       -> raise WordSet.Incompatible

  let append (a: WordSet.t) (wss: t) : t =
    let b, wss = pop wss in
    push (WordSet.append b a) wss

  let init : t =
    [WordSet.empty]

  let locked (wss: t) : bool =
    WordSet.locked (fst (pop wss))

  let print (wss: t) : unit =
    print_string "[";
    List.iter (fun ws -> WordSet.print ws; print_string "; ") wss;
    print_string "]"

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
                   
  type idlexpr = E
               | T of term
               | N of nonterm * int
               | C of idlexpr list
               | I of idlexpr list
               | D of idlexpr list
               | L of idlexpr

  type logic = LN of nonterm * int
             | LAnd of logic list
             | LOr of logic list
             | LNot of logic
             | LBool of bool

  type a_rule = { incats: (nonterm * nonterm) list;
                  exprs : idlexpr array }

  type e_rule = { incats: (nonterm * nonterm) list;
                  exprs : idlexpr array;
                  cndtn : logic }

  type rule = ARule of a_rule | ERule of e_rule
                                           
  type grammar = { rules: (rule list) M.t;
                   start: nonterm }

  val print_idlexpr: idlexpr -> unit
  val print_grammar: grammar -> unit
                                 
end

module Graph (G: Grammar) = struct

  type label = Term    of G.term
             | Nonterm of G.nonterm * int
             | Epsilon

  type graph = Split  of graph list
             | Merge  of graph * int * int
             | Node   of edge list
             | Lock   of graph
             | Unlock of graph
             | Final

  and edge = graph * label
                      
  module Cut = struct
    type t = graph
    let compare = compare
  end

  module CutS = Set.Make(Cut)
  module CutM = Map.Make(Cut)

  type cut = WordSetStack.t CutM.t
                       
  type transition = { lbl: label; lst: graph option; cut: cut }

  (* return the list of transitions available from a valid cut *)
  let next (cut: cut): transition list =
    let process_edge part_cut s trans (g, l) =
      { lbl = l;
        lst = Some g;
        cut = CutM.add g s part_cut }::trans
    in
    let process_node g s (trans, merges) = match g with
        Split gs         -> let s' = WordSetStack.lock s in
                            ({ lbl = Epsilon;
                               lst = None;
                               cut = List.fold_right (fun g' -> CutM.add g' s') gs (CutM.remove g cut) }::trans,
                             merges)
      | Merge (g', j, i) -> (trans, CutM.update g'
                                      (function None ->
                                                 Some ([(g,s)], i) | Some (gs, i) -> Some ((g,s)::gs, i)) merges)
      | Lock g'          -> ({ lbl = Epsilon;
                               lst = None;
                               cut = CutM.add g' (WordSetStack.lock s) (CutM.remove g cut) }::trans, merges)
      | Unlock g'        -> begin try ({ lbl = Epsilon;
                                         lst = None;
                                         cut = CutM.add g' (WordSetStack.unlock s) (CutM.remove g cut) }::trans,
                                       merges)
                                  with WordSet.Incompatible -> (trans, merges)
                             end
      | Node es          -> (List.fold_left (process_edge (CutM.remove g cut) s) trans es, merges)
      | Final            -> (trans, merges)
    in
    let process_merge g (gs, i) trans =
      if List.length gs = i
      then let g_, s_ = List.split gs in
           try let merged = WordSetStack.merge s_ in
               { lbl = Epsilon;
                 lst = None;
                 cut = CutM.add g merged (List.fold_right CutM.remove g_ cut) }::trans
           with WordSet.Incompatible -> trans
      else trans
    in
    let trans, merges = CutM.fold process_node cut ([], CutM.empty) in
    CutM.fold process_merge merges trans

  (* apply one transition, given a label and its WordSet *)
  let apply_one (t: transition) (s: WordSet.t): (bool * cut) option =
    match t.lst with
      None   -> Some (false, t.cut)
    | Some g -> try let cut = CutM.update g (function Some s' -> Some (WordSetStack.append s s')
                                                    | _ -> assert false) t.cut in
                    Some (true, cut)
                with WordSet.Incompatible -> None

  (* apply all possible transitions from a list, given a label and its WordSet *)
  let apply (ts: transition list) (l: label) (s: WordSet.t): (bool * cut) list =
    let ts = List.filter (fun t -> t.lbl = l) ts in
    let process_transition cuts t = match t.lst with
        None   -> (false, t.cut)::cuts
      | Some g -> try let cut = CutM.update g (function Some s' -> Some (WordSetStack.append s s')
                                                      | _ -> assert false) t.cut in
                      (true, cut)::cuts
                  with WordSet.Incompatible -> cuts
    in List.fold_left process_transition [] ts


  (* generate graph from IDL expression *)
  let of_idlexpr expr =
    let rec aux succ = function
        G.E       -> Final
      | G.T t     -> Node ([succ, Term t])
      | G.N (n,i) -> Node ([succ, Nonterm (n,i)])
      | G.C ies   -> List.fold_left aux succ (List.rev ies) 
      | G.I ies   -> let l = List.length ies in
                     let gs = List.mapi (fun i e' -> aux (Merge (succ, i, l)) e') ies in
                     Split gs
      | G.D ies   -> let gs = List.map (fun e' -> (aux (Node [succ, Epsilon]) e', Epsilon)) ies in
                     Node gs
      | G.L ie    -> Lock (aux (Unlock succ) ie)
    in aux Final expr

  (* generate initial cut from IDL expression *)
  let init expr =
    let g = of_idlexpr expr in
    CutM.add g WordSetStack.init CutM.empty

  (* print graph *)
  let rec print_graph =
    let print_label = function
        Term t        -> G.T.print t
      | Nonterm (n,i) -> begin G.N.print n; print_string "["; print_int i; print_string "]" end
      | Epsilon       -> print_string "Epsilon"
    in function
      Split gs      -> begin print_string "Split(";
                             List.iter (fun g -> print_graph g; print_string ", ") gs;
                             print_string ")"
                       end
    | Merge (g,i,j) -> begin print_string "Merge(";
                             print_graph g;
                             print_string ", ";
                             print_int (i+1);
                             print_string "/";
                             print_int j;
                             print_string ")"
                       end
    | Node es       -> begin print_string "Node(";
                             List.iter (fun (g,l) -> print_label l;
                                                     print_string ": ";
                                                     print_graph g;
                                                     print_string ", ") es;
                             print_string ")"
                       end
    | Lock g        -> begin print_string "Lock(";
                             print_graph g;
                             print_string ")"
                       end
    | Unlock g      -> begin print_string "Unlock(";
                             print_graph g;
                             print_string ")"
                       end
    | Final         -> print_string "Final"

  (* print cut *)
  let print_cut cut =
    print_string "<cut> = {\n";
    CutM.iter (fun g ws -> print_graph g;
                           print_string " -> ";
                           WordSetStack.print ws;
                           print_newline ()) cut;
    print_string "}"
    
end

                              
module Int = struct
  type t = int
  let compare = compare
end

               
module I = Map.Make(Int)

                   
module ContextEntry (G: Grammar) = struct

  type t = { cat: G.nonterm;
             wsm: WordSet.t I.t option }

  let empty cat = { cat; wsm = None }

end
                    
  
module Context (G: Grammar) = struct

  module E = ContextEntry(G)
                                    
  module Nonterm = struct
    type t = G.nonterm
    let compare = compare
  end
                     
  module M = Map.Make(Nonterm)

  type nonterm = G.nonterm
  type term    = G.term

  type t = E.t M.t

  (* create initial context from incats *)
  let create (incats: (nonterm * nonterm) list): t =
    List.fold_right (fun (n, cat) -> M.add n (E.empty cat)) incats M.empty
 
  (* extract substring matching positions from WordSet *)
  (* string is encoded as a list of terms *)
  let substr (ts: term list) (ws: WordSet.t) : term list =
    let rec aux o ws ts = match ws, ts with
        []     , _
      | _      , []              -> []
      | x::y::t, h::u when y = o -> aux (o+1) t u
      | x::y::t, h::u when x = o -> h::(aux (o+1) ((x+1)::y::t) u)
      | x::y::t, h::u when o < x -> aux (o+1) ws u
      | _      , _               -> assert false
    in aux 0 (List.rev ws) ts

  (* return true iff given category/dimension pair may cover a given WordSet in text w.r.t. context *)
  let compat (cntxt: t) (ts: term list) (n: nonterm) (i: int) (wsm: WordSet.t I.t): bool =
    let ws = I.find i wsm in
    let intersecting = (M.exists (fun _ (e: E.t) ->
                            match e.wsm with
                              None   -> false
                            | Some w -> I.exists (fun _ e' -> WordSet.intersect ws e') w) cntxt) in
    match (M.find n cntxt).wsm with
      None   when not intersecting -> true
    | None                         -> false
    | Some w                       -> begin match I.find_opt i w with
                                        Some ws' when substr ts ws = substr ts ws' -> true
                                      | _                                          -> false
                                      end

  (* update context adding nonterminal of given category on given WordSet *)
  let reserve (cntxt: t) (n: nonterm) (i: int) (wsm: WordSet.t I.t): t =
    M.update n (function None   -> assert false
                       | Some e -> match (e: E.t).wsm with
                                     None  -> (Some { e with wsm = Some wsm })
                                   | Some _ -> Some e) cntxt

  exception Cannot_unify
              
  (* unify two contexts if possible *)
  let unify (c: t) (d: t) : t option =
    let unify_i (i: WordSet.t I.t) (j: WordSet.t I.t) =
      I.merge (fun k g h -> match g, h with
                              None,    None                 -> None
                            | Some ws, None                 -> Some ws
                            | None,    Some yt              -> Some yt
                            | Some ws, Some yt when ws = yt -> Some ws
                            | _,       _                    -> raise Cannot_unify)
        i j in
    try Some (M.union (fun n a b -> match (a: E.t).wsm, b.wsm with
                                    | None,   None   -> Some a
                                    | Some e, None   -> Some a
                                    | None,   Some f -> Some b
                                    | Some e, Some f -> Some (E.{ a with wsm = Some (unify_i e f) }))
                c d)
    with Cannot_unify -> None

  (* print context *)
  let print (cntxt: t): unit =
    print_string "<context> = {\n";
    let aux1 n (e: E.t) =
      G.N.print n;
      print_string ":";
      G.N.print e.cat;
      match e.wsm with
        None   -> print_string " none\n"
      | Some w -> begin print_string " [ ";
                        I.iter (fun i ws -> print_string "#";
                                            print_int i;
                                            print_string ":";
                                            WordSet.print ws;
                                            print_string " ") w
                  end;
      print_string "]\n"
    in M.iter aux1 cntxt;
       print_string "}"

end

(*TEST*)

(* to test, first type
                    #directory "_build";;
                    #load "formal.cmo";;
   in the toplevel *)
                              
(*open Formal

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

let ie = G.C ([G.L (G.I [G.T "a"; G.T "b"]); G.T "b"])

let ie_g = of_idlexpr ie

 *)
