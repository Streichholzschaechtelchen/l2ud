module type Symbol = sig

  type t
  val compare: t -> t -> int
  val print  : t -> unit

end
              
module EIDLPMCFG (T: Symbol) (N: Symbol) = struct

  type term = T.t
  type nonterm = N.t

  module M = Map.Make(N)
         
  type deco_term = Term of term
                 | Nonterm of nonterm
                 | Diamond
                         
  type dtlt_t = deco_term list array

  module Dtl = struct
    type t = deco_term list
    let compare = compare
  end
                  
  module Dtlt = struct
    type t = dtlt_t
    let compare = compare
  end

  module Dtltm = struct
    type t = dtlt_t M.t
    let compare = compare
  end

  module DTLS = Set.Make(Dtl)                             
  module S = Set.Make(Dtlt)
  module DTLTMS = Set.Make(Dtltm)

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

  (* Erase diamonds, returning list of terminals and non-terminals *)
  let sigma sl =
    let erase_rev acc = function
        Term t    -> (Term t)::acc
      | Nonterm n -> (Nonterm n)::acc
      | Diamond   -> acc
    in List.rev (List.fold_left erase_rev [] sl)

  (* Print decorated term *)
  let print_deco_term = function
      Term t    -> T.print t
    | Nonterm n -> N.print n
    | Diamond   -> print_string "_"
    
  (* Print list of decorated terms *)
  let print_deco_terms dtl =
    print_string "[";
    List.iter (fun dt -> print_string " ";
                         print_deco_term dt) dtl;
    print_string " ]"

  (* Print tuple of lists of decorated terms *)
  let print_tuple dtlt = 
    print_string "(";
    Array.iter (fun dtl -> print_string " ";
                           print_deco_terms dtl;
                           print_string ";") dtlt;
    print_string " )"

  (* Print set of lists of decorated terms *)
  let print_set dtlts =
    print_string "< set > = {";
    S.iter (fun dtlt -> print_newline (); print_tuple dtlt) dtlts;
    if not (S.is_empty dtlts) then print_newline ();
    print_string "}\n"

  (* Return { f(x,y) | x \in a, y \in b } *)
  let cartesian f a b =
    DTLS.fold (fun x acc -> DTLS.fold (fun y bdd -> DTLS.union (f x y) bdd)
                           b acc)
      a DTLS.empty

  (* Return { f(x_1, f(x_2, ... f(x_{n-1}, x_n)...))
             | (x_1, ..., x_n) \in a_1 x ... x a_n } *)
  let fold_cartesian f = function
      []   -> DTLS.empty
    | h::t -> List.fold_right (fun e acc -> cartesian f e acc) t h

  (* Return { [| x_1; ...; x_n |] 
             | (x_1, ..., x_n) \in a_1 x ... x a_n *)
  let array_cartesian a =
    let l = Array.length a in
    let rec range = function 0 -> [] | n -> (n-1)::(range (n-1)) in
    let empty_arr = Array.make l [] in
    let aux acc i s =
      DTLS.fold (fun dtl bdd ->           
          S.fold (fun dtlt cee ->
              let n_dtlt = Array.copy dtlt in
              n_dtlt.(i) <- dtl;
              S.add n_dtlt cee)
            acc
            bdd)
        s
        S.empty in
    List.fold_left2 aux (S.singleton empty_arr) (range (Array.length a)) (List.rev (Array.to_list a))

  (* Same as above with map implementation instead of array *)
  let map_cartesian a =
    let aux n s acc =
      S.fold (fun dtlt bdd ->
          DTLTMS.fold (fun dtltm cee ->
              DTLTMS.add (M.add n dtlt dtltm) cee)
            acc
            bdd)
        s
        DTLTMS.empty in
    M.fold aux a (DTLTMS.singleton M.empty)
                              
  (* See paper *)
  let rec comb_ x y =
    let rec shift acc = function
        []         -> acc, []
      | Diamond::t -> acc, t
      | h::t       -> shift (h::acc) t
    in match shift [] x with
         _, []   -> DTLS.singleton (x @ Diamond::y)
       | x', x'' -> DTLS.map (fun y' -> (List.rev x') @ Diamond::y') (comb x'' y)

  and comb x y =
    DTLS.union (comb_ x y) (comb_ y x)

  (* Evaluate whether condition holds for given values of variables *)
  let rec evaluate_logic li = function
      LN (n,i) -> (try (M.find n li).(i) = []
                   with Not_found -> failwith "Logic can only use input variables")
    | LAnd cs  -> List.for_all (evaluate_logic li) cs
    | LOr cs   -> List.exists (evaluate_logic li) cs
    | LNot c   -> not (evaluate_logic li c)
    | LBool b  -> b
    
                 
  (* Add to acc all linearizations of given idl expression using given rule*)
  let rec linearize_ rs acc r =
    let incats, exprs = match r with
        ARule r -> r.incats, r.exprs
      | ERule r -> r.incats, r.exprs
    in
    let li = List.fold_left (fun bdd n -> M.add (fst n) (linearize rs (snd n)) bdd)
               M.empty incats in
    let lic = map_cartesian li in
    if DTLTMS.is_empty lic
    then (S.union (array_cartesian (Array.map (linearize_e M.empty) exprs)) acc)
    else DTLTMS.fold (fun li bdd ->
             let proceed = match r with
                 ARule _ -> true
               | ERule r -> evaluate_logic li r.cndtn
             in if proceed
                then S.union (array_cartesian (Array.map (linearize_e li)
                                                 exprs))
                       bdd
                else bdd
           )
           lic acc

  and linearize_e li = function
      E       -> DTLS.singleton []
    | T t     -> DTLS.singleton [Term t]
    | N (n,i) -> (try DTLS.singleton (M.find n li).(i)
                  with Not_found -> assert false)
    | C ies   -> let lin_ies = List.map (linearize_e li) ies in
                 fold_cartesian (fun a b ->
                     DTLS.singleton (b @ (Diamond::a))) lin_ies
    | I ies   -> let lin_ies = List.map (linearize_e li) ies in
                 fold_cartesian comb lin_ies
    | D ies   -> List.fold_left (fun acc ie -> DTLS.union acc
                                                 (linearize_e li ie))
                           DTLS.empty ies
    | L ie    -> DTLS.map sigma (linearize_e li ie)
    
  (* Return all linearizations of given nonterminal using rules *)
  and linearize rs n =
    let rules = try M.find n rs
                with Not_found -> failwith
                                    "Reference to non-existing rule" in
    List.fold_left (linearize_ rs) S.empty rules

  (* Return set L(G) *)
  let language g =
    S.map (Array.map sigma) (linearize g.rules g.start)

end
              
(* typo: if p.2 *)
(* p.2, def. of comb: def. is not clear *)

module MyString = struct
  
  type t = string
  let compare = String.compare
  let print = print_string

end

module G = EIDLPMCFG(MyString)(MyString)
