module type Symbol = sig

  type t
  val compare: t -> t -> int
  val print  : t -> unit
  val lock_labels: < reset: unit -> unit; get: unit -> t >

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

  (* 1. FRONTEND GRAMMAR *)

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


  (* Printing functions *)
                   
  let rec print_idlexpr = function
      E       -> print_string "Epsilon"
    | T t     -> T.print t
    | N (n,i) -> begin N.print n;
                       print_string "[";
                       print_int i;
                       print_string "]"
                 end
    | C ies   -> begin print_string "++(";
                       List.iter (fun ie -> print_idlexpr ie; print_string ", ") ies;
                       print_string ")"
                 end
    | I ies   -> begin print_string "||(";
                       List.iter (fun ie -> print_idlexpr ie; print_string ", ") ies;
                       print_string ")"
                 end
    | D ies   -> begin print_string "\/(";
                       List.iter (fun ie -> print_idlexpr ie; print_string ", ") ies;
                       print_string ")"
                 end
    | L ie    -> begin print_string "`(";
                       print_idlexpr ie;
                       print_string ")"
                 end
                   
  let rec print_logic = function
      LN (n,i)    -> begin N.print n;
                           print_string "[";
                           print_int i;
                           print_string "]"
                     end
    | LAnd ls     -> begin print_string "And(";
                           List.iter (fun l -> print_logic l; print_string ", ") ls;
                           print_string ")"
                     end
    | LOr ls      -> begin print_string "Or(";
                           List.iter (fun l -> print_logic l; print_string ", ") ls;
                           print_string ")"
                     end
    | LNot l      -> begin print_string "Not(";
                           print_logic l;
                           print_string ")"
                     end
    | LBool true  -> print_string "True"
    | LBool false -> print_string "False"
        
  let print_rule = function
      ARule a_r -> begin print_string "    ";
                         List.iter (fun (a, b) -> print_string "(";
                                                  N.print a;
                                                  print_string " : ";
                                                  N.print b;
                                                  print_string ") ") a_r.incats;
                         print_string "->\n";
                         Array.iteri (fun i ie -> print_string "      ";
                                                  print_int i;
                                                  print_string ": ";
                                                  print_idlexpr ie;
                                                  print_newline ()) a_r.exprs
                   end
    | ERule e_r -> begin print_string "    ";
                         List.iter (fun (a, b) -> print_string "(";
                                                  N.print a;
                                                  print_string " : ";
                                                  N.print b;
                                                  print_string ") ") e_r.incats;
                         print_string "->\n";
                         print_string "     if ";
                         print_logic e_r.cndtn;
                         print_newline();
                         Array.iteri (fun i ie -> print_string "      ";
                                                  print_int i;
                                                  print_string ": ";
                                                  print_idlexpr ie;
                                                  print_newline ()) e_r.exprs
                   end


  let print_grammar g =
    print_string ("<Frontend EIDLPMCFG grammar>\n");
    print_string "start: ";
    N.print g.start;
    print_newline ();
    print_string "rules:\n";
    M.iter (fun cat rule -> print_string "  ";
                            N.print cat;
                            print_string " <- \n";
                            List.iter print_rule rule) g.rules;

  (* 2. BACKEND GRAMMAR (WITH SEPARATE LOCKS) *)
  (* Logic is not yet implemented *)

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
                    
  (* Printing functions *)
                   
  let rec print_idlexpr' = function
      E'       -> print_string "Epsilon"
    | T' t     -> T.print t
    | N' (n,i) -> begin N.print n;
                        print_string "[";
                        print_int i;
                        print_string "]"
                  end
    | C' ies'   -> begin print_string "++(";
                         List.iter (fun ie' -> print_idlexpr' ie'; print_string ", ") ies';
                         print_string ")"
                   end
    | I' ies'   -> begin print_string "||(";
                         List.iter (fun ie' -> print_idlexpr' ie'; print_string ", ") ies';
                         print_string ")"
                   end
    | D' ies'   -> begin print_string "\/(";
                         List.iter (fun ie' -> print_idlexpr' ie'; print_string ", ") ies';
                         print_string ")"
                   end
        
  let print_rule' r' = 
     print_string "    ";
     List.iter (fun (a, b) -> print_string "(";
                              N.print a;
                              print_string " : ";
                              N.print b;
                              print_string ") ") r'.incats';
     print_string "->\n";
     Array.iteri (fun i ie -> print_string "      ";
                              print_int i;
                              if snd ie then print_string "[`]";
                              print_string ": ";
                              print_idlexpr' (fst ie);
                              print_newline ()) r'.exprs'

  let print_grammar' g =
    print_string ("<Backend EIDLPMCFG grammar>\n");
    print_string "start: ";
    N.print g.start';
    print_newline ();
    print_string "rules:\n";
    M.iter (fun cat rule -> print_string "  ";
                            N.print cat;
                            print_string " <- \n";
                            List.iter print_rule' rule) g.rules'

  (* 3. LINEARIZATION FUNCTIONS *)

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

  (* 4. CONVERSION BETWEEN GRAMMAR FORMATS *)

  (* Add (category name -> rule) mapping to rule map *)
  let append_rule cat r rs =
    M.update cat (function None -> Some [r] | Some rl -> Some (r::rl)) rs

  (* Convert IDL expressions and rules to the new format *)
  let rec convert_idlexpr (acc, rs, incats) =
    let rec aux (acc, rs, incats) = function
        E       -> (E'::acc, rs, incats)
      | T t     -> ((T' t)::acc, rs, incats)
      | N (n,i) -> ((N' (n,i))::acc, rs, incats)
      | C ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   ((C' (List.rev ies'))::acc, rs, incats)
      | I ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   ((I' (List.rev ies'))::acc, rs, incats)
      | D ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   ((D' (List.rev ies'))::acc, rs, incats)
      | L ie    -> let lock_label = N.lock_labels#get () in
                   let lock_rule  = ARule ({ incats; exprs = [| ie |] }) in
                   let rs = convert_rule lock_label lock_rule rs in
                   ((N' (lock_label,0))::acc, rs, incats)                   
    and fold_aux rs incats ies = List.fold_left aux ([], rs, incats) ies
    in    
    function
        E       -> ((E', false)::acc, rs, incats)
      | T t     -> (((T' t), false)::acc, rs, incats)
      | N (n,i) -> (((N' (n,i)), false)::acc, rs, incats)
      | C ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   (((C' (List.rev ies')), false)::acc, rs, incats)
      | I ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   (((I' (List.rev ies')), false)::acc, rs, incats)
      | D ies   -> let ies', rs, incats = fold_aux rs incats ies in
                   (((D' (List.rev ies')), false)::acc, rs, incats)
      | L ie    -> let ies', rs, incats = aux ([], rs, incats) ie in
                   let ie' = List.hd ies' in
                   ((ie', true)::acc, rs, incats)

  and convert_rule cat r rs =
    match r with
      ARule a_r -> let ies', rs, incats' = (Array.fold_left convert_idlexpr
                                             ([], rs, a_r.incats) a_r.exprs) in
                   let exprs' = Array.of_list (List.rev ies') in
                   append_rule cat { incats'; exprs' } rs
    | ERule e_r -> assert false

  and convert_rules cat rl rs =
    List.fold_right (convert_rule cat) rl rs

  (* Convert grammar from frontend to backend format *)
  let convert g =
    let start' = g.start in
    N.lock_labels#reset ();
    let rules' = M.fold convert_rules g.rules M.empty in
    { rules'; start' }      

end
              
(* typo: if p.2 *)
(* p.2, def. of comb: def. is not clear *)

module MyString = struct
  
  type t = string
  let compare = String.compare
  let print = print_string
  let lock_labels = object      
      val mutable c = 0
      method reset () =
        c <- 0
      method get () =
        let new_label = "@lock@" ^ (string_of_int c) in
        c <- c + 1;
        new_label
    end

end

module G = EIDLPMCFG(MyString)(MyString)
             
