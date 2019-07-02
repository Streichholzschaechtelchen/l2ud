module type Symbol = sig

  type t
  val compare: t -> t -> int
  val print  : t -> unit
  val lock_labels: < reset: unit -> unit; get: unit -> t >
  val to_string: t -> string
  val append_int: t -> int -> t
  val default_label: t

end
              
module EIDLPMCFG (T: Symbol) (N: Symbol) = struct

  type term = T.t
  type nonterm = N.t

  module T = T
  module N = N               

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

  (* 1. GRAMMAR DEFINITION *)

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
    | D ies   -> begin print_string "\\/(";
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
                            List.iter print_rule rule) g.rules

  (* 2. LINEARIZATION FUNCTIONS *)

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
 
  (* 3. COMPILATION OF ERULES *)

  type tril = TTrue | TFalse | TUndef

  let (||+) a b =
    match a, b with
      TTrue , _
    | _     , TTrue  -> TTrue
    | TFalse, TFalse -> TFalse
    | _     , _      -> TUndef
                          
  let (&&+) a b =
    match a, b with
      TTrue , TTrue  -> TTrue
    | TFalse, _
    | _     , TFalse -> TFalse
    | _     , _      -> TUndef

  let (//+) a b =
    match a, b with
    | _     , TUndef -> TUndef
    | _     , _      -> a

  let not_ = function
      TTrue  -> TFalse
    | TFalse -> TTrue
    | TUndef -> TUndef
                                    
  module Nontermi = struct
    type t = nonterm * int
    let compare = compare
  end
                   
  module NontermiS = Set.Make(Nontermi)
                          
  (* Return all term * int pairs used in conditions *)
  let used_in_cndtn g =
    let rec process_logic incats ntis = function
        LN (n, i) -> NontermiS.add (List.assoc n incats, i) ntis
      | LAnd ls   
      | LOr ls    -> List.fold_left (process_logic incats) ntis ls
      | LNot l    -> process_logic incats ntis l
      | LBool _   -> ntis in
    let process_rule ntis = function
        ARule a_r -> ntis
      | ERule e_r -> process_logic e_r.incats ntis e_r.cndtn in
    M.fold (fun n rs ntis -> List.fold_left process_rule ntis rs)
      g.rules NontermiS.empty

  (* Return all fields to mark with emptiness flag *)
  let fields_to_mark g =
    let used_ntis = used_in_cndtn g in
    let rec process_idlexpr args (ntis, new_) = function
      E      -> (ntis, new_)
    | T _    -> (ntis, new_)
    | N (n,i)-> let cat = M.find n args in
                begin match NontermiS.mem (cat,i) ntis with
                  true  -> ntis, new_
                | false -> (NontermiS.add (cat,i) ntis, NontermiS.add (cat,i) new_)
                end
    | C ies
    | I ies
    | D ies  -> List.fold_left (process_idlexpr args) (ntis, new_) ies
    | L ie   -> process_idlexpr args (ntis, new_) ie in
    let rec process_nti (n, i) ntis =
      let args incats =
        List.fold_left (fun args (n, cat) -> M.add n cat args) M.empty incats in
      let process_rule (ntis, new_) = function
          ARule a_r -> process_idlexpr (args a_r.incats) (ntis, new_) a_r.exprs.(i)
        | ERule e_r -> process_idlexpr (args e_r.incats) (ntis, new_) e_r.exprs.(i) in
      let rs = M.find n g.rules in
      let ntis, new_ = List.fold_left process_rule (ntis, NontermiS.empty) rs in
      NontermiS.fold process_nti new_ ntis
    in NontermiS.fold process_nti used_ntis used_ntis

  (* Compute all new categories associated with old categories *)
  let compute_tasm g: tril array list M.t =
    let ftm = fields_to_mark g in
    (*temp*)
    print_string "compute_tasm\n";
    NontermiS.iter (fun (cat,i) -> N.print cat; print_int i; print_newline()) ftm;
    print_newline();
    (*endtemp*)
    let cats: tril array list M.t = M.empty in
    let arity = function
        ARule a_r -> Array.length (a_r.exprs)
      | ERule e_r -> Array.length (e_r.exprs) in
    let cats = M.fold (fun cat rs cats -> M.add cat [Array.make (arity (List.hd rs)) TUndef] cats) g.rules cats in
    let process_nti (n,i) cats =
      M.update n (function None     -> assert false
                         | Some tas -> Some (List.fold_left (fun l ta -> let tat, taf = ta, Array.copy ta in
                                                                   tat.(i) <- TTrue; taf.(i) <- TFalse;
                                                                   tat::taf::l) [] tas))
        cats
    in NontermiS.fold process_nti ftm cats

  (* Return new nonterminal encoding new category *)
  let nonterm_of_cat cat ta =
    let index = snd (Array.fold_left (fun (i, acc) v
                                      -> (i * 3, match v with
                                                   TFalse -> acc + i
                                                 | TUndef -> acc
                                                 | TTrue  -> acc + 2 * i))
                       (1, 0) ta)
    in match index with 0 -> cat
                      | _ -> N.append_int cat index

  (* Read tril from incats3 list *)
  let get_trinary_value (n, i) incats3 =
    let rec aux = function
          []                         -> assert false
        | (n', _, ta)::t when n' = n -> ta.(i)
        | _::t                       -> aux t in
      aux incats3

  (* Compute new outcat given tril arrays of incats *)
  let compute_new_outcat cat msk ies incats3 =
    let rec process_ie = function
        E      -> TFalse
      | T _    -> TTrue
      | N (n,i)-> get_trinary_value (n,i) incats3
      | C ies
      | I ies
      | D ies  -> List.fold_left (||+) TFalse (List.map process_ie ies)
      | L ie   -> process_ie ie in
    nonterm_of_cat cat (Array.map2 (//+) (Array.map process_ie ies) msk)

  (* Compute logic given tril arrays of incats *)
  let rec compute_logic incats3 = function
      LN (n,i)    -> get_trinary_value (n, i) incats3
    | LAnd ls     -> List.fold_left (&&+) TTrue (List.map (compute_logic incats3) ls)
    | LOr ls      -> List.fold_left (||+) TFalse (List.map (compute_logic incats3) ls)
    | LNot l      -> not_ (compute_logic incats3 l)
    | LBool true  -> TTrue
    | LBool false -> TFalse


  (* Clean list of rules, removing useless rules and useless categories in turn *)
  let clean_rules rules =
    (* Try to remove rules *)
    let rec remove_rules rules =
      let rules' = M.map (fun rs ->
                       List.filter (fun r ->
                           let f = List.for_all (fun (_, cat) -> M.mem cat rules) in
                           match r with
                             ARule a_r -> f a_r.incats
                           | ERule e_r -> f e_r.incats) rs ) rules in
      if rules = rules' then rules' else remove_cats rules'
    and remove_cats rules =
      let rules' = M.filter (fun cat rs -> match rs with [] -> false | _ -> true) rules in
      if rules = rules' then rules' else remove_rules rules'
    in remove_rules rules

  (* EIDL-PMCFG -> IDL-PMCFG *)
  let develop_grammar g =
    let tasm  = compute_tasm g in
    let nsm   = M.mapi (fun cat tas -> List.map (nonterm_of_cat cat) tas) tasm in
    let rec new_incats_choices acc (n, cat) =
      let tas = M.find cat tasm in
      let ns  = M.find cat nsm in
      List.fold_left2 (fun bdd ta n' ->
          List.fold_left (fun cee choices -> ((n, n', ta)::choices)::cee)
            bdd acc) [] tas ns
    in
    let rules =
      M.fold (fun cat rs rules
              -> let outcatmsk = List.hd (M.find cat tasm) in
                 List.fold_left (fun rules -> function
                       ARule a_r -> let incats3l = List.fold_left new_incats_choices [[]] a_r.incats in
                                    let incatsl  = List.map (List.map (fun (n, cat, _) -> (n, cat))) incats3l in
                                    let outcatl  = List.map (compute_new_outcat cat outcatmsk a_r.exprs) incats3l in
                                    List.fold_left2 (fun rules incats outcat ->
                                        M.update outcat (function None ->
                                                                   Some ([ARule ({ a_r with incats })])
                                                                | Some l ->
                                                                   Some ((ARule ({ a_r with incats }))::l)) rules)
                                      rules incatsl outcatl
                     | ERule e_r -> let incats3l = List.fold_left new_incats_choices [[]] e_r.incats in
                                    let incatsl  = List.map (List.map (fun (n, cat, _) -> (n, cat))) incats3l in
                                    let outcatl  = List.map (compute_new_outcat cat outcatmsk e_r.exprs) incats3l in
                                    let logicl   = List.map (fun incats3 -> compute_logic incats3 e_r.cndtn) incats3l in
                                    let outlogl  = List.combine outcatl logicl in
                                    let exprs    = e_r.exprs in
                                    List.fold_left2 (fun rules incats (outcat, logic) ->
                                        match logic with
                                          TTrue -> M.update outcat
                                                     (function None ->
                                                                Some ([ARule ({ exprs; incats })])
                                                             | Some l ->
                                                                Some ((ARule ({ exprs; incats }))::l)) rules
                                        | TFalse -> rules
                                        | TUndef -> assert false)
                                      rules incatsl outlogl)
                   rules rs) g.rules M.empty in
    let new_starts = M.find g.start nsm in
    let n = match M.find_opt g.start g.rules with
        None        -> 0
      | Some []     -> assert false
      | Some (h::_) -> match h with
                         ARule a_r -> Array.length a_r.exprs
                       | ERule e_r -> Array.length e_r.exprs in
    let rules = match n with
        0 -> rules
      | _ -> List.fold_left (fun rules new_start ->
                 if new_start = g.start
                 then rules
                 else let exprs  = Array.init n (fun i -> N (N.default_label, i)) in
                      let incats = [(N.default_label, new_start)] in
                      let r = ARule ({exprs; incats}) in
                      M.update g.start (function None -> Some ([r])
                                               | Some rs -> Some (r::rs)) rules) rules new_starts in
    let rules = clean_rules rules in
    let start = g.start in
    { rules; start }
      

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
  let to_string = fun x -> x
  let append_int s i = s ^ "(" ^ (string_of_int i) ^ ")"
  let default_label = "x"

end

module G = EIDLPMCFG(MyString)(MyString)
             















