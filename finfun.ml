(* Convert a typed Tgfpm.file into transfer language 1's Tl1.file, computing all FINite FUNctions *)

let string_project i1 i2 =
  i1 ^ "." ^ i2

let string_select i1 (i2,i3) =
  i1 ^ "/" ^ i2 ^ "=" ^ i3

let string_apply i1 i2 =
  i1 ^ "!" ^ i2

let string_formal i cats =
  List.fold_left string_select i (Tl1.M.bindings cats)

let update_cats old_cats param values =
  List.fold_left (fun acc value ->
      List.fold_left (fun acc old_cat -> (Tl1.M.add param value old_cat)::acc)
        acc
        old_cats)
    []
    values

let flatten_typ (params: Tgfpm.param_map)
                (i: Tgfpm.ident)
                (t: Tgfpm.typ)
    : (Tl1.ident Tl1.M.t list) * Tl1.typ =
  let rec flatten b (pref, acc) (i, t) =
    match t with
      Tgfpm.Trecord r -> let new_pref = if b then (string_project pref i) else i
                         in List.fold_left (flatten true) (new_pref, acc) r
    | _               -> (pref, (i, t)::acc) in
  let rec develop (cats, fields) (i, t) =
    match t with
      Tgfpm.Ttyp          -> assert false
    | Tgfpm.Tset          -> (cats, i::fields)
    | Tgfpm.Tparam p      -> let values = Tgfpm.M.find p params in
                             ((update_cats cats i values), fields)
    | Tgfpm.Ttable (p, t) -> let values = Tgfpm.M.find p params in
                             List.fold_left (fun (cats, acc) v ->
                                 develop (cats, acc) (string_apply i v, t)
                               ) (cats, fields) values
    | Tgfpm.Trecord _
    | Tgfpm.Tbool
    | Tgfpm.Tbogus
    | Tgfpm.Tskip         -> assert false in
  let cats, typ = List.fold_left develop ([Tl1.M.empty], [])
                    (snd (flatten false ("", []) (i, t))) in
  cats, List.sort compare typ

type formal_lincat_map = (Tl1.ident Tl1.M.t * Tl1.typ) Tl1.M.t
type to_formal_map = Tl1.ident list Tgfpm.M.t
                                                                  
let flatten_lincats (params: Tgfpm.param_map) (l: Tgfpm.lincat_map)
  : formal_lincat_map * to_formal_map =
  Tgfpm.M.fold (fun i t (m, tfm) ->
      let cats, typ = flatten_typ params i t in
      (List.fold_left (fun m cat -> Tl1.M.add (string_formal i cat) (cat, typ) m)
         m cats),
      Tgfpm.M.add i (List.map (string_formal i) cats) tfm)
    l (Tl1.M.empty, Tgfpm.M.empty)
                                                               
let tl1_lincats =
  Tl1.M.map snd

let update_cats old_cats param values =
  List.fold_left (fun acc value ->
      List.fold_left (fun acc old_cat -> ((param,value)::old_cat)::acc)
        acc
        old_cats)
    []
    values           
  
(* Remove block nodes *)
let rec remove_block (e: Tgfpm.expr) : Tgfpm.expr =
  match e.expr_node with
      Tgfpm.Eepsilon | Tgfpm.Eempty | Tgfpm.Estring _ | Tgfpm.Eident _ -> e 
    | Tgfpm.Eselect (e1, e2)
      -> { e with expr_node = Tgfpm.Eselect (remove_block e1,
                                             remove_block e2) }
    | Tgfpm.Eproject (e', i)
      -> { e with expr_node = Tgfpm.Eproject (remove_block e', i) }
    | Tgfpm.Eblock e
      -> remove_block e
    | Tgfpm.Econcat (e1, e2)
      -> { e with expr_node = Tgfpm.Econcat (remove_block e1,
                                             remove_block e2) }
    | Tgfpm.Einterl (e1, e2)
      -> { e with expr_node = Tgfpm.Einterl (remove_block e1,
                                             remove_block e2) }
    | Tgfpm.Edisj (e1, e2)
      -> { e with expr_node = Tgfpm.Edisj (remove_block e1,
                                           remove_block e2) }
    | Tgfpm.Elock e'
      -> { e with expr_node = Tgfpm.Elock (remove_block e') }
    | Tgfpm.Elambda (i1, i2, e')
      -> { e with expr_node = Tgfpm.Elambda (i1, i2, remove_block e') }
    | Tgfpm.Efor (i1, i2, e')
      -> { e with expr_node = Tgfpm.Efor (i1, i2, remove_block e') }
    | Tgfpm.Erecord r
      -> { e with expr_node = Tgfpm.Erecord (List.map (fun (i, e) -> (i, remove_block e)) r) }
    | Tgfpm.Etable t
      -> { e with expr_node = Tgfpm.Etable (List.map (fun (i, e) -> (i, remove_block e)) t) }
    | Tgfpm.Eif (e1, e2, e3)
      -> { e with expr_node = Tgfpm.Eif (remove_block e1,
                                         remove_block e2,
                                         remove_block e3) }
    | Tgfpm.Eand (e1, e2)
      -> { e with expr_node = Tgfpm.Eand (remove_block e1,
                                          remove_block e2) }
    | Tgfpm.Eor (e1, e2)
      -> { e with expr_node = Tgfpm.Eor (remove_block e1,
                                         remove_block e2) }
    | Tgfpm.Enot e'
      -> { e with expr_node = Tgfpm.Enot (remove_block e') }
    | Tgfpm.Etrue
    | Tgfpm.Efalse
    | Tgfpm.Eskip -> e
    | Tgfpm.Econcrecord _
    | Tgfpm.Enilrecord
    | Tgfpm.Econctable _
    | Tgfpm.Eniltable -> assert false

let rec contains_skip : Tgfpm.record -> bool = function
    []       -> false
  | (_,e)::t -> begin match e.expr_node with
                  Tgfpm.Eskip -> true
                | _           -> contains_skip t
                end

(*Develop for expressions, producing several new exprs*)
let rec develop_for (params_: Tgfpm.param_map)
          (m: Tgfpm.ident Tgfpm.M.t)
          (e: Tgfpm.expr)
        : Tgfpm.expr list =
  let develop_for = develop_for params_ in
  let rec cartesian a b =
    match a, b with
      []   , _
    | _    , [] -> []
    | h::t , _  -> let acc = cartesian t b in
                   List.fold_left (fun acc x -> (h, x)::acc) acc b in
  let rec cartesiann = function
      []   -> [[]]
    | h::t -> let acc = cartesiann t in
              List.fold_left (fun bdd ac ->
                  List.fold_left (fun bdd x ->
                      (x::ac)::bdd) bdd h)
                [] acc in
  let rec cartesian3 a b c =
    let abc = cartesiann [a; b; c] in
    List.map (function [x;y;z] -> (x,y,z) | _ -> assert false) abc in
  let rec apply f l = List.flatten (List.map f l) in
  match e.expr_node with
    Tgfpm.Eepsilon
  | Tgfpm.Eempty
  | Tgfpm.Estring _
  | Tgfpm.Etrue
  | Tgfpm.Efalse
  | Tgfpm.Eskip                -> [e]
  | Tgfpm.Eident i             -> begin match Tgfpm.M.find_opt i m with
                                    None   -> [e]
                                  | Some v -> [{ e with expr_node = Tgfpm.Eident v }]
                                  end
  | Tgfpm.Eselect (e1, e2)     -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Eselect (e1, e2) }]) e12s
  | Tgfpm.Efor (i1, i2, e')    -> let params = Tgfpm.M.find i2 params_ in
                                  apply (fun v -> develop_for (Tgfpm.M.add i1 v m) e') params
  | Tgfpm.Eproject (e', i)     -> let e's = develop_for m e' in
                                  apply (fun e' -> [{ e with expr_node = Tgfpm.Eproject (e', i) }]) e's
  | Tgfpm.Eblock e'            -> let e's = develop_for m e' in
                                  apply (fun e' -> [{ e with expr_node = Tgfpm.Eblock e' }]) e's
  | Tgfpm.Econcat (e1, e2)     -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Econcat (e1, e2) }]) e12s
  | Tgfpm.Einterl (e1, e2)     -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Einterl (e1, e2) }]) e12s
  | Tgfpm.Edisj (e1, e2)       -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Edisj (e1, e2) }]) e12s
  | Tgfpm.Elock e'             -> let e's = develop_for m e' in
                                  apply (fun e' -> [{ e with expr_node = Tgfpm.Elock e' }]) e's
  | Tgfpm.Elambda (i1, i2, e') -> let e's = develop_for m e' in
                                  apply (fun e' -> [{ e with expr_node = Tgfpm.Elambda (i1, i2, e') }]) e's
  | Tgfpm.Erecord r            -> let rs = List.map (fun (i,e) -> List.map (fun x -> (i,x)) (develop_for m e)) r in
                                  let rs' = cartesiann rs in
                                  apply (fun r -> [{ e with expr_node = Tgfpm.Erecord r }]) rs'
  | Tgfpm.Etable t             -> let ts = List.map (fun (i,e) -> List.map (fun x -> (i,x)) (develop_for m e)) t in
                                  let ts' = cartesiann ts in
                                  apply (fun t -> [{ e with expr_node = Tgfpm.Etable t }]) ts'
  | Tgfpm.Eif (e1, e2, e3)     -> let e1s = develop_for m e1 and e2s = develop_for m e2 and e3s = develop_for m e3 in
                                  let e123s = cartesian3 e1s e2s e3s in
                                  apply (fun (e1, e2, e3) -> [{ e with expr_node = Tgfpm.Eif (e1, e2, e3) }]) e123s
  | Tgfpm.Eand (e1, e2)        -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Eand (e1, e2) }]) e12s
  | Tgfpm.Eor (e1, e2)         -> let e1s = develop_for m e1 and e2s = develop_for m e2 in
                                  let e12s = cartesian e1s e2s in
                                  apply (fun (e1, e2) -> [{ e with expr_node = Tgfpm.Eor (e1, e2) }]) e12s
  | Tgfpm.Enot e'              -> let e's = develop_for m e' in
                                  apply (fun e' -> [{ e with expr_node = Tgfpm.Enot e' }]) e's
  | _                          -> assert false
            
(*Pre-compute expression, deleting tables and lambda nodes, simplifying logic*)
let rec evaluate (params_: Tgfpm.param_map)
                 (fli: formal_lincat_map)
                 (old_args: (Tgfpm.ident * Tgfpm.ident) list)
                 (new_args: (Tl1.ident * Tl1.ident) list)
                 (outc: Tgfpm.ident)
                 (m: Tgfpm.ident Tgfpm.M.t)
                 (e: Tgfpm.expr)
        : Tgfpm.expr =
  let evaluate = evaluate params_ fli old_args new_args outc in
  let rec search k = function
      []                    -> assert false
    | (k',v)::t when k = k' -> v
    | _::t                  -> search k t in
  let make_record i (e: Tgfpm.expr) =
    match e.expr_type with
      Tgfpm.Tset -> let cat = search i new_args in
                    Tgfpm.{ expr_node = Tgfpm.Eproject (
                                            Tgfpm.{ expr_node = (Tgfpm.Eident cat);
                                                    expr_type = Tgfpm.Trecord ([cat, Tgfpm.Tset]) },
                                            cat );
                            expr_type = Tgfpm.Tset }
    | _          -> assert false
  in                          
  match e.expr_node with
    Tgfpm.Eepsilon
  | Tgfpm.Eempty
  | Tgfpm.Estring _ -> e
  | Tgfpm.Eident i  -> begin try { e with expr_node = Tgfpm.Eident (Tgfpm.M.find i m) }
                             with Not_found -> begin try make_record i e
                                                     with _ -> e
                                               end
                       end
  | Tgfpm.Eselect (e1, e2)
    -> begin let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
             match e_e1.expr_node, e_e2.expr_node with
               Tgfpm.Eskip   , _
             | _             , Tgfpm.Eskip
               -> Tgfpm.skip
             | Tgfpm.Etable t, Tgfpm.Eident i
               -> evaluate m (search i t)
             | Tgfpm.Elambda (i,_,e), Tgfpm.Eident j
               -> evaluate (Tgfpm.M.add i j m) e
             | Tgfpm.Eproject (e', i), Tgfpm.Eident j
               -> begin let e_e' = evaluate m e' in
                        match e_e'.expr_node with
                          Tgfpm.Eident k -> let new_field = string_apply i j in
                                            { e with expr_node = Tgfpm.Eproject (e_e', new_field) }
                        | _              -> assert false
                  end
             | _, _ -> assert false 
       end
  | Tgfpm.Efor (_, _, _) -> assert false
  | Tgfpm.Eproject (e', i)
    -> begin let e_e' = evaluate m e' in
             match e_e'.expr_node with
               Tgfpm.Eskip
               -> Tgfpm.skip
             | Tgfpm.Erecord r
               -> evaluate m (search i r)
             | Tgfpm.Eident j
               -> let param_values = fst (Tl1.M.find j fli) in
                  (try { e with expr_node = Tgfpm.Eident (Tl1.M.find i param_values) }
                   with Not_found
                        -> { e with expr_node = Tgfpm.Eproject ({ e' with expr_node = Tgfpm.Eident j }, i) })
             | Tgfpm.Eproject (e'', i')
               -> { e with expr_node = Tgfpm.Eproject (e'', string_project i i') }
             | _ -> assert false
       end
  | Tgfpm.Eblock _ -> assert false
  | Tgfpm.Econcat (e1, e2)    ->
     let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
     begin match e_e1.expr_node, e_e2.expr_node with
       Tgfpm.Eskip, _ | _, Tgfpm.Eskip -> Tgfpm.skip
       | _, _ -> { e with expr_node = Tgfpm.Econcat (e_e1, e_e2) }
     end
  | Tgfpm.Einterl (e1, e2)    ->
     let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
     begin match e_e1.expr_node, e_e2.expr_node with
       Tgfpm.Eskip, _ | _, Tgfpm.Eskip -> Tgfpm.skip
       | _, _ -> { e with expr_node = Tgfpm.Einterl (e_e1, e_e2) }
     end
  | Tgfpm.Edisj (e1, e2)      ->
     let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
     begin match e_e1.expr_node, e_e2.expr_node with
       Tgfpm.Eskip, _ | _, Tgfpm.Eskip -> Tgfpm.skip
       | _, _ -> { e with expr_node = Tgfpm.Edisj (e_e1, e_e2) }
     end
  | Tgfpm.Elock e'            ->
     let e_e' = evaluate m e' in
     begin match e_e'.expr_node with
       Tgfpm.Eskip -> Tgfpm.skip
     | _ -> { e with expr_node = Tgfpm.Elock e_e' }
     end
  | Tgfpm.Elambda (i1, i2, e') ->
     let e_e' = evaluate m e' in
     begin match e_e'.expr_node with
       Tgfpm.Eskip -> Tgfpm.skip
     | _ -> { e' with expr_node = Tgfpm.Elambda (i1, i2, e_e') }
     end
  | Tgfpm.Erecord r           ->
     let e_r = List.map (fun (i,e) -> (i, evaluate m e)) r in
     begin if contains_skip e_r
           then Tgfpm.skip
           else { e with expr_node = Tgfpm.Erecord e_r }
     end
  | Tgfpm.Etable t            ->
     let e_t = List.map (fun (i,e) -> (i, evaluate m e)) t in     
     { e with expr_node = Tgfpm.Etable e_t }
  | Tgfpm.Eif (e1, e2, e3)    ->
     begin let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 and e_e3 = evaluate m e3 in
           match e_e1.expr_node, e_e2.expr_node, e_e3.expr_node with
             Tgfpm.Etrue  , Tgfpm.Eskip, _           -> Tgfpm.skip
           | Tgfpm.Etrue  , _          , _           -> e_e2
           | Tgfpm.Efalse , _          , Tgfpm.Eskip -> e_e3
           | Tgfpm.Efalse , _          , _           -> e_e3
           | _            , Tgfpm.Eskip, Tgfpm.Eskip -> Tgfpm.skip
           | _
             -> { e with expr_node = Tgfpm.Eif (e_e1, e_e2, e_e3) }
     end
  | Tgfpm.Eand (e1, e2)       ->
     begin let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
               match e_e1.expr_node, e_e2.expr_node with
                 Tgfpm.Efalse, _          
               | _           , Tgfpm.Efalse
                 -> { e with expr_node = Tgfpm.Efalse }
               | Tgfpm.Etrue , _
                 -> e_e2
               | _           , Tgfpm.Etrue
                 -> e_e1
               | _           , _
                 -> { e with expr_node = Tgfpm.Eand (e_e1, e_e2) }
     end
  | Tgfpm.Eor (e1, e2)        ->
     begin let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
           match e_e1.expr_node, e_e2.expr_node with
             Tgfpm.Etrue , _
           | _           , Tgfpm.Etrue
             -> { e with expr_node = Tgfpm.Etrue }
           | Tgfpm.Efalse, _
             -> e_e2
           | _           , Tgfpm.Efalse
             -> e_e1
           | _           , _
             -> { e with expr_node = Tgfpm.Eor (e_e1, e_e2) }
     end
  | Tgfpm.Enot e'             ->
     begin let e_e' = evaluate m e' in
           match e_e'.expr_node with
             Tgfpm.Etrue  -> { e with expr_node = Tgfpm.Efalse }
           | Tgfpm.Efalse -> { e with expr_node = Tgfpm.Etrue }
           | _            -> { e with expr_node = Tgfpm.Enot e_e' }
     end
  | Tgfpm.Etrue
  | Tgfpm.Efalse
  | Tgfpm.Eskip
    -> e
  | _ -> assert false

exception Skip

let tl1_expr (params_: Tgfpm.param_map)
             (fli: formal_lincat_map)
             (old_args: (Tgfpm.ident * Tgfpm.ident) list)
             (new_args: (Tl1.ident * Tl1.ident) list)
             (e: Tgfpm.expr)
             (outc: Tgfpm.ident)
    : Tl1.record * Tl1.ident =
  Ast.nummerlappar#reset ();
  let evaluate = evaluate params_ fli old_args new_args outc in
  (*Flatten record, replacing subrecords fields by dotted fields*)
  let rec flatten b (pref, acc) (i, (e: Tgfpm.expr)) =
    match e.expr_node with
      Tgfpm.Erecord r -> let new_pref = if b then (string_project pref i) else i
                         in List.fold_left rec_flatten (new_pref, acc) r
    | _               -> (pref, (i, e)::acc)
  (*Replace all partially evaluated record fields by evaluated lambdas*)
  and rec_flatten (pref, acc) (i, (e: Tgfpm.expr)) =
    let rec aux (e: Tgfpm.expr) =
      match e.expr_node, e.expr_type with
        Tgfpm.Elambda (j, t, e'), Tgfpm.Ttable (_, _)  -> let e'' = aux e' in
                                                          let expr_node = Tgfpm.Elambda (j, t, e'') in
                                                          { e with expr_node }
      | Tgfpm.Etable t          , Tgfpm.Ttable (_, _)  -> let t' = List.map (fun (j, e') -> (j, aux e')) t in
                                                          let expr_node = Tgfpm.Etable t' in
                                                          { e with expr_node }
      | _                       , Tgfpm.Ttable (i', t) -> let v = Ast.nummerlappar#next () in
                                                          let e1 = Tgfpm.{ expr_node = Eident v;
                                                                           expr_type = Tparam i' } in
                                                          let e2 = Tgfpm.{ expr_node = Tgfpm.Eselect (e, e1);
                                                                           expr_type = t } in
                                                          let e3 = aux e2 in
                                                          let expr_node = Tgfpm.Elambda (v, i', e3) in
                                                          { e with expr_node }
      | _                      , _                    -> e 
    in flatten true (pref, acc) (i, (aux e))
  in
  let e = snd ( (flatten false) ("", []) (outc, e) ) in
  (*Convert Tgfpm.expr -> Tl1.expr*)
  let rec convert (e: Tgfpm.expr) =
    match e.expr_node with
      Tgfpm.Eepsilon         -> Tl1.Eepsilon
    | Tgfpm.Eempty           -> Tl1.Eempty
    | Tgfpm.Eskip            -> raise Skip
    | Tgfpm.Estring s        -> Tl1.Estring s
    | Tgfpm.Eident i         -> Tl1.Eident i
    | Tgfpm.Eproject (e', j) -> begin match e'.expr_node with
                                  Tgfpm.Eident i -> Tl1.Eproject (i, j)
                                | _              -> assert false
                                end
    | Tgfpm.Econcat (e1, e2) -> Tl1.Econcat (convert e1, convert e2)
    | Tgfpm.Einterl (e1, e2) -> Tl1.Einterl (convert e1, convert e2)
    | Tgfpm.Edisj (e1, e2)   -> Tl1.Edisj (convert e1, convert e2)
    | Tgfpm.Elock e          -> Tl1.Elock (convert e)
    | _                      -> assert false
  in
  (*Process field i = e and fill in params and others fields*)
  let rec process m (params, fields) (i, (e: Tgfpm.expr)) =
    match e.expr_type with
      Tgfpm.Tparam p -> begin match (evaluate m e).expr_node with
                          Tgfpm.Eident j -> (Tl1.M.add i j params, fields)
                        | _              -> assert false
                        end
    | Tgfpm.Ttable (j,_) -> begin match e.expr_node with
                              Tgfpm.Etable t ->
                               List.fold_left
                                 (fun acc (k, e') -> process m acc
                                                       (string_apply i k, e'))
                                 (params, fields) t
                            | Tgfpm.Elambda (k, p, e') ->
                               let values = Tgfpm.M.find p params_ in
                               List.fold_left
                                 (fun acc v -> process (Tgfpm.M.add k v m) acc
                                                 (string_apply i v, e'))
                                 (params, fields) values
                            | _ -> assert false
                            end
    | _ -> (params, Tl1.M.add i (convert (evaluate m e)) fields)
  in
  let params, fields = List.fold_left (process Tgfpm.M.empty) (Tl1.M.empty, Tl1.M.empty) e in
  (*Compute type of result*)
  let new_outc = string_formal outc params in
  let new_rcrd = Tl1.M.bindings fields in
  new_rcrd, new_outc
            
let rec tl1_log (e: Tgfpm.expr) : Tl1.log option =
  match e.expr_node with
    Tgfpm.Etrue            -> None
  | Tgfpm.Efalse           -> raise Skip
  | Tgfpm.Eand (e1, e2)    -> begin match tl1_log e1, tl1_log e2 with
                                Some e1', Some e2' -> Some (Tl1.Land (e1', e2'))
                              | _       , _        -> assert false
                              end
  | Tgfpm.Eor (e1, e2)     -> begin match tl1_log e1, tl1_log e2 with
                                Some e1', Some e2' -> Some (Tl1.Lor (e1', e2'))
                              | _       , _        -> assert false
                              end
  | Tgfpm.Enot e'          -> begin match tl1_log e' with
                                Some e'' -> Some (Tl1.Lnot e'')
                              | _        -> assert false
                              end
  | Tgfpm.Eproject (e', j) -> begin match e'.expr_node with
                                  Tgfpm.Eident i -> Some (Tl1.Lproject (i, j))
                                | _              -> assert false
                              end
  | _                      -> assert false
                                     
let distribute (e: Tgfpm.expr) : (Tgfpm.expr * Tgfpm.expr) list =
  (* Convert formula into PNF *)
  let p_e = e |> Prenex.chainify |> Prenex.prenexify |> Prenex.unchainify in
  let mapand l =
    List.map (fun (lo, e) -> (Tgfpm.{ expr_node = Tgfpm.Eand (l, lo);
                                      expr_type = Tgfpm.Tbool }, e)) in
  let not_ l =
    Tgfpm.{ expr_node = Tgfpm.Enot l;
            expr_type = Tgfpm.Tbool } in
  let true_ =
    Tgfpm.{ expr_node = Tgfpm.Etrue;
            expr_type = Tgfpm.Tbool } in
  (* Take a formula in PNF and returns a list of logic * expression *)
  (* Both logic and expression are of type Tgfpm.expr at that point *)
  let rec aux (e: Tgfpm.expr) = match e.expr_node with
      Tgfpm.Eif (e1, e2, e3) -> let e2' = aux e2 and e3' = aux e3 in
                                (mapand e1 e2')@(mapand (not_ e1) e3')
    | Tgfpm.Eskip            -> []
    | _                      -> [ true_, e ]
  in aux p_e
                                              
let tl1_lin (params: Tgfpm.param_map)
            (fli: formal_lincat_map)
            (tfm: to_formal_map)
            (i: Tgfpm.ident)
            (l: Tgfpm.lin)
            ((j: int), (acc: Tl1.lin_map))
    : int * Tl1.lin_map =
  let t_lin_args = l.lin_args
  and t_lin_expr = l.lin_expr
  and t_lin_outc = l.lin_outc in
  let cartesian acc cats =
    List.fold_left (fun bdd cat ->
        List.fold_left (fun cee old_nuple -> ((cat::old_nuple)::cee))
          bdd
          acc)
      []
      cats in
  let all_cats = List.map (fun (_, l) -> Tgfpm.M.find l tfm) t_lin_args in
  let cats_nuples = List.fold_left cartesian [[]] all_cats in
  (*List.iter (fun x -> print_int (List.length x);
                      print_string ": ";
                      List.iter print_string x;
                      print_string " ") all_cats;
  print_int (List.length cats_nuples); print_newline ();*)
  let handle_nuple (j, bdd) nuple =
    let nuple = List.rev nuple in
    let lin_args = List.map2 (fun a b -> (fst a,b)) t_lin_args nuple in
    let fli = List.fold_left2 (fun m a b -> Tl1.M.add (fst a) (Tl1.M.find b fli)  m) Tl1.M.empty lin_args nuple in
    let lin_logs_and_exprs = distribute (remove_block t_lin_expr) in
    let handle_log_and_expr (j, bdd) (l, e) =
      let lin_ident = i ^ "#" ^ (string_of_int j) in
      try let lin_rcrd, lin_outc = tl1_expr params fli t_lin_args lin_args e t_lin_outc in
          let lin_logc = tl1_log (evaluate params fli t_lin_args lin_args t_lin_outc Tgfpm.M.empty l) in
          let new_lin = Tl1.{ lin_outc; lin_args; lin_rcrd; lin_logc } in
          (j+1, Tl1.M.add lin_ident new_lin bdd)
      with Skip -> (j, bdd)
    in List.fold_left handle_log_and_expr (j, bdd) lin_logs_and_exprs      
  in List.fold_left handle_nuple (j, acc) cats_nuples

let tl1_lin_develop (params: Tgfpm.param_map)
                    (fli: formal_lincat_map)
                    (tfm: to_formal_map)
                    (i: Tgfpm.ident)
                    (l: Tgfpm.lin)
                    (acc: Tl1.lin_map)
    : Tl1.lin_map =
  let lin_exprs = develop_for params Tgfpm.M.empty l.lin_expr in
  let ls        = List.map (fun lin_expr -> { l with lin_expr }) lin_exprs in
  (*print_int (List.length ls); print_newline ();*)
  snd (List.fold_right (tl1_lin params fli tfm i) ls (0, acc)) 
   
let tl1_lins (params: Tgfpm.param_map)
             (fli: formal_lincat_map)
             (tfm: to_formal_map)
             (l: Tgfpm.lin_map)
  : Tl1.lin_map =
  Tgfpm.M.fold (tl1_lin_develop params fli tfm) l Tl1.M.empty

let tl1_file (file: Tgfpm.file) : Tl1.file =
  let t_params  = file.params
  and t_lincats = file.lincats
  and t_lins    = file.lins in
  let fli, tfm = flatten_lincats t_params t_lincats in
  let lins = tl1_lins t_params fli tfm t_lins in
  let lincats = tl1_lincats fli in
  let name = file.name in
  { name; lincats; lins }
               

 



