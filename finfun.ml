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
    | Tgfpm.Trecord _     -> assert false in
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

let rec remove_block (e: Tgfpm.expr) : Tgfpm.expr =
  match e.expr_node with
    Tgfpm.Estring _ | Tgfpm.Eident _ -> e 
    | Tgfpm.Eselect (e1, e2)
      -> { e with expr_node = Tgfpm.Eselect (remove_block e1,
                                             remove_block e2) }
    | Tgfpm.Eproject (e', i)
      -> { e with expr_node = Tgfpm.Eproject (remove_block e', i) }
    | Tgfpm.Eblock e
      -> e
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
    | Tgfpm.Erecord r
      -> { e with expr_node = Tgfpm.Erecord (List.map (fun (i, e) -> (i, remove_block e)) r) }
    | Tgfpm.Etable t
      -> { e with expr_node = Tgfpm.Etable (List.map (fun (i, e) -> (i, remove_block e)) t) }

let tl1_expr (params_: Tgfpm.param_map)
             (fli: formal_lincat_map)
             (old_args: (Tgfpm.ident * Tgfpm.ident) list)
             (new_args: (Tl1.ident * Tl1.ident) list)
             (e: Tgfpm.expr)
             (outc: Tgfpm.ident)
    : Tl1.record * Tl1.ident =
  (*Flatten record, replacing subrecords fields by dotted fields*)
  let rec flatten b (pref, acc) (i, (e: Tgfpm.expr)) =
    match e.expr_node with
      Tgfpm.Erecord r -> let new_pref = if b then (string_project pref i) else i
                         in List.fold_left (flatten true) (new_pref, acc) r
    | _               -> (pref, (i, e)::acc) in
  let e = remove_block e in
  let e = snd ( (flatten false) ("", []) (outc, e) ) in
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
  (*Pre-compute expression, deleting tables and lambda nodes*)
  let rec evaluate (m: Tgfpm.ident Tgfpm.M.t) (e: Tgfpm.expr) : Tgfpm.expr = match e.expr_node with
      Tgfpm.Estring _ -> e
    | Tgfpm.Eident i  -> begin try { e with expr_node = Tgfpm.Eident (Tgfpm.M.find i m) }
                               with Not_found -> begin try make_record i e
                                                       with _ -> e
                                                 end
                         end
    | Tgfpm.Eselect (e1, e2)
      -> begin let e_e1 = evaluate m e1 and e_e2 = evaluate m e2 in
               match e_e1.expr_node, e_e2.expr_node with
                 Tgfpm.Etable t, Tgfpm.Eident i
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
    | Tgfpm.Eproject (e', i)
      -> begin let e_e' = evaluate m e' in
               match e_e'.expr_node with
                 Tgfpm.Erecord r
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
    | Tgfpm.Econcat (e1, e2)    -> { e with expr_node = Tgfpm.Econcat (evaluate m e1, evaluate m e2) }
    | Tgfpm.Einterl (e1, e2)    -> { e with expr_node = Tgfpm.Einterl (evaluate m e1, evaluate m e2) }
    | Tgfpm.Edisj (e1, e2)      -> { e with expr_node = Tgfpm.Edisj (evaluate m e1, evaluate m e2) }
    | Tgfpm.Elock e             -> { e with expr_node = Tgfpm.Elock (evaluate m e) }
    | Tgfpm.Elambda (i1, i2, e) -> { e with expr_node = Tgfpm.Elambda (i1, i2, evaluate m e) }
    | Tgfpm.Erecord r           -> { e with expr_node = Tgfpm.Erecord (List.map (fun (i,e) -> (i, evaluate m e)) r) }
    | Tgfpm.Etable t            -> { e with expr_node = Tgfpm.Etable (List.map (fun (i,e) -> (i, evaluate m e)) t) }
  in
  (*Convert Tgfpm.expr -> Tl1.expr*)
  let rec convert (e: Tgfpm.expr) =
    match e.expr_node with
      Tgfpm.Estring s        -> Tl1.Estring s
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
                                              
let tl1_lin (params: Tgfpm.param_map)
            (fli: formal_lincat_map)
            (tfm: to_formal_map)
            (i: Tgfpm.ident)
            (l: Tgfpm.lin)
            (acc: Tl1.lin_map)
    : Tl1.lin_map =
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
  let handle_nuple (j, bdd) nuple =
    let nuple = List.rev nuple in
    let lin_ident = i ^ "#" ^ (string_of_int j) in
    let lin_args = List.map2 (fun a b -> (fst a,b)) t_lin_args nuple in
    let fli = List.fold_left2 (fun m a b -> Tl1.M.add (fst a) (Tl1.M.find b fli)  m) Tl1.M.empty lin_args nuple in
    let lin_rcrd, lin_outc = tl1_expr params fli t_lin_args lin_args t_lin_expr t_lin_outc in
    let new_lin = Tl1.{ lin_outc; lin_args; lin_rcrd } in
    (j+1, Tl1.M.add lin_ident new_lin bdd)
  in snd (List.fold_left handle_nuple (0, acc) cats_nuples)
   
let tl1_lins (params: Tgfpm.param_map)
             (fli: formal_lincat_map)
             (tfm: to_formal_map)
             (l: Tgfpm.lin_map)
  : Tl1.lin_map =
  Tgfpm.M.fold (tl1_lin params fli tfm) l Tl1.M.empty

let tl1_file (file: Tgfpm.file) : Tl1.file =
  let t_params  = file.params
  and t_lincats = file.lincats
  and t_lins    = file.lins in
  let fli, tfm = flatten_lincats t_params t_lincats in
  let lins = tl1_lins t_params fli tfm t_lins in
  let lincats = tl1_lincats fli in
  let name = file.name in
  let includeccs = file.includeccs in
  { name; includeccs; lincats; lins }
               

 



