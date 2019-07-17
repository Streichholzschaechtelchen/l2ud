(* Convert a Tl2.file into a Formal.G EIDLPMCFG grammar with string type symbols *)

open Formal

type formal_rule_map = (G.rule list) G.M.t
                                            
let fill_in_lincat (i: Tl2.ident) _ (rs: formal_rule_map) =
  G.M.add i [] rs

let rec assoc_expr (e: G.idlexpr) : G.idlexpr =
  match e with
    G.E
  | G.V
  | G.T _
  | G.N (_,_) -> e
  | G.C es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.C es' -> (List.rev es') @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.C (List.rev es')
  | G.I es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.I es' -> (List.rev es') @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.I (List.rev es')
  | G.D es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.D es' -> (List.rev es') @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.D (List.rev es')
  | G.L e     -> let e' = assoc_expr e in
                 match e' with
                   G.L e'' -> e'
                 | _       -> G.L e'
  
let rec convert_expr (e: Tl2.expr) : G.idlexpr =
  match e with
    Tl2.Eepsilon         -> G.E
  | Tl2.Eempty           -> G.V
  | Tl2.Estring s        -> G.T s
  | Tl2.Eproject (id, i) -> G.N (id, i)
  | Tl2.Econcat (e1, e2) -> G.C ([convert_expr e1; convert_expr e2])
  | Tl2.Einterl (e1, e2) -> G.I ([convert_expr e1; convert_expr e2])
  | Tl2.Edisj (e1, e2)   -> G.D ([convert_expr e1; convert_expr e2])
  | Tl2.Elock (e')       -> G.L (convert_expr e')

let convert_assoc_expr (e: Tl2.expr) : G.idlexpr =
  assoc_expr (convert_expr e)

let rec assoc_log (e: G.logic) : G.logic =
  match e with
    G.LBool _
  | G.LN (_,_) -> e
  | G.LAnd es  -> let es' = List.fold_left
                              (fun acc e
                               -> match (assoc_log e) with
                                    G.LAnd es' -> es' @ acc
                                  | e'         -> e'::acc)
                              [] es
                  in G.LAnd es'
  | G.LOr es   -> let es' = List.fold_left
                              (fun acc e
                               -> match (assoc_log e) with
                                    G.LOr es' -> es' @ acc
                                  | e'        -> e'::acc)
                              [] es
                  in G.LOr es'
  | G.LNot e   -> let e' = assoc_log e in
                  match e' with
                   G.LNot e'' -> e''
                  | _          -> G.LNot e'

let rec convert_log (l: Tl2.log) : G.logic =
  match l with
    Tl2.Land (l1, l2)    -> G.LAnd ([convert_log l1; convert_log l2])
  | Tl2.Lor (l1, l2)     -> G.LOr ([convert_log l1; convert_log l2])
  | Tl2.Lnot l'          -> G.LNot (convert_log l')
  | Tl2.Lproject (id, i) -> G.LN (id, i)
  
let add_rule_from_lin _ (l: Tl2.lin) (rs: formal_rule_map) =
  let lin_outc = l.lin_outc
  and lin_rcrd = l.lin_rcrd
  and lin_logc = l.lin_logc in
  let rules = G.M.find lin_outc rs in
  let incats = l.lin_args in
  let exprs = Array.map convert_assoc_expr lin_rcrd in
  let rule = match lin_logc with
      None   -> G.ARule (G.{ incats; exprs })
    | Some l -> G.ERule (G.{ incats; exprs; cndtn = convert_log l })
  in Formal.G.M.add lin_outc (rule::rules) rs

let convert_file (file: Tl2.file) : G.grammar =
  let rules = Tl2.M.fold fill_in_lincat file.lincats G.M.empty in
  let rules = Tl2.M.fold add_rule_from_lin file.lins rules in
  let start = "S" in
  G.{ rules; start }
