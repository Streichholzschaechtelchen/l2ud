(* Convert a Tl2.file into a Formal.G EIDLPMCFG grammar with string type symbols *)

open Formal

type formal_rule_map = (G.rule list) G.M.t
                                            
let fill_in_lincat (i: Tl2.ident) _ (rs: formal_rule_map) =
  G.M.add i [] rs

let rec assoc_expr (e: G.idlexpr) : G.idlexpr =
  match e with
    E
  | G.T _
  | G.N (_,_) -> e
  | G.C es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.C es' -> es' @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.C es'
  | G.I es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.I es' -> es' @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.I es'
  | G.D es    -> let es' = List.fold_left
                             (fun acc e
                              -> match (assoc_expr e) with
                                   G.D es' -> es' @ acc
                                 | e'      -> e'::acc)
                             [] es
                 in G.D es'
  | G.L e     -> let e' = assoc_expr e in
                 match e' with
                   G.L e'' -> e'
                 | _       -> G.L e'
  
let rec convert_expr (e: Tl2.expr) : G.idlexpr =
  match e with
    Tl2.Estring s        -> G.T s
  | Tl2.Eproject (id, i) -> G.N (id, i)
  | Tl2.Econcat (e1, e2) -> G.C ([convert_expr e1; convert_expr e2])
  | Tl2.Einterl (e1, e2) -> G.I ([convert_expr e1; convert_expr e2])
  | Tl2.Edisj (e1, e2)   -> G.D ([convert_expr e1; convert_expr e2])
  | Tl2.Elock (e')       -> G.L (convert_expr e')

let convert_assoc_expr (e: Tl2.expr) : G.idlexpr =
  assoc_expr (convert_expr e)
  
let add_rule_from_lin _ (l: Tl2.lin) (rs: formal_rule_map) =
  let lin_outc = l.lin_outc
  and lin_rcrd = l.lin_rcrd in
  let rules = G.M.find lin_outc rs in
  let incats = l.lin_args in
  let exprs = Array.map convert_assoc_expr lin_rcrd in
  let rule = G.ARule (G.{ incats; exprs }) in
  Formal.G.M.add lin_outc (rule::rules) rs

let convert_file (file: Tl2.file) : G.grammar =
  let rules = Tl2.M.fold fill_in_lincat file.lincats G.M.empty in
  let rules = Tl2.M.fold add_rule_from_lin file.lins rules in
  let start = "S" in
  G.{ rules; start }
