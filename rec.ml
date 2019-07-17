(* Convert a Tl1.file into transfer language 2's Tl2.file, computing all RECord projections *)

let tl2_typ = List.length

let rec search k = function
    []                    -> raise Not_found
  | (k',v)::t when k = k' -> v
  | _::t                  -> search k t
    
let rec index e = function
    []              -> raise Not_found
  | h::t when h = e -> 0
  | _::t            -> 1 + (index e t)

let rec tl2_log (lincats: Tl1.lincat_map)
                (args: (Tl1.ident * Tl1.ident) list)
                (l: Tl1.log option)
        : Tl2.log option =
  let rec aux = function
    Tl1.Land (l1, l2)     -> Tl2.Land (aux l1, aux l2)
  | Tl1.Lor (l1, l2)      -> Tl2.Lor (aux l1, aux l2)
  | Tl1.Lnot l'           -> Tl2.Lnot (aux l')
  | Tl1.Lproject (i1, i2) -> let i = index i2 (Tl1.M.find (search i1 args)
                                                 lincats) in
                             Tl2.Lproject (i1, i)
  in match l with
       None   -> None
     | Some l'-> Some (aux l')
                                       
let rec tl2_expr (lincats: Tl1.lincat_map)
                 (args: (Tl1.ident * Tl1.ident) list)
                 (e: Tl1.expr)
        : Tl2.expr =
  match e with
    Tl1.Eepsilon          -> Tl2.Eepsilon
  | Tl1.Eempty            -> Tl2.Eempty
  | Tl1.Estring s         -> Tl2.Estring s
  | Tl1.Eident i          -> assert false
  | Tl1.Eproject (i1, i2) -> let i = index i2 (Tl1.M.find (search i1 args)
                                                 lincats) in
                             Tl2.Eproject (i1, i)
  | Tl1.Econcat (e1, e2)  -> Tl2.Econcat (tl2_expr lincats args e1,
                                          tl2_expr lincats args e2)
  | Tl1.Einterl (e1, e2)  -> Tl2.Einterl (tl2_expr lincats args e1,
                                          tl2_expr lincats args e2)
  | Tl1.Edisj (e1, e2)    -> Tl2.Edisj (tl2_expr lincats args e1,
                                        tl2_expr lincats args e2)
  | Tl1.Elock e           -> Tl2.Elock (tl2_expr lincats args e)

let tl2_record (lincats: Tl1.lincat_map)
               (args: (Tl1.ident * Tl1.ident) list)
               (r: Tl1.record)
    : Tl2.record =
  Array.of_list (List.fold_right (fun (_, e) acc -> (tl2_expr lincats args e)::acc) r [])
                                       
let tl2_lin (lincats: Tl1.lincat_map) (l: Tl1.lin) : Tl2.lin =
  let lin_outc = l.lin_outc
  and lin_args = l.lin_args
  and lin_logc = tl2_log lincats l.lin_args l.lin_logc
  and lin_rcrd = tl2_record lincats l.lin_args l.lin_rcrd in
  Tl2.{ lin_outc; lin_args; lin_logc; lin_rcrd }

let tl2_file (file: Tl1.file) : Tl2.file =
  let name = file.name
  and lincats = Tl1.M.map tl2_typ file.lincats
  and lins = Tl1.M.map (tl2_lin file.lincats) file.lins in
  Tl2.{ name; lincats; lins }
    
