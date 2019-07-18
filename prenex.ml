(* Replace tables and records by chained lists *)
(* Add Econcrecord, Enilrecord, Econctable, Eniltable *)
(* Delete Erecord, Etable *)
let rec chainify (e: Tgfpm.expr) : Tgfpm.expr =
  let expr_node = 
    match e.expr_node with
      Tgfpm.Eepsilon
    | Tgfpm.Eempty
    | Tgfpm.Estring _
    | Tgfpm.Eident _
    | Tgfpm.Eskip
    | Tgfpm.Etrue
    | Tgfpm.Efalse              -> e.expr_node
    | Tgfpm.Eselect (e1, e2)    -> Tgfpm.Eselect (chainify e1, chainify e2)
    | Tgfpm.Eproject (e', i)    -> Tgfpm.Eproject (chainify e', i)
    | Tgfpm.Eblock e'           -> assert false
    | Tgfpm.Econcat (e1, e2)    -> Tgfpm.Econcat (chainify e1, chainify e2)
    | Tgfpm.Einterl (e1, e2)    -> Tgfpm.Einterl (chainify e1, chainify e2)
    | Tgfpm.Edisj (e1, e2)      -> Tgfpm.Edisj (chainify e1, chainify e2)
    | Tgfpm.Elock e'            -> Tgfpm.Elock (chainify e')
    | Tgfpm.Elambda (i1, i2, e')-> Tgfpm.Elambda (i1, i2, chainify e')
    | Tgfpm.Erecord r           -> chainify_record r
    | Tgfpm.Etable t            -> chainify_table t
    | Tgfpm.Eif (e1, e2, e3)    -> Tgfpm.Eif (chainify e1, chainify e2, chainify e3)
    | Tgfpm.Eand (e1, e2)       -> Tgfpm.Eand (chainify e1, chainify e2)
    | Tgfpm.Eor (e1, e2)        -> Tgfpm.Eor (chainify e1, chainify e2)
    | Tgfpm.Enot e'             -> Tgfpm.Enot (chainify e')
    | Tgfpm.Enilrecord
    | Tgfpm.Eniltable
    | Tgfpm.Econcrecord _
    | Tgfpm.Econctable _        -> assert false
  in { e with expr_node }

and chainify_record = function
    []       -> Tgfpm.Enilrecord
  | (i,e)::t -> Tgfpm.Econcrecord ((i, chainify e),
                                   { expr_node = chainify_record t;
                                     expr_type = Tgfpm.Tbogus })(*Bogus type*)

and chainify_table = function
    []       -> Tgfpm.Eniltable
  | (i,e)::t -> Tgfpm.Econctable ((i, chainify e),
                                   { expr_node = chainify_table t;
                                     expr_type = Tgfpm.Tbogus })(*Bogus type*)
  
(* Pull up all "if" nodes *)
let rec prenexify (e: Tgfpm.expr) : Tgfpm.expr =
  let p = prenexify in
  match e.expr_node with
      Tgfpm.Eepsilon
    | Tgfpm.Eempty
    | Tgfpm.Estring _
    | Tgfpm.Eident _
    | Tgfpm.Eskip
    | Tgfpm.Etrue
    | Tgfpm.Efalse
    | Tgfpm.Enilrecord
    | Tgfpm.Eniltable -> e
    | Tgfpm.Eselect (e1, e2)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2 in
               match p_e1.expr_node, p_e2.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Eselect (f2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Eselect (f2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k3 = p { e with expr_node = Tgfpm.Eselect (f3, g3) } in
                    let k2 = p { e with expr_node = Tgfpm.Eselect (f3, g2) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Eselect (f2, p_e2) } in
                    let g3 = p { e with expr_node = Tgfpm.Eselect (f3, p_e2) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _                     , Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Eselect (p_e1, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Eselect (p_e1, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _ -> e
         end
    | Tgfpm.Eproject (e', i)
      -> begin let p_e' = prenexify e' in
               match p_e'.expr_node with
                 Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Eproject (f2, i) } in
                    let g3 = p { e with expr_node = Tgfpm.Eproject (f3, i) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _ -> e
         end
    | Tgfpm.Eblock _ -> assert false
    | Tgfpm.Econcat (e1, e2)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2 in
               match p_e1.expr_node, p_e2.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Econcat (f2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Econcat (f2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Econcat (f3, g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Econcat (f3, g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Econcat (f2, p_e2) } in
                    let g3 = p { e with expr_node = Tgfpm.Econcat (f3, p_e2) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _                     , Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Econcat (p_e1, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Econcat (p_e1, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _ -> e
         end
    | Tgfpm.Einterl (e1, e2)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2 in
               match p_e1.expr_node, p_e2.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Einterl (f2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Einterl (f2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Einterl (f3, g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Einterl (f3, g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Einterl (f2, p_e2) } in
                    let g3 = p { e with expr_node = Tgfpm.Einterl (f3, p_e2) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _                     , Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Einterl (p_e1, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Einterl (p_e1, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _ -> e
         end
    | Tgfpm.Edisj (e1, e2)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2 in
               match p_e1.expr_node, p_e2.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Edisj (f2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Edisj (f2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Edisj (f3, g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Edisj (f3, g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Edisj (f2, p_e2) } in
                    let g3 = p { e with expr_node = Tgfpm.Edisj (f3, p_e2) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _                     , Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Edisj (p_e1, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Edisj (p_e1, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _ -> e
         end
    | Tgfpm.Elock e'
      -> begin let p_e' = prenexify e' in
               match p_e'.expr_node with
                 Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Elock f2 } in
                    let g3 = p { e with expr_node = Tgfpm.Elock f3 } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _ -> e
         end
    | Tgfpm.Elambda (i1, i2, e')
      -> begin let p_e' = prenexify e' in
               match p_e'.expr_node with
                 Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Elambda (i1, i2, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Elambda (i1, i2, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _ -> e
         end
    | Tgfpm.Erecord r -> assert false (* Replaced in the last step *)
    | Tgfpm.Etable t  -> assert false (* Replaced in the last step *)
    | Tgfpm.Eif (e1, e2, e3)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2
               and p_e3 = prenexify e3 in
               match p_e1.expr_node, p_e2.expr_node, p_e3.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3), Tgfpm.Eif (h1, h2, h3)
                 -> let m2 = p { e with expr_node = Tgfpm.Eif (f2, g2, h2) } in
                    let m3 = p { e with expr_node = Tgfpm.Eif (f2, g2, h3) } in
                    let k2 = { e with expr_node = Tgfpm.Eif (h1, m2, m3) } in
                    let n2 = p { e with expr_node = Tgfpm.Eif (f2, g3, h2) } in
                    let n3 = p { e with expr_node = Tgfpm.Eif (f2, g3, h3) } in
                    let k3 = { e with expr_node = Tgfpm.Eif (h1, n2, n3) } in
                    let j2 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    let o2 = p { e with expr_node = Tgfpm.Eif (f3, g2, h2) } in
                    let o3 = p { e with expr_node = Tgfpm.Eif (f3, g2, h3) } in
                    let l2 = { e with expr_node = Tgfpm.Eif (h1, o2, o3) } in
                    let p2 = p { e with expr_node = Tgfpm.Eif (f3, g3, h2) } in
                    let p3 = p { e with expr_node = Tgfpm.Eif (f3, g3, h3) } in
                    let l3 = { e with expr_node = Tgfpm.Eif (h1, p2, p3) } in
                    let j3 = { e with expr_node = Tgfpm.Eif (g1, l2, l3) } in
                    { e with expr_node = Tgfpm.Eif (f1, j2, j3) }
               | Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3), _
                 -> let j2 = p { e with expr_node = Tgfpm.Eif (f2, g2, p_e3) } in
                    let j3 = p { e with expr_node = Tgfpm.Eif (f2, g3, p_e3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Eif (f3, g2, p_e3) } in
                    let k3 = p { e with expr_node = Tgfpm.Eif (f3, g3, p_e3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _, Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Eif (f2, p_e2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Eif (f2, p_e2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Eif (f3, p_e2, g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Eif (f3, p_e2, g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | _, Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Eif (p_e1, f2, g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Eif (p_e1, f2, g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Eif (p_e1, f3, g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Eif (p_e1, f3, g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _, _
                 -> let g2 = p { e with expr_node = Tgfpm.Eif (f2, p_e2, p_e3) } in
                    let g3 = p { e with expr_node = Tgfpm.Eif (f3, p_e2, p_e3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Eif (p_e1, f2, p_e3) } in
                    let g3 = p { e with expr_node = Tgfpm.Eif (p_e1, f3, p_e3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _, Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Eif (p_e1, p_e2, f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Eif (p_e1, p_e2, f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _, _ -> e
         end
    | Tgfpm.Eand (e1, e2) ->
       begin let p_e1 = prenexify e1
             and p_e2 = prenexify e2 in
             match p_e1.expr_node, p_e2.expr_node with
               Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
               -> let j2 = p { e with expr_node = Tgfpm.Eand (f2, g2) } in
                  let j3 = p { e with expr_node = Tgfpm.Eand (f2, g3) } in
                  let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                  let k2 = p { e with expr_node = Tgfpm.Eand (f3, g2) } in
                  let k3 = p { e with expr_node = Tgfpm.Eand (f3, g3) } in
                  let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                  { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
             | Tgfpm.Eif (f1, f2, f3), _
               -> let g2 = p { e with expr_node = Tgfpm.Eand (f2, p_e2) } in
                  let g3 = p { e with expr_node = Tgfpm.Eand (f3, p_e2) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _                     , Tgfpm.Eif (f1, f2, f3)
               -> let g2 = p { e with expr_node = Tgfpm.Eand (p_e1, f2) } in
                  let g3 = p { e with expr_node = Tgfpm.Eand (p_e1, f3) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _, _ -> e
       end
    | Tgfpm.Eor (e1, e2) ->
       begin let p_e1 = prenexify e1
             and p_e2 = prenexify e2 in
             match p_e1.expr_node, p_e2.expr_node with
               Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
               -> let j2 = p { e with expr_node = Tgfpm.Eor (f2, g2) } in
                  let j3 = p { e with expr_node = Tgfpm.Eor (f2, g3) } in
                  let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                  let k2 = p { e with expr_node = Tgfpm.Eor (f3, g2) } in
                  let k3 = p { e with expr_node = Tgfpm.Eor (f3, g3) } in
                  let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                  { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
             | Tgfpm.Eif (f1, f2, f3), _
               -> let g2 = p { e with expr_node = Tgfpm.Eor (f2, p_e2) } in
                  let g3 = p { e with expr_node = Tgfpm.Eor (f3, p_e2) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _                     , Tgfpm.Eif (f1, f2, f3)
               -> let g2 = p { e with expr_node = Tgfpm.Eor (p_e1, f2) } in
                  let g3 = p { e with expr_node = Tgfpm.Eor (p_e1, f3) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _, _ -> e
       end       
    | Tgfpm.Enot e'
      -> begin let p_e' = prenexify e' in
               match p_e'.expr_node with
                 Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Enot f2 } in
                    let g3 = p { e with expr_node = Tgfpm.Enot f3 } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _ -> e
         end
    | Tgfpm.Econcrecord ((i,e1),e2)
      -> begin let p_e1 = prenexify e1
               and p_e2 = prenexify e2 in
               match p_e1.expr_node, p_e2.expr_node with
                 Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
                 -> let j2 = p { e with expr_node = Tgfpm.Econcrecord ((i,f2), g2) } in
                    let j3 = p { e with expr_node = Tgfpm.Econcrecord ((i,f2), g3) } in
                    let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                    let k2 = p { e with expr_node = Tgfpm.Econcrecord ((i,f3), g2) } in
                    let k3 = p { e with expr_node = Tgfpm.Econcrecord ((i,f3), g3) } in
                    let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                    { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
               | Tgfpm.Eif (f1, f2, f3), _
                 -> let g2 = p { e with expr_node = Tgfpm.Econcrecord ((i,f2), p_e2) } in
                    let g3 = p { e with expr_node = Tgfpm.Econcrecord ((i,f3), p_e2) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _                     , Tgfpm.Eif (f1, f2, f3)
                 -> let g2 = p { e with expr_node = Tgfpm.Econcrecord ((i,p_e1), f2) } in
                    let g3 = p { e with expr_node = Tgfpm.Econcrecord ((i,p_e1), f3) } in
                    { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
               | _, _ -> e
         end
  | Tgfpm.Econctable ((i,e1),e2)
    -> begin let p_e1 = prenexify e1
             and p_e2 = prenexify e2 in
             match p_e1.expr_node, p_e2.expr_node with
               Tgfpm.Eif (f1, f2, f3), Tgfpm.Eif (g1, g2, g3)
               -> let j2 = p { e with expr_node = Tgfpm.Econctable ((i,f2), g2) } in
                  let j3 = p { e with expr_node = Tgfpm.Econctable ((i,f2), g3) } in
                  let h2 = { e with expr_node = Tgfpm.Eif (g1, j2, j3) } in
                  let k2 = p { e with expr_node = Tgfpm.Econctable ((i,f3), g2) } in
                  let k3 = p { e with expr_node = Tgfpm.Econctable ((i,f3), g3) } in
                  let h3 = { e with expr_node = Tgfpm.Eif (g1, k2, k3) } in
                  { e with expr_node = Tgfpm.Eif (f1, h2, h3) }
             | Tgfpm.Eif (f1, f2, f3), _
               -> let g2 = p { e with expr_node = Tgfpm.Econctable ((i,f2), p_e2) } in
                  let g3 = p { e with expr_node = Tgfpm.Econctable ((i,f3), p_e2) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _                     , Tgfpm.Eif (f1, f2, f3)
               -> let g2 = p { e with expr_node = Tgfpm.Econctable ((i,p_e1), f2) } in
                  let g3 = p { e with expr_node = Tgfpm.Econctable ((i,p_e1), f3) } in
                  { e with expr_node = Tgfpm.Eif (f1, g2, g3) }
             | _, _ -> e
       end

(* Restore tables and records *)
(* Add Etable, Erecord *)
(* Delete Econcrecord, Enilrecord, Econctable, Eniltable *)
let rec unchainify (e: Tgfpm.expr) : Tgfpm.expr =
  let expr_node = 
    match e.expr_node with
      Tgfpm.Eepsilon
    | Tgfpm.Eempty
    | Tgfpm.Estring _
    | Tgfpm.Eident _
    | Tgfpm.Eskip
    | Tgfpm.Etrue
    | Tgfpm.Efalse              -> e.expr_node
    | Tgfpm.Enilrecord          -> Tgfpm.Erecord []
    | Tgfpm.Eniltable           -> Tgfpm.Etable []
    | Tgfpm.Eselect (e1, e2)    -> Tgfpm.Eselect (unchainify e1, unchainify e2)
    | Tgfpm.Eproject (e', i)    -> Tgfpm.Eproject (unchainify e', i)
    | Tgfpm.Eblock e'           -> assert false
    | Tgfpm.Econcat (e1, e2)    -> Tgfpm.Econcat (unchainify e1, unchainify e2)
    | Tgfpm.Einterl (e1, e2)    -> Tgfpm.Einterl (unchainify e1, unchainify e2)
    | Tgfpm.Edisj (e1, e2)      -> Tgfpm.Edisj (unchainify e1, unchainify e2)
    | Tgfpm.Elock e'            -> Tgfpm.Elock (unchainify e')
    | Tgfpm.Elambda (i1, i2, e')-> Tgfpm.Elambda (i1, i2, unchainify e')
    | Tgfpm.Erecord _
    | Tgfpm.Etable _            -> assert false
    | Tgfpm.Eif (e1, e2, e3)    -> Tgfpm.Eif (unchainify e1, unchainify e2, unchainify e3)
    | Tgfpm.Eand (e1, e2)       -> Tgfpm.Eand (unchainify e1, unchainify e2)
    | Tgfpm.Eor (e1, e2)        -> Tgfpm.Eor (unchainify e1, unchainify e2)
    | Tgfpm.Enot e'             -> Tgfpm.Enot (unchainify e')
    | Tgfpm.Econcrecord ((i,e1),e2)
                                -> unchainify_record ((i,e1),e2)
    | Tgfpm.Econctable ((i,e1),e2)
                                -> unchainify_table ((i,e1),e2)
  in { e with expr_node }

and unchainify_record ((i, e1), e2) =
  let u_e1 = unchainify e1 and u_e2 = unchainify e2 in
  match u_e2.expr_node with
    Tgfpm.Erecord r -> Tgfpm.Erecord ((i, u_e1)::r)
  | _               -> assert false

and unchainify_table ((i, e1), e2) =
  let u_e1 = unchainify e1 and u_e2 = unchainify e2 in
  match u_e2.expr_node with
    Tgfpm.Etable r -> Tgfpm.Etable ((i, u_e1)::r)
  | _              -> assert false

